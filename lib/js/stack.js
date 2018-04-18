let _callcount = 0;
let _breakat   = 500;

/* IO operations */
const _IO = (function() {
    return {
        'print': function(msg) {
            _print(msg);
            return {};
        },
        'error': function(msg) {
            throw msg;
        },
        'debug': function(msg) {
            print("[DEBUG] " + msg);
            return {};
        }
    }
}());

let _debug_i = 0;
function _debug_incr() { _debug_i += 4; }
function _debug_decr() { _debug_i -= 4; }

function _debug(msg) {
    let result = "";
    for (let j = 0; j < _debug_i; j++) result += " ";
    _IO.print(result + msg);
}

/* Continuations */

/** "Errors" **/
class WithinInitialContinuationError extends Error {
    get thunk() {
        return this._thunk;
    }

    constructor(/* Thunk */ thunk, ...args) {
        super(...args);
        this._thunk = thunk;
    }
}
// Initialise public members
WithinInitialContinuationError.prototype._thunk = null;

class SaveContinuationError extends Error {

    get continuation() { return this._continuation; }
    set continuation(cont) { this._continuation = cont; }

    get pureContinuation() { return this._continuation.newFrames.head.pureContinuation; }
    set pureContinuation(pureCont) { this._continuation.newFrames.head.pureContinuation = pureCont; }

    get handler() { return this._continuation.newFrames.head.handler; }

    constructor() {
        super();
        this._continuation = new Continuation();
    }
}
SaveContinuationError.prototype._continuation = null;

class PerformOperationError extends SaveContinuationError {

    constructor(op) {
        super();
        this.op = op;
    }
}
PerformOperationError.prototype.op = {"_label": "NoOp"}

class ResumeError extends SaveContinuationError {

    get resumption() { return this._resume; }
    get arg() { return this._arg; }

    constructor(resumption, arg) {
        super();
        this._resume = resumption;
        this._arg = arg;
    }
}
ResumeError.prototype._resumption = null;
ResumeError.prototype._arg = null;

/** Continuations **/
class ZipperContinuation {
    get newFrames() { return this._newFrames; }
    set newFrames(fs) { this._newFrames = fs; }

    get oldFrames() { return this._oldFrames; }
    set oldFrames(fs) { this._oldFrames = fs; }

    constructor(/* FrameList<T> */ newFrames = null, /* FrameList<T> */ oldFrames = null) {
        this._newFrames = newFrames;
        this._oldFrames = oldFrames;
    }

    assemble() { /* FrameList<T> */
        return FrameList.concat(FrameList.reverse(this._newFrames), this._oldFrames);
    }

    append(/* FrameList<T> */ oldFrames) {
        this._oldFrames = FrameList.concat(this._oldFrames, oldFrames);
    }

    augment(/* T */ frame) {
        throw "error";
    }

    compose(/* ZipperContinuation<T> */ other) {
        this._newFrames = FrameList.concat(other.newFrames, this._newFrames);
        this.append(other.oldFrames);
    }

    invoke(/* Value */ _x) {
        throw "ZipperContinuation: method `invoke' must be implemented by any subclass";
    }

    isEmpty() {
        return this._newFrames === null && this._oldFrames === null;
    }
}
ZipperContinuation.prototype._newFrames = null;
ZipperContinuation.prototype._oldFrames = null;

class ContinuationFrame {
    get handler() { return this._handler; }
    set handler(h) { this._handler = h; }

    get pureContinuation() { return this._pureCont; }
    set pureContinuation(cont) { this._pureCont = cont; }

    constructor(/* HandleFrame */ handler = new AbstractHandleFrame(), /* PureContinuation */ pureCont = new PureContinuation()) {
        this._handler = handler;
        this._pureCont = pureCont;
    }

    augment(/* PureContinuationFrame */ frame) {
        this._pureCont.augment(frame);
    }

    instantiate(/* HandleFrame */ handleFrame) {
        this._handler = this.handler.instantiate(handleFrame);
    }
}
ContinuationFrame.prototype._handler = null;
ContinuationFrame.prototype._pureCont = null;

class PureContinuation extends ZipperContinuation {

    constructor(/* FrameList<PureContinuationFrame> */ newFrames = null, /* FrameList<PureContinuationFrame> */ oldFrames = null) {
        super(newFrames, oldFrames);
    }

    invoke(/* Value */ x) {
        let frames = this.assemble();
        let result = x;
        try {
            while (frames !== null) {
                //console.log("FrameList.length = " + FrameList.length(frames));
                result = frames.head.invoke(result);
                frames = frames.tail;
            }
        } catch (e) {
            if (e instanceof SaveContinuationError) { /* PerformOperationError is a subtype of SaveContinuationError */
                e.pureContinuation.append(frames.tail);
                throw e;
            } else {
                throw e;
            }
        }
        return result;
    }

    augment(/* PureContinuationFrame */ frame) {
        this._newFrames = FrameList.cons(frame, this._newFrames);
        return;
    }

    copy() {
        const copy_frames = function(fs) { return FrameList.reverse(FrameList.reverse(fs)); }
        return new PureContinuation(copy_frames(this._newFrames, this._oldFrames));
    }
}

// class Resumption {
    
//     constructor(/* Continuation */ cont) {
//     }

//     assemble() {
//     }
// }

class HandleError extends Error {
    get error() { return this._err; }
    set error(err) { return this._err; }

    constructor(/* SaveContinuationError */ err) {
        super();
        this._err = err;
    }
}

class InitialHandleError extends HandleError {
    constructor(/* SaveContinuationnError */ err, /* FrameList<PureContinuationFrame> */ oldFrames) {
        super(err);
        this.error.pureContinuation.append(oldFrames);
    }
}

class Continuation extends ZipperContinuation {

    constructor(/* FrameList<ContinuationFrame> */ newFrames = FrameList.singleton(new ContinuationFrame()), /* FrameList<ContinuationFrame> */ oldFrames = null) {
        super(newFrames, oldFrames);
    }

    invoke(/* Value */ x) {
        let frames = this.assemble();
        let result = x;
        let exn = new Error();
        let discontinue = false;
        let cur = null;
        while (frames !== null) {
            cur = frames.head;
            let handler = cur.handler;
            //_print(cur.handler.toString());
            //_print(JSON.stringify(cur));
            try {
                if (!discontinue) {
                    // Begin evaluation of the pure continuation
                    let pureFrames = cur.pureContinuation.assemble();
                    try {
                        while (pureFrames !== null) {
                            let frame = pureFrames.head;
                            //_print(JSON.stringify(frame));
                            result = frame.invoke(result);
                            pureFrames = pureFrames.tail;
                        }
                    } catch (e) {
                        if (e instanceof PerformOperationError) {
                            // Operation performed inside of `frame'
                            // Augment the pure continuation with the unevaluated frames
                            throw new InitialHandleError(e, pureFrames.tail);
                        } else if (e instanceof SaveContinuationError) {
                            // Trampoline bounce initiated during evaluation `frame'
                            // Augment the pure continuation with the unevaluated frames
                            throw new InitialHandleError(e, pureFrames.tail);
                        } else {
                            throw e;
                        }
                    }
                    // Invoke return clause
                    try {
                        result = handler.valueFrame.call(null, result);
                    } catch (e) {
                        throw "error while evaluating return clause"; // TODO
                    }
                    frames = frames.tail;
                } else {
                    // Handling mode
                    try {
                        result = handler.effectFrame.call(null, exn);
                    } catch (e) {
                        if (e instanceof SaveContinuationError) {
                            throw new HandleError(e);
                        } else {
                            throw e;
                        }
                    }
                    frames = frames.tail;
                    discontinue = false;
                }
            } catch (e) {
                if (e instanceof InitialHandleError) {
                    // Prepare invocation of the current handler
                    exn = e.error;
                    discontinue = true;
                    continue;
                } else if (e instanceof HandleError) {
                    // Prepare invocation of the next handler
                    exn = e.error;
                    if (frames === null || frames.tail === null) throw exn;
                    frames = frames.tail;
                    exn.pureContinuation.compose(frames.head.pureContinuation);
                    discontinue = true;
                    continue;
                } else {
                    throw e;
                }
            }
        }
        //_print("Result: " + JSON.stringify(result));
        return result;
    }

    instantiateTrapPoint(/* HandleFrame */ handleFrame) {
        this.newFrames.head.instantiate(handleFrame);
        return;
    }

    setAbstractTrapPoint() {
        this.newFrames = FrameList.cons(new ContinuationFrame(), this.newFrames);
        return;
    }

    static establishInitialContinuation(/* Thunk */ _thunk) { /* generic object */
        _callcount = 0;
        let i = 0;
        let thunk = function() { return AbsurdHandleFrame.initiate(_thunk); };
        while (true) {
            try {
                return thunk();
            } catch (e) {
                if (e instanceof ResumeError) {
                    //_callcount = 0;
                    // _print("Resuming");
                    const resumption = e.resumption.copy();
                    resumption.compose(e.continuation);
                    const k = resumption;
                    const arg = e.arg;
                    thunk = function() { return k.invoke(arg); };
                } else if (e instanceof PerformOperationError) {
                    _print("Unhandled operation: " + e.op._label);
                    throw e;
                } else if (e instanceof SaveContinuationError) {
                    //_debug("Bounce");
                    const k = e.continuation;
                    thunk = function() { return k.invoke(undefined); }; // the argument to `invoke' is unused
                } else {
                    throw e;
                }
            }
        }
        return; // should never be reached
    }

    augment(frame) {
        this.newFrames.head.augment(frame);
        return;
    }

    copy() {
        const copy_frames = function(fs) { return FrameList.reverse(FrameList.reverse(fs)); }
        return new Continuation(copy_frames(this._newFrames, this._oldFrames));
    }
}

/** Frames **/
class HandleFrame {
    constructor() { }

    install(_x) {
        throw "error: the `install' method must be implemented by any subclass.";
    }

    toString() {
        return "HandleFrame";
    }
}

class AbstractHandleFrame extends HandleFrame {

    constructor() {
        super();
    }

    instantiate(handleFrame) {
        return handleFrame;
    }

    install(_x) {
        throw "error: Cannot install an abstract handler.";
    }

    toString() {
        return "AbstractHandleFrame";
    }
}

class GenericHandleFrame extends HandleFrame {

    get valueFrame() { return this._val; }
    get effectFrame() { return this._eff; }

    constructor(val, eff) {
        super();
        if (val === undefined) throw "VAL IS UNDEFINED";
        this._val = val;
        this._eff = eff;
    }

    toString() {
        return "GenericHandleFrame(" + this._eff.name + ")";
    }
}
GenericHandleFrame.prototype._val = null;
GenericHandleFrame.prototype._eff = null;

class AbsurdHandleFrame extends GenericHandleFrame {
    static value(x) {
        return x;
    }

    static effect(exn) {
        try {
            throw exn;
        } catch (e) {
            if (e instanceof PerformOperationError) {
                throw "Unhandled operation " + e.op._label;
            } else if (e instanceof SaveContinuationError) {
                e.continuation.instantiateTrapPoint(new AbsurdHandleFrame());
                throw e;
            } else {
                throw e;
            }
        }
    }

    static initiate(f) {
        try {
            return f();
        } catch (e) {
            return AbsurdHandleFrame.effect(e);
        }
    }

    constructor() {
        super(AbsurdHandleFrame.value, AbsurdHandleFrame.effect);
    }

    toString() {
        return "AbsurdHandleFrame";
    }
}

class PureContinuationFrame {
    constructor() { }
    invoke(_x) {
        throw "error: abstract method `invoke' must be implemented by any subclass.";
    }
}

class GenericPureContinuationFrame extends PureContinuationFrame {
    get frameFn() { return this._frameFn; }
    set frameFn(f) { this._frameFn = f; }
    get args() { return this._args; }
    set args(xs) { this._args = xs; }

    constructor(frameFn, ...args) {
        super();
        this._frameFn = frameFn;
        this._args = args;
    }

    invoke(x) {
        const args = this._args;
        return this._frameFn.call(null, x, ...args);
    }
}
GenericPureContinuationFrame.prototype._frameFn = function(...args) { throw "error: No frame provided."; }
GenericPureContinuationFrame.prototype._args = [];

class GenericInitialPureContinuationFrame extends GenericPureContinuationFrame {
    constructor(frameFn, ...args) {
        super(frameFn, ...args);
    }

    invoke(_x) {
        return this.frameFn.apply(null, this.args);
    }
}

/** List **/
class FrameList {

    get head() { return this._head; }
    set head(f) { this._head = f; }

    get tail() { return this._tail; }
    set tail(fs) { this._tail = fs; }

    static nil() { return null; }
    static cons(frame, frames) {
        return new FrameList(frame, frames);
    }
    static singleton(frame) {
        return FrameList.cons(frame, FrameList.nil());
    }

    static reverse(frames) {
        let result = FrameList.nil();
        let ptr = frames;
        while (ptr !== FrameList.nil()) {
            result = FrameList.cons(ptr.head, result);
            ptr = ptr.tail;
        }
        return result;
    }

    static concat(first, rest) {
        if (first === FrameList.nil()) return rest;
        else if (rest === FrameList.nil()) return first;
        else {
            let result = rest;
            let ptr = FrameList.reverse(first);
            while (ptr !== FrameList.nil()) {
                result = FrameList.cons(ptr.head, result);
                ptr = ptr.tail;
            }
            return result;
        }
    }

    static length(frames) {
        let i = 0;
        while (frames !== null) {
            i++;
            frames = frames.tail;
        }
        return i;
    }

    static last(frames) {
        if (frames === FrameList.nil()) throw "error: `last' invocation on empty list.";
        let ptr = frames;
        while (frames.tail !== FrameList.nil()) ptr = frames.tail;
        return ptr;
    }

    constructor(head, tail) {
        this._head = head;
        this._tail = tail;
    }

    append(fs) {
        let result = rest;
        let ptr = tail;
        while (ptr !== FrameList.nil()) {
            result = FrameList.cons(ptr.head, result);
            ptr = ptr.tail;
        }
        return result;
    }
}
FrameList.prototype._head = null;
FrameList.prototype._tail = null;

const _Resumption = (function() {
    return {
        'makeDeep': function(exn, val, eff) {
            exn.continuation.instantiateTrapPoint(new GenericHandleFrame(val, eff));
            const resumption = exn.continuation;
            //console.log("noop");
            exn.continuation = new Continuation();
            return function(x) {
                //_print("Resuming with " + JSON.stringify(x));
                //_debug("... invoking resumption(" + handleFn.name + ") with " + JSON.stringify(x));
                throw new ResumeError(resumption, x);
                // return resumption.invoke(x);
            };
        }
    };
})();
