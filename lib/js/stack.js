/* Generalised Stack Inspection runtime */

let _callcount = 0;
let _breakat   = 3000;

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
function _absurd(f) {
    try {
        //_print("Absurd");
        return f();
    } catch (e) {
        if (e instanceof PerformOperationError) {
            throw "Unhandled operation";
        } else if (e instanceof SaveContinuationError) {
            //_print("Pop absurd frame");
            e.continuation.instantiateTrapPoint(new GenericHandleFrame(_absurd));
            throw e;
        } else {
            throw e;
        }
    }
}
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

    get delimitedContinuation() { return this._delimCont; }
    get arg() { return this._arg; }

    constructor(delimitedContinuation, arg) {
        super();
        this._delimCont = delimitedContinuation;
        this._arg = arg;
    }
}
ResumeError.prototype._delimCont = null;
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
}

class Continuation extends ZipperContinuation {

    constructor(/* FrameList<ContinuationFrame> */ newFrames = FrameList.singleton(new ContinuationFrame()), /* FrameList<ContinuationFrame> */ oldFrames = null) {
        super(newFrames, oldFrames);
    }

    invoke(/* Value */ x) {
        let frames = this.assemble();
        let result = x;
        let exn = new Error();
        const INVOKE = 0, RESUME = 1, DISCONTINUE = 2;
        let mode = INVOKE;
        //_print("BEGIN invocation");
        while (frames !== null) {
            try {
                _debug("Installing " + frames.head.handler.toString());
                _debug_incr();
                switch (mode) {
                case INVOKE:
                    _debug("... continue with " + JSON.stringify(result));
                    const pureCont = frames.head.pureContinuation;
                    result = frames.head.handler.install(function() { return pureCont.invoke(result); })
                    break;
                case DISCONTINUE:
                    if (exn instanceof SaveContinuationError) {
                        _debug("... discontinue with SaveContinuationError");
                    } else if (exn instanceof PerformOperationError) {
                        _debug("... discontinue with PerformOperationError(" + JSON.stringify(exn.op) + ")");
                    } else {
                        _debug("... discontinue with unknown: " + JSON.stringify(exn));
                    }
                    result = frames.head.handler.install(function() { throw exn; });
                    mode = DISCONTINUE;
                    break;
                default:
                    throw "Unknown mode " + mode;
                }
                _debug_decr();
                _debug("Uninstalled " + frames.head.handler.toString());
                frames = frames.tail;
            } catch (e) {
                // if (e instanceof ResumeError) {
                //     _debug_decr();
                //     _debug("Uninstalled " + frames.head.handler.toString() + " due to ResumeError");

                //     if (mode === DISCONTINUE) {
                //         // Merge back in the original pure continuation
                //         e.pureContinuation.compose(frames.head.pureContinuation);
                //     }
                //     e.delimitedContinuation.compose(e.continuation);
                //     frames = e.delimitedContinuation.assemble();
                //     result = e.arg;
                //     mode = INVOKE;
                // } else
                if (e instanceof PerformOperationError) {
                    _debug_decr();
                    _debug("Uninstalled " + frames.head.handler.toString() + " due to operation invocation " + JSON.stringify(e.op));
                    if (mode === DISCONTINUE) {
                        // Merge back in the original pure continuation
                        e.pureContinuation.compose(frames.head.pureContinuation);
                    }
                    // discontinue handlers.tail with e
                    if (frames.tail === null) { // edge case: no more handlers to search
                        throw e;
                    } else {
                        mode = DISCONTINUE;
                        frames = frames.tail;
                        exn = e;
                    }
                } else if (e instanceof SaveContinuationError) {
                    _debug_decr();
                    _debug("Uninstalled " + frames.head.handler.toString() + " due to SaveContinuationError");
                    if (mode === DISCONTINUE) {
                        // Merge e.handler with handlers.head
                        // _print("Pure: " + JSON.stringify(e.pureContinuation));
                        e.pureContinuation.compose(frames.head.pureContinuation);
                    }
                    // merge with frames.tail
                    if (frames.tail === null) {
                        throw e;
                    } else {
                        mode = DISCONTINUE;
                        frames = frames.tail;
                        exn = e;
                    }
                    // e.continuation.append(frames.tail);
                    // throw e;
                } else {
                    throw e;
                }
            }
        }

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
        let thunk = function() { return _absurd(_thunk); };
        while (true) {
            try {
                return thunk();
            } catch (e) {
                if (e instanceof PerformOperationError) {
                    _print("Unhandled operation: " + e.op._label);
                    throw e;
                // } else if (e instanceof ResumeError) {
                //     _debug("Resume at top level");
                //     _print(JSON.stringify(e.delimitedContinuation));
                //     e.delimitedContinuation.compose(e.continuation);
                //     const k = e.delimitedContinuation;
                //     const arg = e.arg;
                //     thunk = function() { return k.invoke(arg); }; // the argument to `invoke' is unused
                } else if (e instanceof SaveContinuationError) {
                    _debug("Bounce");
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

    get handleFn() { return this._handleFn; }
    set handleFn(f) { this._handleFn = f; }

    constructor(handle) {
        super();
        this._handleFn = handle;
    }

    install(f) {
        return this._handleFn.call(null, f);
    }

    toString() {
        return "GenericHandleFrame(" + this._handleFn.name + ")";
    }
}
GenericHandleFrame.prototype._handleFn = function(_f) { throw "error: No handle provided"; };

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
        'makeDeep': function(exn, handleFn) {
            exn.continuation.instantiateTrapPoint(new GenericHandleFrame(handleFn));
            const continuation = exn.continuation;
            //console.log("noop");
            exn.continuation = new Continuation();
            return function(x) {
                _debug("... invoking resumption(" + handleFn.name + ") with " + JSON.stringify(x));
                return continuation.invoke(x);
                // throw new ResumeError(continuation, x);
            };
        }
    };
})();
