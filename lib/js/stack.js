/* Generalised Stack Inspection runtime */

let _callcount = 0;
let _breakat   = 100;

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

/* Continuations */
function _absurd(f) {
    try {
        return f();
    } catch (e) {
        throw e;
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

    get handler() { return this._continuation.newFrames.head; }

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

/** Continuations **/
class ZipperContinuation {
    get newFrames() { return this._newFrames; }
    set newFrames(fs) { this._newFrames = fs; }

    get oldFrames() { return this._oldFrames; }
    set oldFrames(fs) { this._oldFrames = fs; }

    constructor(newFrames = null, oldFrames = null) {
        this._newFrames = newFrames;
        this._oldFrames = oldFrames;
    }

    assemble() {
        return FrameList.concat(FrameList.reverse(this._newFrames), this._oldFrames);
    }

    append(oldFrames) {
        this._oldFrames = FrameList.concat(this._oldFrames, oldFrames);
    }

    augment(frame) {
        throw "error";
    }

    compose(other) {
        this._newFrames = FrameList.concat(other.newFrames, this._newFrames);
        this.append(other.oldFrames);
    }

    invoke(_x) {
        throw "ZipperContinuation: method `invoke' must be implemented by any subclass";
    }
}
ZipperContinuation.prototype._newFrames = null;
ZipperContinuation.prototype._oldFrames = null;

class PureContinuation extends ZipperContinuation {

    constructor(newFrames = null, oldFrames = null) {
        super(newFrames, oldFrames);
    }

    invoke(x) {
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
                //frames = FrameList.reverse(frames.tail);
                e.handler.pureContinuation.append(frames.tail);
                throw e;
            } else {
                throw e;
            }
        }
        return result;
    }

    augment(frame) {
        this._newFrames = FrameList.cons(frame, this._newFrames);
        return;
    }
}

class Continuation extends ZipperContinuation {

    constructor(newFrames = FrameList.singleton(new AbstractHandleFrame()), oldFrames = null) {
        super(newFrames, oldFrames);
    }

    invoke(x) {
        let handlers = this.assemble();
        let result = x;
        let exn = new Error();
        let discontinue = false;

        while (handlers !== null) {
            try {
                if (discontinue) {
                    result = handlers.head.discontinue(exn);
                    handlers = handlers.tail;
                    discontinue = false;
                } else {
                    result = handlers.head.invoke(result);
                    handlers = handlers.tail;
                }
            } catch (e) {
                if (e instanceof PerformOperationError) {
                    if (discontinue) {
                        // Merge back in the original pure continuation
                        e.handler.pureContinuation.compose(handlers.head.pureContinuation);
                    }
                    // discontinue handlers.tail with e
                    if (handlers.tail === null) { // edge case: no more handlers to search
                        throw e;
                    } else {
                        discontinue = true;
                        handlers = handlers.tail;
                        exn = e;
                    }
                } else if (e instanceof SaveContinuationError) {
                    if (discontinue) {
                        // Merge e.handler with handlers.head
                        e.handler.pureContinuation.compose(handlers.head.pureContinuation);
                    }
                    // merge with handlers.tail
                    e.continuation.append(handlers.tail);
                    throw e;
                } else {
                    throw e;
                }
            }
        }

        return result;
    }

    instantiateTrapPoint(handleFn) {
        const head = this.newFrames.head.instantiate(handleFn);
        this.newFrames = FrameList.cons(head, this.newFrames.tail);
        return;
    }

    copy() {
        return new Continuation(this.newFrames, this.oldFrames);
    }

    setAbstractTrapPoint() {
        this.newFrames = FrameList.cons(new AbstractHandleFrame(), this.newFrames);
        return;
    }

    static CWCC(/* ContinuationReceiver */ receiver) { /* generic object */
        try {
            throw new SaveContinuationError(); // begin unwind
        } catch (e) {
            if (e instanceof SaveContinuationError) {
                e.handler.augment(new CWCC_frame0(receiver));
                throw e;
            } else {
                throw e;
            }
        }

        return null; // should never be reached
    }

    static establishInitialContinuation(/* Thunk */ _thunk) { /* generic object */
        let thunk = _thunk;
        while (true) {
            try {
                return Continuation.initialContinuationAux(thunk);
            } catch (e) {
                if (e instanceof WithinInitialContinuationError) {
                    thunk = e.thunk;
                } else {
                    throw e;
                }
            }
        }
        return;
    }

    static initialContinuationAux(/* Thunk */ thunk) {
        try {
            return thunk();
        } catch (e) {
            if (e instanceof PerformOperationError) {
                _print("Unhandled operation: " + e.op._label);
                throw e;
            } else if (e instanceof SaveContinuationError) {
                e.continuation.instantiateTrapPoint(_absurd);
                const k = e.continuation;
                throw new WithinInitialContinuationError(function() {
                    return k.invoke(undefined); // argument is unused
                });
            } else {
                throw e;
            }
        }
        return;
    }

    augment(frame) {
        this.newFrames.head.augment(frame);
        return;
    }
}

/** Frames **/
class HandleFrame {
    get pureContinuation() { return this._pureCont; }
    set pureContinuation(pureCont) { this._pureCont = pureCont; }

    constructor(pureContinuation = null) {
        if (pureContinuation === null)
            this._pureCont = new PureContinuation();
        else
            this._pureCont = pureContinuation;
    }

    invoke(_x) {
        throw "error: Cannot invoke a raw handle frame.";
    }

    augment(frame) {
        this._pureCont.augment(frame);
    }
}
HandleFrame.prototype._pureCont = null;

class AbstractHandleFrame extends HandleFrame {

    constructor() {
        super();
    }

    instantiate(handleFn) {
        return new GenericHandleFrame(handleFn, this.pureContinuation);
    }

    invoke(_x) {
        throw "error: Cannot invoke an abstract handler.";
    }
}

class GenericHandleFrame extends HandleFrame {

    get handleFn() { return this._handleFn; }
    set handleFn(f) { this._handleFn = f; }

    constructor(handle, pureCont) {
        super(pureCont);
        this._handleFn = handle;
    }

    discontinue(/* PerformOperationError */ exn) {
        return this._handleFn.call(null, function() { throw exn; });
    }

    invoke(x) {
        const pureCont = this.pureContinuation;
        return this._handleFn.call(null, function() { return pureCont.invoke(x); });
    }

    // extend(pureCont) {
    //     this._pureCont.compose(pureCont);
    // }

    // static splice(handleFrame0, handleFrame1) {
    //     return new GenericHandleFrame(handleFrame0.handleFn, FrameList.concat(handleFrame0.frames, handleFrame1.frames));
    // }
}
GenericHandleFrame.prototype._handleFn = function(_f) { throw "error: No handle provided"; };

class GenericPureContinuationFrame {
    get frameFn() { return this._frameFn; }
    set frameFn(f) { this._frameFn = f; }
    get args() { return this._args; }
    set args(xs) { this._args = xs; }

    constructor(frameFn, ...args) {
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

class CWCC_frame0 extends GenericPureContinuationFrame {
    get receiver() {
        return this._reciever;
    }

    constructor(/* ContinuationReceiver */ receiver) {
        super();
        this._receiver = receiver;
    }

    invoke(/* generic object */ resumeValue) {
        return this._receiver(resumeValue);
    }
}
// Initialise public members
CWCC_frame0.prototype._receiver = null;

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
            exn.continuation.instantiateTrapPoint(handleFn);
            const k = exn.continuation.copy();
            //console.log("noop");
            exn.continuation.setAbstractTrapPoint();
            return function(x) {
                return k.invoke(x);
            };
        }
    };
})();
