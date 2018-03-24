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
class ContinuationFrame {
    get continuation() { /* type FrameList */
        return this._continuation;
    }

    set continuation(cont) {
        this._continuation = cont;
    }

    /*
     * Each frame invokes the loader for the frame above it, thereby
     * effectively reconstructing the stack. The topmost frame gets
     * the restart value passed into its continuation.
     */
    reload(/* FrameList */ framesAbove, /* generic object */ resumeValue) {
        /*
         * The continuation for the call to `reload' is the expression
         * continuation of this assignment followed by the subsequent
         * try/catch block.
         *
         * This call does _not_ have to be protected by a try/catch
         * because we have not made nay progress at this point. When
         * we `apply' the continuation function, we need to establish
         * a try/catch in order to tie this continuation in to the new
         * frames about to be created.
         */
        const value =
              framesAbove === null
              ? resumeValue
              : framesAbove.first.reload(framesAbove.rest, resumeValue);

        try {
            return this.apply(value);
        } catch(e) {
            if (e instanceof SaveContinuationError) {
                e.append(this.continuation);
                throw e;
            } else {
                throw e;
            }
        }
    }

    /* Must be implemented by any subclass */
    apply(returnValue) {
        throw "abstract function invocation error: `apply'.";
    }
}
// Initialise public fields
ContinuationFrame.prototype._continuation = null;

class FrameList {
    get first() { /* type ContinuationFrame */
        return this._first;
    }

    get rest() { /* type FrameList */
        return this._rest;
    }

    constructor(/* ContinuationFrame */ first, /* FrameList */ rest) {
        this._first = first;
        this._rest = rest;
    }

   static  reverse(/* FrameList */ fs) { /* type FrameList */
        let result = null; /* type FrameList */
        while (fs !== null) {
            result = new FrameList(fs.first, result);
            fs = fs.rest;
        }
        return result;
    }
}
// Initialise public fields
FrameList.prototype._first = null;
FrameList.prototype._rest = null;

class SaveContinuationError extends Error {
    /*
     * A list of frames that have not yet been fully assembled into
     * the continuation. Each frame has saved its own state.
     */
    get newFrames() {
        return this._new_frames;
    }

    /*
     * A list of frames that already have been assembled into the
     * continuation. When unwinding the stack we do not need to unwind
     * these frames.
     */
    get oldFrames() {
        return this._old_frames;
    }

    /*
     * Push a newly created heap frame onto the list of frames that
     * need to be assembled into the continuation. This should be done
     * in the exception handler that protects the initial subroutine
     * call.
     */
    extend(/* ContinuationFrame */ extension) { /* unit */
        this._new_frames = new FrameList(extension, this.newFrames);
        return;
    }

    /*
     * Append the tail of the current continuation to the exception
     * oject so that the (exception) handler can assemble the new
     * frames onto it.
     */
    append(/* FrameList */ oldFrames) { /* unit */
        this._old_frames = oldFrames;
        return;
    }

    toContinuation() { /* Continuation */
        return new Continuation(this._new_frames, this._old_frames);
    }

    constructor(...args) {
        super(...args);
    }
}
// Initialise public members
SaveContinuationError.prototype._new_frames = null;
SaveContinuationError.prototype._old_frames = null;

const Continuation = (function() {
    // Nested classes
    class CWCC_frame0 extends ContinuationFrame {
        get receiver() {
            return this._reciever;
        }

        constructor(/* ContinuationReceiver */ receiver) {
            super();
            this._receiver = receiver;
        }

        apply(/* generic object */ resumeValue) {
            return this._receiver.apply(resumeValue);
        }
    }
    // Initialise public members
    CWCC_frame0.prototype._receiver = null;

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

    function beginUnwind() {
            throw new SaveContinuationError();
    }

    return class {
       /*
        * Holds the list of frames in order from most recent to least
        * recent. This is the direction we want for sharing, but the
        * reverse direction for loading.
        */
        get frames() { /* FrameList */
            return this._frames;
        }

        constructor(/* FrameList */ newFrames, /* FrameList */ oldFrames) {
           /*
            * The new frames do not know what the continuation is below
            * them. We take them one by one and push them onto the old
            * frames while setting their continuation to the list of
            * frames below.
            */
            let new_frames  = newFrames; /* FrameFrame */
            let frames = oldFrames; /* FrameList */
            while (new_frames !== null) {
                const new_frame = new_frames.first; /* ContinuationFrame */
                new_frames = new_frames.rest;
                if (new_frame.continuation !== null)
                    throw "Continuation not empty?";
                new_frame.continuation = frames;
                frames = new FrameList(new_frame, frames);
            }
            this._frames = frames;
        }

        reload(/* generic object */ resumeValue) { /* generic object */
            /*
             * Reverse the frames in order to reload them.
             */
            const rev = FrameList.reverse(this._frames);
            return rev.first.reload(rev.rest, resumeValue);
        }

        static CWCC(/* ContinuationReceiver */ receiver) { /* generic object */
            try {
                beginUnwind();
            } catch (e) {
                if (e instanceof SaveContinuationError) {
                    e.extend(new CWCC_frame0(receiver));
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
                return thunk.apply();
            } catch (e) {
                if (e instanceof SaveContinuationError) {
                    const k = e.toContinuation(); /* Continuation */
                    throw new WithinInitialContinuationError(function() {
                        return k.reload(k);
                    });
                }
            }
            return;
        }
    }
}());

class GenericContinuationFrame extends ContinuationFrame {

    constructor(frameFn, ...args) {
        super();
        this.frameFn = frameFn;
        this.args = args;
    }

    /* Override */
    apply(/* generic object */ resumeValue) { /* generic object */
        const args = this.args;
        return this.frameFn.call(null, resumeValue, ...args);
    }

    /* Dummy frame */
    noContinuationFrameError() {
        throw "No continuation frame has been provided";
    }
}
GenericContinuationFrame.prototype.frameFn = GenericContinuationFrame.noContinuationFrameError;
GenericContinuationFrame.prototype.args = [];

class GenericInitialContinuationFrame extends GenericContinuationFrame {
    constructor(...args) {
        super(...args);
    }

    // Initial continuation frames are generated by the
    // trampoline. They are created before any let bindings have been
    // evaluated.
    /* Override */
    apply(_resumeValue) {
        return this.frameFn.apply(null, this.args);
    }
}
