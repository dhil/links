class ContinuationFrame {
    get continuation() { /* type FrameList */
        return this._continuation;
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
         * brcause we have not made nay progress at this point. When
         * we `apply' the continuation function, we need to establish
         * a try/catch in order to tie this continuation in to the new
         * frames about to be created.
         */
        const value =
              framesAbove === null
              ? resumeValue
              : frameAbove.first.reload(frameAbove.rest, resumeValue);

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
        this._new_frames = new FrameList(extenstion, this.newFrames());
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
            this._receiver = receiver;
        }

        apply(/* generic objcet */ resumeValue) {
            return this._receiver.apply(resumeValue);
        }
    }
    // Initialise public members
    CWCC_frame0.prototype._receiver = null;

    class WithinInitialContinuationError extends Error {
        get thunk() {
            return this._thunk;
        }

        constructor(/* Thunk */ thunk) {
            this._thunk = thunk;
        }
    }
    // Initialise public members
    WithinInitialContinuationError.prototype._thunk = null;

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
                fs = new FrameList(new_frame, frames);
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

        static beginUnwind() {
            throw new SaveContination();
        }

        static CWCC(/* ContinuationReceiver */ receiver) { /* generic object */
            try {
                beginUnwind();
            } catch (e) {
                if (e instanceof SaveContinuationError) {
                    e.Extend(new CWCC_frame0(receiver));
                    throw e;
                } else {
                    throw e;
                }
            }

            return null; // should never be reached
        }

        static establishInitialContinuation(/* Thunk */ thunk) { /* generic object */
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

// Example program

const Tak = (function() {
    // Main class static variables
    let _callcount = 0;
    let _breakat   = -1;
    let _breakevery = -1;

    // Main class static functions

    function _takPause() {
        print("takPause at " + callcount);
        breakat = callcount + breakevery;
        print("next pause at " + breakat);
        return Continuation.CWCC(function(/* Continuation */ cont) {
            print("takPause cont is " + cont);
            return null;
        });
    }

    function _takAn(/* int */ x, /* int */ y, /* int */ z) {
        _callcount++;
        if (_callcount === _breakat) {
            try {
                _takPause();
            } catch (e) {
                if (e instanceof SaveContinuationError) {
                    e.extend(new TakFrame0(x, y, z));
                    throw e;
                } else
                    throw e;
            }
        }
        return _takAn0(x, y, z);
    }

    function _takAn0(/* int */ x, /* int */ y, /* int */ z) {
        if (!(x < y))
            return z;
        let temp0 = 0;
        try {
            temp0 = _takAn(x - 1, y, z);
        } catch (e) {
            if (e instanceof SaveContinuationError) {
                e.extend(new TakFrame1(x,y,z));
                throw e;
            } else
                throw e;
        }
        return _takAn1(temp0, x, y, z);
    }

    function _takAn1(/*int */ temp0, /* int */ x, /* int */ y, /* int */ z) {
        let temp1 = 0;
        try {
            temp1 = _takAn(y - 1, z, x);
        } catch (e) {
            if (e instanceof SaveContinuationError) {
                e.extend(new TakFrame2(temp0, x, y, z));
                throw e;
            } else
                throw e;
        }
        return _takAn2(temp1, temp0, x, y, z);
    }

    function _takAn2(/* int */ temp1, /*int */ temp0, /* int */ x, /* int */ y, /* int */ z) {
        let temp2 = 0;
        try {
            temp2 = _takAn(z - 1, x, y);
        } catch (e) {
            if (e instanceof SaveContinuationError) {
                e.extend(new TakFrame3(temp1, temp0));
                throw e;
            } else
                throw e;
        }
        return _takAn3(temp2, temp1, temp0);
    }

    function _takAn3(/* int */ temp2, /* int */ temp1, /*int */ temp0) {
        // No try/catch needed here
        return _takAn(temp0, temp1, temp2);
    }

    function _takgo(x, y, z) {
        return Continuation.establishInitialContinuation(function() {
            return _takAn(x, y, z);
        });
    }

    // Nested (frame) classes
    class TakFrame0 extends ContinuationFrame {
        get x() { return this._x; }
        get y() { return this._y; }
        get z() { return this._z; }

        constructor(/* int */ x, /* int */ y, /* int */ z) {
            this._x = x;
            this._y = y;
            this._z = z;
        }

        apply(/* generic object */ _resumeValue) { /* generic object */
            return _takAn0(this.x, this.y, this.z);
        }
    }

    class TakFrame1 extends ContinuationFrame {
        get x() { return this._x; }
        get y() { return this._y; }
        get z() { return this._z; }

        constructor(/* int */ x, /* int */ y, /* int */ z) {
            this._x = x;
            this._y = y;
            this._z = z;
        }

        apply(/* generic object */ _resumeValue) { /* generic object */
            return _takAn1(this.x, this.y, this.z);
        }
    }

    class TakFrame2 extends ContinuationFrame {
        get temp0() { return this._temp0; }
        get x() { return this._x; }
        get y() { return this._y; }
        get z() { return this._z; }

        constructor(/* int */ temp0, /* int */ x, /* int */ y, /* int */ z) {
            this._temp0 = temp0;
            this._x = x;
            this._y = y;
            this._z = z;
        }

        apply(/* generic object */ resumeValue) { /* generic object */
            return _takAn2(resumeValue, this.temp0, this.x, this.y, this.z);
        }
    }

    class TakFrame3 extends ContinuationFrame {
        get temp1() { return this._temp1; }
        get temp0() { return this._temp0; }

        constructor(/* int */ temp1, /* int */ temp0) {
            this._temp1 = temp1;
            this._temp0 = temp0;
        }

        apply(/* generic object */ resumeValue) { /* generic object */
            return _takAn3(resumeValue, this.temp1, this.temp0);
        }
    }

    return class {
        static get callcount() { return _callcount; }
        static set callcount(n) { _callcount = n; }
        static get breakat() { return _breakat; }
        static set breakat(n) { _breakat = n; }
        static get breakevery() { return _breakevery; }
        static set breakevery(n) { _breakevery = n; }

        static takAn0(x,y,z) { return _takAn0(x,y,z); }
        static takAn1(x,y,z) { return _takAn1(x,y,z); }
        static takPause() { return _takPause(); }
        static taktest() {
            print("tak (18, 12, 6) = " + _takgo(18, 12, 6));
        }
    };
}());

Tak.taktest();
