/* Program generated... */
'use strict';


/* [Begin] Include /home/dhil/projects/links/my-links/lib/js/base.js */
/* Basic IO */
const _print = (function(that) {
    if (that.hasOwnProperty("console"))
        return console.hasOwnProperty("log") ? console.log : print; // HACK
    else
        return print;
})(this);

/* Conversion functions */
const _String = (function() {
    return {
        'ofNumber': function(n) { return n.toString(); },
        'concat': function(base, suffix) {
            return base.concat(suffix);
        }
    };
}());

const _Number = (function() {
    return {
        'toFloat': function(n) { return n; },
        'toInt': function(n) { return Math.floor(n); },
        'integerDivision': function(n, m) { return Math.floor(n / m); }
    }
}());

/* List operations */
const _List = (function() {
    const nil = null;
    function cons(x, xs) {
        return {'_head': x, '_tail': xs};
    }

    function head(xs) { return xs['_head']; }
    function tail(xs) { return xs['_tail']; }
    function singleton(x) { return cons(x, nil) };
    function length(xs) {
        let acc = 0;
        let ys = xs;
        while (ys !== nil) {
            acc = acc + 1;
            ys = tail(ys);
        }
        return acc;
    }
    return {
        'nil': nil,
        'cons': cons,
        'head': head,
        'tail': tail,
        'length': length,
        'singleton': singleton,
        'revAppend': function(xs, ys) {
            var out = ys;
            while (xs !== nil) {
                out = cons(head(xs), out);
                xs = tail(xs);
            }
            return out;
        },
        'concat': function(xs, ys) {
            let ws = ys;
            let vs = nil;
            let zs = xs;
            // Copy xs into vs
            while (zs !== nil) {
                vs = cons(head(zs), vs);
                zs = tail(zs);
            }
            // Copy vs onto ys (safe to share ys between (xs ++ ys) and ys)
            while (vs !== nil) {
                ws = cons(head(vs), ws);
                vs = tail(vs);
            }
            return ws;
        }
    }
}());

/* Records */
const _Record = (function() {
    return {
        'union': function(r, s) {
            return Object.assign({}, r, s);
        },
        'erase': function(r, labels) {
            let s = {};
            let ls = new Set(labels);
            for (let l in r) {
                if (ls.has(l)) continue;
                else s[l] = r[l]
            }
            return s;
        }
    };
})();

/* Closure operations */
const _Closure = (function() {
    return {
        'apply': function(f, x) {
            return function() {
                return f.apply(this, [x].concat(Array.prototype.slice.call(arguments)));
            }
        },
        '_yield': function(f, x) {
            return function*() {
                return yield* f.apply(this, [x].concat(Array.prototype.slice.call(arguments)));
            }
        }
    };
}());
/* [End] Include /home/dhil/projects/links/my-links/lib/js/base.js */
/* [Begin] Include /home/dhil/projects/links/my-links/lib/js/array.js */
/* Arrays */
const _Array = (function() {
    return {
        'make': function(n, z) {
            if (n < 0) throw "Invalid argument: Array.make";

            const type = typeof(z);
            let array;
            // if (type === "number") { // slight hack, we use floats to represent integers
            //     array = new Float64Array(new ArrayBuffer(n*8));
            // } else {
                array = new Array(n);
            // }

            // Initialise the array
            for (let i = 0; i < n; i++)
                array[i] = z;
            return array;
        },
        'get': function(a, i) {
            if (i < 0 || i >= a.length) throw "Out of bounds";
            return a[i];
        },
        'set': function(a, i, x) {
            if (i < 0 || i >= a.length) throw "Out of bounds";
            a[i] = x;
            return {};
        },
        'length': function(a) {
            return a.length;
        }
    };
})();
/* [End] Include /home/dhil/projects/links/my-links/lib/js/array.js */
/* [Begin] Include /home/dhil/projects/links/my-links/lib/js/performance.js */
/* Performance */
/* The following trick is inspired from
   https://github.com/v8/v8/blob/master/test/js-perf-test/base.js
   Performance.now is used in latency benchmarks, the fallback is
   Date.now. */
let _hasPerformanceAPI = this.hasOwnProperty("performance") && performance.hasOwnProperty("now");

const _Performance = (function() {
    const now = _hasPerformanceAPI
        ? performance.now // Milliseconds with microseconds in fractional part
        : Date.now;       // Only milliseconds since Unix EPOCH

    return {
        'now': now,
        'elapsed': function(start, end) {
            const result = _hasPerformanceAPI
                  ? Math.round(end - start) // discards microseconds
                  : end - start;
            return result;
        }
    };
}());
/* [End] Include /home/dhil/projects/links/my-links/lib/js/performance.js */
/* [Begin] Include /home/dhil/projects/links/my-links/lib/js/stack.js */
/* Generalised Stack Inspection runtime */

let _callcount = 0;
let _breakat   = 2;

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

    set rest(r) {
        this._rest = r;
    }

    constructor(/* ContinuationFrame */ first, /* FrameList */ rest) {
        this._first = first;
        this._rest = rest;
    }

    static reverse(/* FrameList */ fs) { /* type FrameList */
        let result = null; /* type FrameList */
        while (fs !== null) {
            result = new FrameList(fs.first, result);
            fs = fs.rest;
        }
        return result;
    }

    static splice(frames0, frames1) {
        let ptr = frames0;
        while (ptr.rest !== null) ptr = ptr.rest;
        ptr.rest = frames1;
        return frames0;
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
     * object so that the (exception) handler can assemble the new
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

class PerformOperationError extends SaveContinuationError {

    constructor(op) {
        super();
        this.op = op;
    }
}
PerformOperationError.prototype.op = {"_label": "NoOp"}

class ResumeError extends Error {
    constructor(f, arg) {
        super();
        this.f = f;
        this.arg = arg;
    }
}
ResumeError.prototype.f = null;
ResumeError.prototype.arg = null;


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
                if (new_frame.continuation !== null && new_frame.continuation !== frames)
                    FrameList.splice(new_frame.continuation, frames);//throw "Continuation not empty?";
                else
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
                if (e instanceof PerformOperationError) {
                    _print("Unhandled operation: " + e.op._label);
                    throw e;
                } else if (e instanceof SaveContinuationError) {
                    const k = e.toContinuation(); /* Continuation */
                    throw new WithinInitialContinuationError(function() {
                        return k.reload(k);
                    });
                } else {
                    throw e;
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

class GenericHandleFrame extends ContinuationFrame {
    constructor(handle, f) {
        super();
        this.handle = handle;
        this.f = f;
    }

    apply(resumeValue) {
        const f = this.f;
        return this.handle.call(null, function() { return f.reload(resumeValue); });
    }
}
GenericHandleFrame.prototype.handle = null;
GenericHandleFrame.prototype.f = null;

/* [End] Include /home/dhil/projects/links/my-links/lib/js/stack.js */

/* [Begin] Bindings */
function get_391() {
  function _get_420_an1_421() {
    let _v_g6_420 = undefined;

    try {
      throw new PerformOperationError({ "_label": "Get", "_value": { "p": {} } });
    } catch(_exn) {
      if (_exn instanceof SaveContinuationError) {
          _exn.extend(new GenericContinuationFrame(_get_422_an2_423));
            throw _exn;
        } else {
          throw _exn;
        }
    }
    return _get_422_an2_423(_v_g6_420);
  }
  
  function _get_422_an2_423(_420) {
    return _420;
  }
  
  _callcount = (_callcount + 1);
  if ((_callcount >= _breakat)) {
    _callcount = 0;
      try {
        return Continuation.CWCC(function(cont) {
            return null;
          });
      } catch(_exn) {
        if (_exn instanceof SaveContinuationError) {
            _exn.extend(new GenericInitialContinuationFrame(_get_420_an1_421));
              throw _exn;
          } else {
            throw _exn;
          }
      }
  }
  return _get_420_an1_421();
}
function main_394() {
  function _main_392_an1_424() {
    let s_392 = undefined;
    
    try {
      s_392 = get_391();
    } catch(_exn) {
      if (_exn instanceof SaveContinuationError) {
          _exn.extend(new GenericContinuationFrame(_main_393_an2_425));
            throw _exn;
        } else {
          throw _exn;
        }
    }
    return _main_393_an2_425(s_392);
  }
  
  function _main_393_an2_425(_392) {
    let t_393 = undefined;
    
    try {
      throw new PerformOperationError({ "_label": "Ask", "_value": { "p": {} } });
    } catch(_exn) {
      if (_exn instanceof SaveContinuationError) {
          _exn.extend(new GenericContinuationFrame(_main_426_an3_427, _392));
            throw _exn;
        } else {
          throw _exn;
        }
    }
    return _main_426_an3_427(t_393, _392);
  }
  
  function _main_426_an3_427(_393, _392) {
    return (_392 + _393);
  }
  
  _callcount = (_callcount + 1);
  if ((_callcount >= _breakat)) {
    _callcount = 0;
      try {
        return Continuation.CWCC(function(cont) {
            return null;
          });
      } catch(_exn) {
        if (_exn instanceof SaveContinuationError) {
            _exn.extend(new GenericInitialContinuationFrame(_main_392_an1_424));
              throw _exn;
          } else {
            throw _exn;
          }
      }
  }
  return _main_392_an1_424();
}

function reader2_403(m_395) {
  _callcount = (_callcount + 1);
    if ((_callcount >= _breakat)) {
      _callcount = 0;
        try {
          return Continuation.CWCC(function(cont) {
              return null;
            });
        } catch(_exn) {
          if (_exn instanceof SaveContinuationError) {
              _exn.extend(new GenericInitialContinuationFrame(reader2_403, m_395));
                throw _exn;
            } else {
              throw _exn;
            }
        }
    }

    function value(_return_value_401) {
        function _reader2_397_an1_428(_401) {
            const x_397 = _401;
            return _reader2_429_an2_430(x_397);
        }

        function _reader2_429_an2_430(_397) {
            return _397;
        }

        return _reader2_397_an1_428(_return_value_401);
    }

    function _handle(f) {
        let v;
        try {
            v = f();
        } catch(_exn) {
            if (_exn instanceof PerformOperationError) {
                switch (_exn.op._label) {
                case 'Ask': {
                    const _399 = _exn.op._value.p;
                    const resume1_396 = function(x) {
                        return _handle(function() { return _exn.toContinuation().reload(x); });
                    };
                    return resume1_396(0);
                }
                default: {
                    // Forwarding
                }
                }
            } else if (_exn instanceof SaveContinuationError) {
                const e = new SaveContinuationError();
                e.extend(new GenericHandleFrame(_handle, _exn.toContinuation()));
                throw e;
            } else {
                throw _exn;
            }
        }
        return value(v);
    }
    return _handle(m_395);
}

function reader_414(m_404) {
  _callcount = (_callcount + 1);
    if ((_callcount >= _breakat)) {
      _callcount = 0;
        try {
          return Continuation.CWCC(function(cont) {
              return null;
            });
        } catch(_exn) {
          if (_exn instanceof SaveContinuationError) {
              _exn.extend(new GenericInitialContinuationFrame(reader_414, m_404));
                throw _exn;
            } else {
              throw _exn;
            }
        }
    }

    function value(_return_value_412) {
        function _reader_406_an4_435(_412) {
            const x_406 = _412;
            
            return _reader_436_an5_437(x_406);
        }
        
        function _reader_436_an5_437(_406) {
            return _406;
        }
        
        return _reader_406_an4_435(_return_value_412);
    }
    
    function _handle(f) {
        let v;
        try {
            v = f();
        } catch(_exn) {
            if (_exn instanceof PerformOperationError) {
                switch (_exn.op._label) {
                case 'Get': {
                    const _408 = _exn.op._value.p;
                    
                    const resume0_405 = function(x) {
                        return _handle(function() { return _exn.toContinuation().reload(x); });
                    };
                    
                    function _reader_410_an1_431(_405) {
                        let _410 = undefined;
                        
                        try {
                            _410 = _405(1);
                        } catch(_exn) {
                            if (_exn instanceof SaveContinuationError) {
                                _exn.extend(new GenericContinuationFrame(_reader_409_an2_432, _405));
                                throw _exn;
                            } else {
                                throw _exn;
                            }
                        }
                        return _reader_409_an2_432(_410, _405);
                    }
                    
                    function _reader_409_an2_432(_410, _405) {
                        let _409 = undefined;
                        
                        try {
                            _409 = _405(2);
                        } catch(_exn) {
                            if (_exn instanceof SaveContinuationError) {
                                _exn.extend(new GenericContinuationFrame(_reader_433_an3_434, _410));
                                throw _exn;
                            } else {
                                throw _exn;
                            }
                        }
                        return _reader_433_an3_434(_409, _410);
                    }
                    function _reader_433_an3_434(_409, _410) {
                        return (_410 + _409);
                    }
                    return _reader_410_an1_431(resume0_405);
                }
                default: {
                    // forward
                    const e = new PerformOperationError(_exn.op);
                    e.extend(new GenericHandleFrame(_handle, _exn.toContinuation()));
                    throw e;
                }
                }
            } else if (_exn instanceof SaveContinuationError) {
                const e = new SaveContinuationError();
                e.extend(new GenericHandleFrame(_handle, _exn.toContinuation()));
                throw e;
            } else {
                throw _exn;
            }
        }
        return value(v);
    }
    return _handle(m_404);
}

function _fun__g5_416() {
  function __fun__g5_415_an1_438() {
    let _415 = undefined;
    
    try {
      _415 = reader_414(main_394);
    } catch(_exn) {
      if (_exn instanceof SaveContinuationError) {
          _exn.extend(new GenericContinuationFrame(__fun__g5_439_an2_440));
            throw _exn;
        } else {
          throw _exn;
        }
    }
    return __fun__g5_439_an2_440(_415);
  }
  
  function __fun__g5_439_an2_440(_415) {
    return _415;
  }
  
  _callcount = (_callcount + 1);
  if ((_callcount >= _breakat)) {
    _callcount = 0;
      try {
        return Continuation.CWCC(function(cont) {
            return null;
          });
      } catch(_exn) {
        if (_exn instanceof SaveContinuationError) {
            _exn.extend(new GenericInitialContinuationFrame(__fun__g5_415_an1_438));
              throw _exn;
          } else {
            throw _exn;
          }
      }
  }
  return __fun__g5_415_an1_438();
}

function run() {
const _417 = Continuation.establishInitialContinuation(function() {
  return reader2_403(_fun__g5_416);
});

/* [End] Bindings */

/* [Begin] Main computation */
return Continuation.establishInitialContinuation(function() {
  return _IO.print(_String.ofNumber(_417));
});
}

run();
/* [End] Main computation */
