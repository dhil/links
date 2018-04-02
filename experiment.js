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

    constructor() {
        super(FrameList.singleton(new AbstractHandleFrame()), FrameList.nil());
    }

    invoke(x) {
        let handlers = this.assemble();
        let result = x;
        let exn = new Error();
        let discontinue = false;
        try {
            while (handlers !== null) {
                if (discontinue) {
                    result = handlers.head.discontinue(exn);
                    handlers = handlers.tail;
                    discontinue = false;
                } else {
                    result = handlers.head.invoke(result);
                    handlers = handlers.tail;
                }
            }
        } catch (e) {
            if (e instanceof PerformOperationError) {
                if (discontinue) {
                    // Merge back in the original pure continuation
                    e.handler.pureContinuation.compose(handlers.head.pureContinuation);
                }
                // discontinue handlers.tail with e
                discontinue = true;
                handlers = handlers.tail;
                exn = e;
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

        return result;
    }

    instantiateTrapPoint(handleFn) {
        const head = this.newFrames.head.instantiate(handleFn);
        this.newFrames = FrameList.cons(head, this.newFrames.tail);
        return;
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
            const k = exn.continuation;
            exn.continuation.setAbstractTrapPoint();
            return function(x) {
                return k.invoke(x);
            };
        }
    };
})();
/* [End] Include /home/dhil/projects/links/my-links/lib/js/stack.js */

/* [Begin] Bindings */
function get_391() {
  function _get_420_an1_421() {
    let _v_g6_420 = undefined;

    try {
      throw new PerformOperationError({ "_label": "Get", "_value": { "p": {} } });
    } catch(_exn) {
      if (_exn instanceof SaveContinuationError) {
          _exn.continuation.augment(new GenericPureContinuationFrame(_get_422_an2_423));
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
            _exn.continuation.augment(new GenericInitialPureContinuationFrame(_get_420_an1_421));
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
          _exn.continuation.augment(new GenericPureContinuationFrame(_main_393_an2_425));
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
          _exn.continuation.augment(new GenericPureContinuationFrame(_main_426_an3_427, _392));
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
            _exn.continuation.augment(new GenericInitialPureContinuationFrame(_main_392_an1_424));
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
              _exn.continuation.augment(new GenericInitialPureContinuationFrame(reader2_403, m_395));
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
                const _resume = _Resumption.makeDeep(_exn, _handle);
                switch (_exn.op._label) {
                case 'Ask': {
                    const _399 = _exn.op._value.p;
                    const resume1_396 = _resume;
                    return resume1_396(0);
                }
                default: {
                    // Forwarding
                }
                }
            } else if (_exn instanceof SaveContinuationError) {
                _exn.continuation.instantiateTrapPoint(_handle);
                _exn.continuation.setAbstractTrapPoint();
                throw _exn;
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
              _exn.continuation.augment(new GenericInitialPureContinuationFrame(reader_414, m_404));
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
            v = value(f());
        } catch(_exn) {
            if (_exn instanceof PerformOperationError) {
                const _resume = _Resumption.makeDeep(_exn, _handle);
                switch (_exn.op._label) {
                case 'Get': {
                    const _408 = _exn.op._value.p;
                    
                    const resume0_405 = _resume;
                    
                    function _reader_410_an1_431(_405) {
                        let _410 = undefined;
                        
                        try {
                            _410 = _405(1);
                        } catch(_exn) {
                            if (_exn instanceof SaveContinuationError) {
                                _exn.continuation.augment(new GenericPureContinuationFrame(_reader_409_an2_432, _405));
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
                                _exn.continuation.augment(new GenericPureContinuationFrame(_reader_433_an3_434, _410));
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
                    _exn.continuation.instantiateTrapPoint(_handle);
                    _exn.continuation.setAbstractTrapPoint();
                    throw _exn;
                }
                }
            } else if (_exn instanceof SaveContinuationError) {
                _exn.continuation.instantiateTrapPoint(_handle);
                _exn.continuation.setAbstractTrapPoint();
                throw _exn;
            } else {
                throw _exn;
            }
        }
        return v;
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
          _exn.continuation.augment(new GenericPureContinuationFrame(__fun__g5_439_an2_440));
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
            _exn.continuation.augment(new GenericInitialPureContinuationFrame(__fun__g5_415_an1_438));
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
