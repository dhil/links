/* CPS runtime */
const _IO = (function() {
    return {
        'print': function(msg, ks) {
            _print(msg);
            return _K.apply(ks, {});
        },
        'error': function(msg, ks) {
            throw msg;
        },
        'debug': function(msg, ks) {
            print("[DEBUG] " + msg);
            return _K.apply(ks, {});
        }
    }
}());

const _ListCPS = (function() {
    return {
        'head': function(xs, kappa) {
            return _K.apply(kappa, _List.head(xs));
        },
        'tail': function(xs, kappa) {
            return _K.apply(kappa, _List.tail(xs));
        }
    };
}());

const _ArrayCPS = (function() {
    return {
        'make': function(n, z, ks) {
            return _K.apply(ks, _Array.make(n, z));
        },
        'get': function(a, i, ks) {
            return _K.apply(ks, _Array.get(a, i));
        },
        'set': function(a, i, x, ks) {
            return _K.apply(ks, _Array.set(a, i, x));
        }
    };
})();

/* Continuation operations */
const _K = (function() {
    let callcount = 0;
    let breakat = 100;

    function makeFrame(pureFrames, ret, eff) {
        return { frames: pureFrames, ret: ret, eff: eff };
    }

    function augment(f, ks) {
        if (ks === _List.nil) throw "cannot augment empty continuation"; /* TODO FIXME */
        else {
            const { frames, ret, eff } = _List.head(ks);
            return _List.cons(makeFrame(_List.cons(f, frames), ret, eff), _List.tail(ks));
        }
    }

    function absurd(z, ks) {
        return _IO.error("Unhandled operation `" + z._label + "'.", ks);
    }

    function apply(kappa, arg) {
        callcount++;
        if (callcount >= breakat) {
            callcount = 0;
            throw new BounceError(kappa, arg);
        }

        if (kappa === _List.nil) throw "empty continuation";

        const { frames, ret, eff } = _List.head(kappa);
        if (frames === _List.nil) // U-KAppNil
            return ret(arg, _List.tail(kappa));
        else {                    // U-KAppCons
            const k = _List.head(frames);
            const ks = _List.tail(frames);
            const fs = _List.cons(makeFrame(ks, ret, eff), _List.tail(kappa));
            return k(arg, fs);
        }
    }

    function trap(kappa, pack) {
        if (kappa === _List.nil) throw "empty continuation";

        const frame = _List.head(kappa);
        const pack_ = {'_label': z._label,
                      '_value': {'p': pack._value.p,
                                 's': _List.cons(frame, pack._value.s) }
                     };
        return frame.eff(pack_, _List.tail(kappa));
    }

    function forward(kappa, pack) {
        if (kappa === _List.nil) throw "empty continuation";
        const frame = _List.head(kappa);
        const pack_ = {'_label': pack._label,
                       '_value': {'p': pack._value.p,
                                  's': _List.cons(frame, pack._value.s)}
                      };
        return frame.eff(pack_, _List.tail(kappa));
    }

    const identity = _List.singleton(makeFrame(_List.nil, function(x, fs) { return x; }, absurd));

    function makeCont(f) {
        return augment(f, identity);
    }

    return {
        'apply': apply,
        'trap': trap,
        'identity': identity,
        'absurd': absurd,
        'deepResume': function(sf) {
            return function (x, ks) {
                const kappa = _List.revAppend(sf, ks);
                return apply(kappa, x);
            }
        },
        'shallowResume': function(sf) {
            if (sf === _List.nil) throw "empty resumption stack";
            const ks_ = _List.head(sf).frames;
            const sf_ = _List.tail(sf);
            return function (x, fs) {
                if (fs === _List.nil) throw "empty continuation";

                const { frames, ret, eff } = _List.head(fs);
                const kappa = _List.cons(makeFrame(_List.concat(ks_, frames), ret, eff), _List.tail(fs));
                const kappa_ = _List.revAppend(sf_, kappa);
                return apply(kappa_, x);
            }
        },
        'makeCont': makeCont,
        'makeFrame': makeFrame,
        'forward': forward,
        'augment': augment
    };
}());

class BounceError extends Error {

    get kappa() { return this._kappa; }
    set kappa(k) { this._kappa = k; }
    get arg() { return this._arg; }
    set arg(x) { this._arg = x; }

    constructor(kappa, arg) {
        super();
        this._kappa = kappa;
        this._arg = arg;
    }
}

const _Trampoline = (function() {
    return {
        'run': function(ks, arg) {
            while(true) {
                try {
                    _K.apply(ks, arg);
                    break;
                } catch (exn) {
                    if (exn instanceof BounceError) {
                        ks = exn.kappa;
                        arg = exn.arg;
                        continue;
                    } else {
                        throw exn;
                    }
                }
            }
            return {};
        }
    };
})();
