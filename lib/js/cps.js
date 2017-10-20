/* CPS runtime */
const _IO = (function() {
    const print = console.log || debug;

    return {
        'print': function(msg, ks) {
            print(msg);
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

/* List operations */
const _List = (function() {
    const nil = null;
    function _cons(x, xs) {
        return {'_head': x, '_tail': xs};
    }

    function _head(xs) { return xs['_head']; }
    function _tail(xs) { return xs['_tail']; }
    function _singleton(x) { return _cons(x, nil) };

    function cons(x, xs, ks) {
        return _K.apply(ks, _cons(x, xs));
    }

    function head(xs, ks) {
        return _K._apply(ks, _head(xs));
    }

    function tail(xs, ks) {
        return _K._apply(ks, _tail(xs));
    }

    return {
        '_nil': nil,
        '_cons': _cons,
        '_head': _head,
        '_tail': _tail,
        '_singleton': _singleton,
        '_revAppend': function(xs, ys) {
            var out = ys;
            while (xs !== nil) {
                out = _cons(_head(xs), out);
                xs = _tail(xs);
            }
            return out;
        },
        'nil': nil,
        'head': _head,
        'tail': _tail
    }
}());

/* Closure operations */
const _Closure = (function() {
    return {
        'apply': function(f, x) {
            return function() {
                return f.apply(this, [x].concat(Array.prototype.slice.call(arguments)));
            }
        }
    };
}());

/* Continuation operations */
const _K = (function() {
    function absurd(z, ks) {
        return _IO.error("Unhandled operation `" + z._label + "'.", ks);
    }

    function apply(kappa, arg) {
        const k = _List._head(kappa);
        const ks = _List._tail(kappa);
        return k(arg, ks);
    }

    function makeCont(k) {
        return _List._cons(k, _List._cons(absurd, _List._nil));
    }

    function vmap(f, z) {
        return {'_label': z._label,
                '_value': {'p': z._value.p,
                           's': f(z._value.s)}
               };
    }

    return {
        'apply': apply,
        'identity': makeCont(function(x, ks) { return x; }),
        'absurd': absurd,
        'pure': function(x, ks) { return x; },
        'vmap': vmap,
        'makeFun': function(s) {
            return function (x, ks) {
                const kappa = _List._revAppend(s, ks);
                return apply(kappa, x);
            }
        }
    };
}());
