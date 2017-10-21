/* CPS runtime */
const _IO = (function() {
    return {
        'print': function(msg, ks) {
            __print(msg);
            return _K.apply(ks, {});
        },
        'error': function(msg, ks) {
            throw msg;
        },
        'debug': function(msg, ks) {
            __print("[DEBUG] " + msg);
            return _K.apply(ks, {});
        }
    }
}());

/* Continuation operations */
const _K = (function() {
    function absurd(z, ks) {
        return _IO.error("Unhandled operation `" + z._label + "'.", ks);
    }

    function apply(kappa, arg) {
        const k = _List.head(kappa);
        const ks = _List.tail(kappa);
        return k(arg, ks);
    }

    function makeCont(k) {
        return _List.cons(k, _List.cons(absurd, _List.nil));
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
                const kappa = _List.revAppend(s, ks);
                return apply(kappa, x);
            }
        }
    };
}());
