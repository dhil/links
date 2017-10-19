/* CPS runtime */

/* List operations */
const __nil = null;
function __cons(x, xs) {
    return {'_head': x, '_tail': xs}
}
function __head(xs) { return xs['_head']; }
function __tail(xs) { return xs['_tail']; }

function _cons(x, xs, ks) {
    return _apply_continuation(ks, __cons(x, xs));
}
const _nil = __nil;

function _head(xs, ks) {
    return _apply_continuation(ks, __head(xs));
}

function _tail(xs, ks) {
    return _apply_continuation(ks, __tail(xs));
}

/* Closure operations */
function _partial_apply(f, x) {
    return function() {
    return f.apply(this, [x].concat(Array.prototype.slice.call(arguments)));
  }
}

/* Continuation operations */
function _apply_continuation(ks, arg) {
    const k = _head(ks);
    const ks = _tail(ks);
    return k(arg, ks);
}
