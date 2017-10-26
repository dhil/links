/* Taken from https://github.com/v8/v8/blob/master/test/js-perf-test/base.js
   Performance.now is used in latency benchmarks, the fallback is Date.now. */
var _performance = performance || {};
_performance.now = (function() {
  return _performance.now       ||
         _performance.mozNow    ||
         _performance.msNow     ||
         _performance.oNow      ||
         _performance.webkitNow ||
         Date.now;
})();

const _now = _performance.now;

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
        'toInt': function(n) { return Math.floor(n); }
    }
}());

/* Console */
const __print = console.log || print;

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
            let zs = xs;
            while (zs !== nil && tail(zs) !== nil) { zs = tail(zs); }
            return cons(zs, ys);
        }
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
