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
