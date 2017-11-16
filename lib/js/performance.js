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
