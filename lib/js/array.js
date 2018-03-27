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
