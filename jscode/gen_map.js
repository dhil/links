/** Handwritten **/
'use strict';

let nil = null;
function cons(x, xs) {
    return {'_head': x, '_tail': xs};
}

function length(xs) {
    let acc = 0;
    let ys = xs;
    while (ys !== nil) { acc += 1; ys = ys['_tail']; }
    return acc;
}

const _Toplevel = (function() {
    return {
        'run': function(f) {
            const result = f().next();
            if (result.done) {
                return result.value;
            } else {
                throw ("Unhandled: " + result.value.op);
            }
        }
    }
}());

function* map(f, xs) {
    if (xs !== nil) {
        const x0 = xs['_head'];
        const x1 = yield* f(x0);
        const xs0 = xs['_tail'];
        const xs1 = yield* map(f, xs0);
        return {'_head': x1, '_tail': xs1 };
    } else {
        return nil;
    }
}

function* incr(x) { return x+1; }

let f = function*() {
    const xs = cons(0, cons(1, nil));
    const ys = yield* map(incr, xs);
    print("[" + ys._head + ", " + ys._tail._head + "]");
    return ys;
};
_Toplevel.run(f)
