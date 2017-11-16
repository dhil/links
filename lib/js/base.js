/* Basic IO */
const _print = this.hasOwnProperty("print") ? print : console.log; // HACK

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
