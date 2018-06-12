/* Basic IO */
const _print = (function(that) {
    if (that.console !== undefined)
        return that.console.log !== undefined ? that.console.log : print; // HACK
    else
        return print;
})(this);

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
        'toInt': function(n) { return Math.floor(n); },
        'integerDivision': function(n, m) { return Math.floor(n / m); }
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

/* Records */
const _Record = (function() {
    return {
        'union': function(r, s) {
            return Object.assign({}, r, s);
        },
        'erase': function(r, labels) {
            let s = {};
            let ls = new Set(labels);
            for (let l in r) {
                if (ls.has(l)) continue;
                else s[l] = r[l]
            }
            return s;
        }
    };
})();

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
