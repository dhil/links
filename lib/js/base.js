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
            let out = ys;
            let zs = xs;
            while (zs !== nil) {
                out = cons(head(zs), out);
                zs = tail(zs);
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
// const _List = (function() {
//     const NIL = 0, CONS = 1;
//     const nil = Immutable.List();

//     function cons(x, xs) {
//         return xs.unshift(x);
//     }

//     function head(xs) {
//         const v = xs.first();
//         if (v === undefined) throw "List.hd error";
//         return v;
//     }

//     function tail(xs) {
//         return xs.shift();
//     }

//     function rev(xs) {
//         return xs.reverse();
//     }

//     function append(xs, ys) {
//         return xs.concat(ys);
//     }

//     return {
//         'nil': nil,
//         'cons': cons,
//         'head': head,
//         'tail': tail,
//         'NIL': NIL,
//         'CONS': CONS,
//         'match': function(xs) {
//             return xs.size === 0 ? NIL : CONS;
//         },
//         'map': function(f, xs) {
//             return xs.map(f);
//         },
//         'rev': rev,
//         'concat': append,
//         'revAppend': function(xs, ys) {
//             var out = ys;
//             while (xs !== nil) {
//                 out = cons(head(xs), out);
//                 xs = tail(xs);
//             }
//             return out;
//         },
//         'singleton': function(x) {
//             return cons(x, nil);
//         },
//         'of': function(xs) {
//             return Immutable.List(xs);
//         },
//         'length': function(xs) {
//             return xs.size;
//         }
//     };
// })();

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
            const xs = [x];
            return function() {
                return f.apply(null, xs.concat(Array.prototype.slice.call(arguments)));
            }
        },
        '_yield': function(f, x) {
            return function*() {
                return yield* f.apply(this, [x].concat(Array.prototype.slice.call(arguments)));
            }
        }
    };
}());
