'use strict';

// List
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

    const NIL = 0, CONS = 1;

    return {
        'NIL': NIL,
        'CONS': CONS,
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
        },
        'match': function(xs) {
            return xs.size === 0 ? NIL : CONS;
        },
    }
}());

// Machine for generators and iterators
function isGenerator(obj){
    return String(obj) === '[object Generator]'
}

function isCommand(obj) {
    return obj !== undefined && obj.hasOwnProperty("command");
}

function makeOperation(label, arg) {
    return {"kind": "op", "label": label, "arg": arg};
}

function isOperation(obj) {
    return isCommand(obj.value) && obj.value.command === "perform";
}

function isReturn(obj) {
    return isCommand(obj) && obj.command === "return";
}

function setTrapPoint(h) {
    return {'command': 'install', 'handler': h};
}

function* identityHandler(f) {
    let result = yield setTrapPoint(f());

    while (!result.done) {
        // if (isGenerator(result.value)) {
        //     let x = yield {'command': 'augment', 'frame': result.value};
        //     result = it.next(x);
        // } else if (isOperation(result.value)) {
        //     let x = yield {'command': 'forward', 'data': result.value};
        //     result = it.next(x);
        // } else if (isCommand(result.value)) {
        //     let x = yield result.value;
        //     result = it.next(x);
        // } else { // value yielded
        //     let x = yield {'command': 'return', 'data': result.value};
        //     result = it.next(x);
        // }
        result = yield result.value;
    }
    return result;
}

var _K = (function() {
    function makeContinuationFrame(handler, frames) {
        return {'handler': handler, 'frames': frames};
    }

    return {
        'empty': _List.nil,
        'augment': function(kappa, it) {
            let k;
            switch (_List.match(kappa)) {
            case _List.NIL:
                k = List.cons({'handler': identityHandler, 'frames': List.cons(it, _List.nil)}, _List.nil);
                break;
            case _List.CONS:
                const {handler, frames} = List.head(kappa);
                k = List.cons(makeContinuationFrame(handler, List.cons(it, frames)), _List.tail(kappa));
                break;
            }
            return k;
        },
        'setTrapPoint': function(kappa, it) {
            const top = makeContinuationFrame(it, _List.nil);
            return _List.cons(top, kappa);
        },
        'compose': function(kappa0, kappa1) {
            return _List.concat(kappa0, kappa1);
        },
        'popFrame': function(kappa) {
            switch (List.match(kappa)) {
            case _List.NIL:
                throw "Empty continuation";
                break;
            case _List.CONS:
                const {handler, frames} = List.head(kappa);
                k = List.cons(makeContinuationFrame(handler, List.cons(it, frames)), _List.tail(kappa));
                break;
            }
            const top = makeContinuationFrame(handler, _List.cons(it, frames));
            return _List.cons(top, _List.tail(kappa));
        }
    };
})();

function run(f) {
    var kappa = [{"handler": absurd, "frames": []}];
    var cur = f();
    var cur_is_handler = false;
    var result = cur.next();
    var resumption = [];
    // Machine loop
    while (true) {
        //console.log(isGenerator(result.value));
        if (isGenerator(result.value)) {
            if (kappa.length > 0) {
                const {handler, frames} = kappa.pop();
                if (!cur_is_handler)
                    frames.push(cur);
                else
                    cur_is_handler = false;
                kappa.push({"handler": handler, "frames": frames});
                cur = result.value;
                //console.log(result.value);
                result = result.value.next();
            } else {
                throw "Empty continuation stack";
            }
        } else if (isCommand(result.value)) {
            //console.log("IS COMMAND");
            //console.log(JSON.stringify(result.value));
            if (result.value.command === "install") {
                kappa.push({"handler": result.value.handler, "frames": []});
                result = cur.next({});
            } else if (result.value.command === "perform") {
                const {handler, frames} = kappa.pop();
                frames.push(cur);
                resumption.push({"handler": handler,"frames":frames});
                cur = handler(result);
                result = cur.next();
                cur_is_handler = true;
            } else if (result.value.command === "resume") {
                //console.log("Resume: " + JSON.stringify(resumption) + JSON.stringify(kappa));
                kappa = kappa.concat(result.value.resumption.reverse());
                const {handler, frames} = kappa.pop();
                //console.log("frames: " + frames.length);
                cur = frames.pop();
                kappa.push({"handler":handler, "frames": frames});
                result = cur.next(result.value.argument);
            } else if (result.value.command === "bind_resume") {
                result = cur.next(function(x) { return {"command": "resume", "resumption": resumption, "argument": x}; });
                resumption = [];
            } else {
                throw "Unknown command";
            }
        } else { // value
            const resumeVal = result.value;
            //console.log("Continuing with " + resumeVal);
            if (kappa.length > 0) {
                const {handler, frames} = kappa.pop();
                //console.log("Frames " + frames.length);
                if (frames.length > 0) {
                    cur = frames.pop();
                    kappa.push({"handler": handler, "frames": frames});
                    if (cur.done) continue;
                    result = cur.next(resumeVal);
                } else {
                    //console.log("Invoking return " + resumeVal);
                    cur = handler({"command": "return", "value": resumeVal});
                    cur_is_handler = true;
                    result = cur.next();
                }
            } else {
                return resumeVal;
            }
        }
    }
    return result;
}

function* count(n) {
    console.log(n);
    if (n <= 0) return n;
    else return yield count(n-1);
}

function* count2() {
    const n = yield {"command": "perform", "op": {"label": "Get", "value": {}}};
    console.log(n);
    if (n <= 0) return n;
    else {
        yield {"command": "perform", "op": {"label": "Put", "value": n-1}};
        return yield count2();
    }
}

function bindresume() {
    return {"command": "bind_resume"};
}

function resume(x) {
    return {"command": "resume", "argument": x};
}

function* state(s, f) {
    var st = s;
    function* _handle(result) {
        //console.log("HANDLING: " + JSON.stringify(result));
        if (isReturn(result)) {
            return result.value;
        } else if (isOperation(result)) {
            switch (result.value.op.label) {
            case "Get": {
                const resume = yield bindresume();
                let res = yield resume(st);
                return res;
                break;
            }
            case "Put": {
                let p = result.value.op.value;
                const resume = yield bindresume();
                st = p;
                let res = yield resume({});
                return res;
                break;
            }
            default:
                throw "Forwarding";
            }
        } else {
            console.log(JSON.stringify(result));
            throw "Unknown";
        }
    }
    yield setTrapPoint(_handle);
    return yield f();
}

function* absurd(result) {
    //console.log("absurd result: " + JSON.stringify(result));
    // let result = yield setTrapPoint(f);
    if (isOperation(result)) throw "Unhandled operation";
    else if (isReturn(result)) {
        //console.log("Returning absurd");
        return result.value;
    }
    else throw "I don't know what to do!";
}


//var f = function*() { return yield count(300000); };
//var f = function*() { return yield count(30000); };
var f = function*() { return yield state(10000000, count2); };
var x = run(f);
print(JSON.stringify(x));

