'use strict';

const _Inst = (function() {
    /* Instruction kinds */
    const INSTALL = 0, TRAP = 1, RETURN = 2, RESUME = 3, BIND_RESUMPTION = 4;
    const make = function(kind, data) {
        return {"kind": kind, "data": data};
    };

    return {
        "INSTALL": INSTALL,
        "TRAP": TRAP,
        "RETURN": RETURN,
        "RESUME": RESUME,
        "BIND_RESUMPTION": BIND_RESUMPTION,
        "isInstruction": function(inst) {
            return inst !== (void 0) && inst.hasOwnProperty("kind");
        },
        "match": function(inst) {
            return inst !== (void 0) && inst.hasOwnProperty("kind") ? inst.kind : (function() { throw "error: cannot match on non-instruction"; })();
        },
        "make": make,
        "setTrapPoint": function(handler) {
            return make(INSTALL, {"handler": handler});
        },
        "value": function(value) {
            return make(RETURN, {"value": value});
        },
        "trap": function(label, arg) {
            return make(TRAP, {"label": label, "argument": arg});
        },
        "resume": function(resumption, argument) {
            // console.log("RESUME INST " + JSON.stringify(resumption));
            return make(RESUME, {"resumption": resumption, "argument": argument});
        },
        "bindResumption": function() {
            return make(BIND_RESUMPTION);
        }
    };
})();

class PureContinuation {
    constructor() {
        this._frames = [];
    }

    // Number of frames that the continuation contains
    get length() {
        return this._frames.length;
    }

    // Pops a frame from the continuation
    pop() {
        if (this.length > 0)
            return this._frames.pop();
        else
            throw "Empty pure continuation";
    }

    // Pushes a frame onto the continuation
    push(f) {
        this._frames.push(f);
    }
}
PureContinuation.prototype._frames = [];

class ContinuationFrame {
    constructor(handler, pureCont = new PureContinuation()) {
        if (handler === (void 0) || handler === null)
            throw "null handler";
        this._handler = handler;
        this._pureCont = pureCont;
    }

    get pureContinuation() { return this._pureCont; }
    get handler() { return this._handler; }
}
ContinuationFrame.prototype._handler = null;
ContinuationFrame.prototype._pureCont = null;

class ZipperContinuation {
    constructor() {
        this._active = [];
        this._passive = [];
    }

    // Number of active continuation frames
    get activeLength() {
        return this._active.length;
    }

    get passiveLength() {
        return this._passive.length;
    }

    // A pointer to the current pure continuation
    get pureContinuation() {
        const len = this.activeLength;
        if (len > 0) {
            return this._active[len - 1].pureContinuation;
        } else {
            throw "empty continuation";
        }
    }

    // A pointer to the current handler
    get handler() {
        const len = this.activeLength;
        if (len > 0)
            return this._active[len - 1].handler;
        else
            throw "empty continuation";
    }

    // Installs a new handler on the front
    setTrapPoint(handler) {
        this._active.push(new ContinuationFrame(handler, new PureContinuation()));
    }

    // Pops a continuation frame to the back
    unwind() {
        if (this.activeLength > 0) {
            const f = this._active.pop();
            this._passive.push(f);
        } else {
            throw "empty continuation";
        }
    }

    // Pops an active continuation frame
    pop() {
        if (this.activeLength > 0)
            return this._active.pop();
        else
            throw "empty continuation";
    }

    // Makes a list of passive frames active, by reverse appending them onto the current list of active frames
    activatePassives(fs) {
        this._active = this._active.concat(fs.reverse());
    }

    // Reify passive continuation frames
    popPassives() {
        const passive = this._passive;
        this._passive = [];
        return passive;
    }

}
ZipperContinuation.prototype._active = [];
ZipperContinuation.prototype._passive = [];

const _CK = (function() {
    const isGenerator = function(g) {
        return String(g) === '[object Generator]'
    };

    return {
        "run": function(f) {
            let kappa = new ZipperContinuation();
            kappa.setTrapPoint(absurd);
            let cur = f();
            let cur_is_handler = false;
            let result = cur.next();
            // Machine loop
            while (true) {
                //console.log(isGenerator(result.value));
                if (isGenerator(result.value)) {
                    if (!cur_is_handler)
                        kappa.pureContinuation.push(cur);
                    else
                        cur_is_handler = false;
                    cur = result.value;
                    //console.log(result.value);
                    result = cur.next();
                } else if (_Inst.isInstruction(result.value)) {
                    const inst = result.value;
                    //console.log("IS COMMAND");
                    // console.log(JSON.stringify(result.value));
                    switch (_Inst.match(inst)) {
                    case _Inst.INSTALL:
                        kappa.setTrapPoint(inst.data.handler);
                        result = cur.next({});
                        break;
                    case _Inst.TRAP: {
                        kappa.pureContinuation.push(cur);
                        const handler = kappa.handler;
                        kappa.unwind();
                        cur = handler(inst);
                        cur_is_handler = true;
                        result = cur.next();
                        break;
                    }
                    case _Inst.RESUME: {
                        // console.log("Resume: " + JSON.stringify(inst.data.resumption));
                        // console.log(JSON.stringify(kappa));
                        kappa.pureContinuation.push(cur);
                        kappa.activatePassives(inst.data.resumption);
                        // console.log("frames: " + frames.length);
                        cur = kappa.pureContinuation.pop();
                        cur_is_handler = false;
                        result = cur.next(inst.data.argument);
                        break;
                    }
                    case _Inst.BIND_RESUMPTION: {
                        //JSON.stringify(resumption);
                        const frames = kappa.popPassives();
                        result = cur.next(function(x) { return _Inst.resume(frames, x); });
                        break;
                    }
                    default:
                        throw "error: unknown instruction " + JSON.stringify(inst);
                    }
                } else { // value
                    const resumeVal = result.value;
                    //console.log("Continuing with " + resumeVal);
                    if (kappa.activeLength > 0) {
                        if (kappa.pureContinuation.length > 0) {
                            cur = kappa.pureContinuation.pop();
                            if (cur.done) continue;
                            result = cur.next(resumeVal);
                        } else {
                            const handler = kappa.handler;
                            kappa.pop();
                            cur = handler(_Inst.value(resumeVal));
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
    };
})();

const _Handle = (function() {
    return {
        "make": function(val, eff) {
            return function* _handle(inst) {
                switch (_Inst.match(inst)) {
                case _Inst.RETURN:
                    return val(inst.data.value);
                    break;
                case _Inst.TRAP:
                    return eff(inst.data);
                    break;
                default:
                    throw "error: unknown handler instruction";
                }
            }
        }
    };
})();

function* count(n) {
    console.log(n);
    if (n <= 0) return n;
    else return yield count(n-1);
}

function* count2() {
    const n = yield _Inst.trap("Get", {});
    yield _Inst.trap("Print", n);
    if (n <= 0) return n;
    else {
        yield _Inst.trap("Put", n-1);
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
    let i = 0;
    function* _handle(inst) {
        switch (_Inst.match(inst)) {
        case _Inst.RETURN:
            return inst.data.value;
        case _Inst.TRAP:
            switch (inst.data.label) {
            case "Get": {
                const resume = yield _Inst.bindResumption();
                let res = yield resume(st);
                return res;
                break;
            }
            case "Put": {
                let p = inst.data.argument;
                const resume = yield _Inst.bindResumption();
                st = p;
                let res = yield resume({});
                return res;
                break;
            }
            default:
                return yield _Inst.trap(inst.data.label, inst.data.argument);
            }
        default:
            throw "error: unknown handler instruction " + JSON.stringify(result);
        }
    }
    yield _Inst.setTrapPoint(_handle);
    return yield f();
}

// function* absurd(inst) {
//     //console.log("absurd result: " + JSON.stringify(result));
//     // let result = yield setTrapPoint(f);
//     switch (_Inst.match(inst)) {
//     case _Inst.RETURN:
//         return inst.data.value;
//     case _Inst.TRAP:
//         throw "Unhandled operation " + inst.data.label;
//     default:
//         throw "error: unknown handler instruction " + JSON.stringify(inst);
//     }
// }
const absurd = _Handle.make(x => x, op => { throw "Unhandled operation " + op.label; });

function* printer(f) {
    function* _handle(inst) {
        switch (_Inst.match(inst)) {
        case _Inst.RETURN:
            return [inst.data.value];
            break;
        case _Inst.TRAP:
            switch (inst.data.label) {
            case "Print": {
                const p = inst.data.argument;
                const resume = yield _Inst.bindResumption();
                console.log(p);
                return yield resume({});
            }
            default:
                return yield _Inst.trap(inst.data.label, inst.data.argument);
            }
        default:
            throw "error: unknown handler instruction";
        }
    }

    yield _Inst.setTrapPoint(_handle);
    return yield f();
}

function* printer2(f) {
    const eff = function*(op) {
        switch (op.label) {
        case "Print": {
            const p = op.argument;
            const resume = yield _Inst.bindResumption();
            console.log(p);
            return yield resume({});
        }
        default:
            return yield _Inst.trap(inst.data.label, inst.data.argument);
        }
    };

    const val = function*(x) { return [x]; };

    yield _Inst.setTrapPoint(_Handle.make(val, eff));
    return yield f();
}


//var f = function*() { return yield count(300000); };
//var f = function*() { return yield count(30000); };
var f = function*() { return yield printer2(function*(){ return yield state(100000, count2); }); };
// function _run() {
    var x = _CK.run(f);
    print(JSON.stringify(x));
    // return;
// }

