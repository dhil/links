/* CEK runtime */
'use strict';

/* Immutable environments */
const Env = (function() {
    const empty = {};

    function toString(env) {
        return JSON.stringify(env);
    }

    function fromString(stringified_env) {
        return JSON.parse(stringified_env);
    }

    return {
        'empty': empty,
        'extend': function(env, k, v) {
            // Deep clone (this is somewhat mental...)
            const copy = fromString(toString(env));
            copy[k] = v;
            return copy;
        },
        'lookup': function(env, k) {
            if (env[k] === undefined) throw ("[Env] Lookup failure: " + k + " is not present.");
            else return env[k];
        }
    };
}());

let env0 = Env.empty;
let env1 = Env.extend(env0, "foo", 42);
let env2 = Env.extend(env1, "bar", -42);
let env3 = Env.extend(env1, "foo", 0);


const List = (function() {
    const NIL = 0;
    const CONS = 1;

    function toString(xs) {
        return JSON.stringify(xs);
    }

    function fromString(stringified_list) {
        return JSON.parse(stringified_list);
    }

    const nil = [];

    function cons(x, xs) {
        const ys = fromString(toString(xs));
        ys.push(x);
        return ys;
    }

    function head(xs) {
        return xs[xs.length-1];
    }

    function tail(xs) {
        const ys = fromString(toString(xs));
        ys.pop();
        return ys;
    }

    return {
        'nil' nil,
        'cons': cons,
        'hd': head,
        'tl': tail,
        'NIL': NIL,
        'CONS': CONS,
        'match': function(xs) {
            return xs.length === 0 ? NIL : CONS;
        },
        'fromJSList': function(xs) {
            return fromString(toString(xs));
        },
        'map': function(f, xs) {
            // Sigh... every operation on JS lists mutates its list argument, expect for map...
            return xs.map(f);
        },
    };
}());

/* Continuations */
const K = (function() {
    const Frame = (function() {
        return {
            'make': function(env, binder, comp) {
                return {'env': env, 'binder': binder, 'computation': comp};
            }
        };
    }());

    const Handler = (function() {
        const IDENTITY = 0;
        const USER_DEFINED = 1;

        return {
            'IDENTITY': IDENTITY,
            'USER_DEFINED': USER_DEFINED,
            'make': function(env, retClause, opClauses, depth) {
                return {'tag': USER_DEFINED, 'env': env, 'return': retClause, 'clauses': opClauses, 'depth': depth};
            },
            'handle': function(kappa, op) {
                return; /* TODO */
            },
            'identity': function() {
                return {'tag': IDENTITY};
            }
        };
    }());

    return {
        'empty': List.nil,
        'Frame': Frame,
        'augment': function(kappa, frame) {
            let k = undefined;
            switch (List.match(kappa)) {
            case List.NIL:
                k = {'handler': Handler.identity, 'frames': List.cons(frame, List.nil)};
                break;
            case List.cons:
                const top = List.hd(kappa);
                k = {'handler': top.handler, 'frames': List.cons(frame, top.frames)};
                break;
            }
            return k;
        },
        'compose': function(kappa1, kappa2) {
            return {}; /* TODO */
        },
        'trap': function(kappa, op) {
            return Handler.handle(kappa, op);
        },
        'setTrap': function(kappa, handler) {
            return List.cons({'handler': handler, 'frames': List.nil}, kappa);
        }
    };
})();

const IR = (function() {
    const COMPUTATION = 0;
    const TAIL_COMPUTATION = 1;
    const SPECIAL = 2;
    const VALUE = 3;
    const BINDING = 4;
    return {
        'COMPUTATION': COMPUTATION,
        'TAIL_COMPUTATION': TAIL_COMPUTATION,
        'SPECIAL': SPECIAL,
        'VALUE': VALUE,
        'BINDING': BINDING,
        'Make': {
            'computation': function(bindings, tail_comp) {
                return {'tag': COMPUTATION, 'bindings': bindings, 'tail_comp': tail_comp};
            },
            'binding': function(identifier, tail_comp) {
                return {'tag': BINDING, 'identifier': identifier, 'tail_comp': tail_comp};
            },
        },
        'match': function(ir) {
            return ir.tag;
        }
    };
}());

const CEK = (function() {
    /* Machine modes */
    const MODE = {
            'VALUE': 0,
            'TAIL_COMPUTATION': 1,
            'COMPUTATION': 2,
            'SPECIAL': 3,
            'APPLY': 4,
            'APPLY_CONTINUATION': 5
    };

    /* Value interpretation */
    function value(env, val) {
        return val; /* TODO */
    }

    /* Entry function */
    function interpret(program) {
        let mode = MODE.COMPUTATION;

        let control = program;
        let environment = Env.empty;
        let kontinuation = K.empty;

        /* Main interpreter loop */
        while (true) {
            switch (mode) {
            case MODE.VALUE: /* Value interpretation */
                break;

            /* Tail computation interpretation */
            case MODE.TAIL_COMPUTATION: {

                switch (IR.match(control)) {
                case IR.RETURN: {
                    // Interpret value
                    const val = value(environment, control.value);

                    // Set up next instruction
                    mode = MODE.APPLY_CONTINUATION;
                    control = val;
                    continue;
                    break;
                }
                case IR.APPLY: {
                    // Transform
                    const f = value(environment, control.fun);
                    const args = List.map(v => value(environment, v), control.args);

                    // Set up for application
                    mode = MODE.APPLY;
                    control = {'f':f, 'args':args};
                    continue
                    break;
                }
                default: {
                    throw ("Case not yet implemented");
                }
                }
                break;
            }
            /* Computation interpretation */
            case MODE.COMPUTATION: {
                // match on bindings
                switch (List.match(control.bindings)) {
                case List.NIL: {
                    // Set up next instruction
                    const tc = control.tail_comp;
                    mode = MODE.TAIL_COMPUTATION;
                    control = tc;
                    continue;
                    break; }
                case List.CONS: {
                    // Destruct bindings
                    const b = List.hd control.bindings;
                    const bs = List.tl control.bindings;
                    const tc = b.tail_comp;

                    // Augment continuation
                    const frame = K.Frame.make(environment, b.identifier, IR.Make.computation(bs, control.tail_comp));
                    const kappa = K.augment(frame, kontinuation);

                    // Set up next instruction
                    kontinuation = kappa;
                    mode = MODE.TAIL_COMPUTATION;
                    control = tc;
                    continue;
                    break; }
                }
            }
            case MODE.SPECIAL: /* Special interpretation */
                break;
            case MODE.APPLY: /* Function application interpretation */
                break;
            case MODE.APPLY_CONTINUATION: /* Continuation application interpretation */
                break;
            default:
                throw ("Unknown MODE: " + mode);
            }
        }

        return {};
    }

    return {
        'run': interpret
    };
}());

let program = {'bindings':[], 'tail_computation': {} };
CEK.run(program);
