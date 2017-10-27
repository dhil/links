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

    function copy(env) {
        return fromString(toString(env));
    }

    return {
        'empty': empty,
        'extend': function(env, k, v) {
            // Deep clone (this is somewhat mental...)
            const env0 = copy(env);
            env0[k] = v;
            return env0;
        },
        'lookup': function(env, k) {
            if (env[k] === undefined) throw ("[Env] Lookup failure: " + JSON.stringify(k) + " is not present.");
            else return env[k];
        },
        'unsafeLookup': function(env, k) {
            return env[k];
        },
        'shadow': function(env0, /* by */ env1) {
            const env2 = copy(env0);
            const env3 = copy(env1);
            return Object.assign(env2, env3);
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
        return [x].concat(ys);
    }

    function head(xs) {
        return xs[0];
    }

    function tail(xs) {
        const ys = fromString(toString(xs));
        return ys.slice(1, xs.length);
    }

    return {
        'nil': nil,
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

/* Names */
const Name = (function() {
    let n = 0;
    return {
        'fresh': function(prefix) {
            const number = n++;
            return (prefix + "_" + number);
        }
    };
}());

const IR = (function() {
    const COMPUTATION = 0;  const TAIL_COMPUTATION = 1;
    const SPECIAL     = 2;  const VALUE            = 3;
    const BINDING     = 4;  const APPLY            = 5;
    const VARIABLE    = 6;  const IFTHENELSE       = 7;
    const CASESPLIT   = 8;  const CLAUSE           = 9;
    const OPCLAUSE    = 10; const CONSTANT         = 11;
    const HANDLE      = 12; const DO               = 13;
    const RETURN      = 14; const FUNCTION         = 15;
    return {
        'COMPUTATION': COMPUTATION,
        'TAIL_COMPUTATION': TAIL_COMPUTATION,
        'SPECIAL': SPECIAL,
        'VALUE': VALUE,
        'BINDING': BINDING,
        'APPLY': APPLY,
        'VARIABLE': VARIABLE,
        'IFTHENELSE': IFTHENELSE,
        'CASESPLIT': CASESPLIT,
        'CLAUSE': CLAUSE,
        'OPCLAUSE': OPCLAUSE,
        'CONSTANT': CONSTANT,
        'RETURN': RETURN,
        'FUNCTION': FUNCTION,
        'Make': {
            'computation': function(bindings, tail_comp) {
                return {'tag': COMPUTATION, 'bindings': bindings, 'tail_comp': tail_comp};
            },
            'binding': function(name, tail_comp) {
                return {'tag': BINDING, 'name': name, 'tail_comp': tail_comp};
            },
            'variable': function(name) {
                return {'tag': VARIABLE, 'name': name};
            },
            'apply': function(f, args) {
                return {'tag': APPLY, 'fn': f, 'args': args};
            },
            'ifthenelse': function(cond, tt, ff) {
                return {'tag': IFTHENELSE, 'cond': cond, 'tt': tt, 'ff': ff};
            },
            'casesplit': function(cond, clauses) {
                return {'tag': CASESPLIT, 'cond': cond, 'clauses': clauses};
            },
            'clause': function(label, binders, body) {
                return {'tag': CLAUSE, 'label': label, 'binders': binders, 'body': body};
            },
            'opclause': function(label, binders, resumptionbinder, body) {
                return {'tag': OPCLAUSE, 'label': label, 'binders': binders, 'resumption': resumptionbinder, 'body': body};
            },
            'handle': function(computation, retClause, opClauses, depth) {
                return {'tag': HANDLE, 'comp': computation, 'return': retClause, 'operations': opClauses, 'depth': depth};
            },
            'do': function(label, args) {
                return {'tag': DO, 'label': label, 'args': args};
            },
            'constant': function(value) {
                return {'tag': CONSTANT, 'value': value};
            },
            'ret': function(value) {
                return {'tag': RETURN, 'value': value};
            },
            'fn': function(name, params, body) {
                return {'tag': FUNCTION, 'name': name, 'params': params, 'body':body};
            }
        },
        'match': function(ir) {
            return ir.tag;
        }
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

        const identity = (function() {
            const x = Name.fresh("x");
            return {'tag': IDENTITY, 'env': Env.empty, 'return': IR.Make.clause("Return", [x], IR.Make.computation([], IR.Make.ret(IR.Make.variable(x)))), 'clauses': [], 'depth': "deep"};
        }());

        return {
            'IDENTITY': IDENTITY,
            'USER_DEFINED': USER_DEFINED,
            'make': function(env, retClause, opClauses, depth) {
                return {'tag': USER_DEFINED, 'env': env, 'return': retClause, 'clauses': opClauses, 'depth': depth};
            },
            'handle': function(kappa, op) {
                return; /* TODO */
            },
            'identity': identity
        };
    }());

    const EMPTY = 0; const RETURN = 1; const NEXT = 2;

    function match(kappa) {
        switch (List.match(kappa)) {
        case List.NIL: return EMPTY;
        case List.CONS:
            const top = List.hd(kappa);
            switch (List.match(top.frames)) {
            case List.NIL:  return RETURN;
            case List.CONS: return NEXT;
            }
        }
    }

    return {
        'EMPTY': EMPTY,
        'RETURN': RETURN,
        'NEXT': NEXT,
        'empty': List.nil,
        'Frame': Frame,
        'augment': function(frame, kappa) {
            let k = undefined;
            switch (List.match(kappa)) {
            case List.NIL:
                k = List.cons({'handler': Handler.identity, 'frames': List.cons(frame, List.nil)}, List.nil);
                break;
            case List.CONS:
                const top = List.hd(kappa);
                k = List.cons({'handler': top.handler, 'frames': List.cons(frame, top.frames)}, List.tl(kappa));
                break;
            }
            return k;
        },
        'compose': function(kappa1, kappa2) {
            return {}; /* TODO */
        },
        'Handler': Handler,
        'trap': function(kappa, op) {
            return Handler.handle(kappa, op);
        },
        'setTrap': function(kappa, handler) {
            return List.cons({'handler': handler, 'frames': List.nil}, kappa);
        },
        'match': match,
        'popFrame': function(kappa) {
            switch (match(kappa)) {
            case EMPTY: throw ("Empty");
            case RETURN: throw("Return");
            case NEXT:
                const top    = List.hd(kappa);
                const frame  = List.hd(top.frames);
                const frames = List.tl(top.frames);
                return {'frame': frame, 'kappa': List.cons({'handler': top.handler, 'frames': frames}, List.tl(kappa))};
            }
        },
        'popHandler': function(kappa) {
            switch (match(kappa)) {
            case EMPTY: throw ("Empty");
            case RETURN:
            case NEXT:
                const top    = List.hd(kappa);
                return {'handler': top.handler, 'frames': top.frames, 'kappa': List.tl(kappa)};
            }
        }
    };
})();


const primitives = {
    '%print': function(env, args) {
        print(args[0].value);
        return IR.Make.constant({});
    },
    '%eq': function(env, args) {
        return IR.Make.constant(args[0].value === args[1].value);
    },
    '%sub': function(env, args) {
        return IR.Make.constant(args[0].value - args[1].value);
    },
    '%mult': function(env, args) {
        print("HERE");
        return IR.Make.constant(args[0].value * args[1].value);
    }
};

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

    /* Debug */
    function dump(obj) { print(JSON.stringify(obj)); return; }

    /* Entry function */
    function interpret(funs, program) {
        /* Value interpretation */
        function value(env, val) {
            let result = undefined;
            switch (IR.match(val)) {

            case IR.CONSTANT: {
                result = val;
                break;
            }

            case IR.VARIABLE: {
                const v = Env.unsafeLookup(env, val.name) || Env.unsafeLookup(funs, val.name);
                if (v === undefined) {
                    throw ("Unknown variable " + v);
                }
                v['isprimitive'] = (Env.unsafeLookup(primitives, v.value) !== undefined);
                //dump(v);
                result = v;
                break;
            }
            default:
                throw ("Value interpretation: " + (JSON.stringify(val)));
            }

            return result;
        }

        // Interpreter
        let mode = MODE.COMPUTATION;

        let control = program;       // C
        let environment = Env.empty; // E
        let kontinuation = K.empty;  // K

        /* Main interpreter loop */
        while (true) {
            //dump(control);
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
                    // dump(environment);
                    // dump(control.args);
                    const fn = value(environment, control.fn);
                    // print("APPLY"); dump(fn);
                    // dump(control.args);
                    // dump(environment);
                    const args = List.map(v => value(environment, v), control.args);
                    //dump(fn);
                    // Check whether the function is primitive
                    if (fn.isprimitive) {
                        if (primitives[fn.value] !== undefined) {
//                            dump(args);
                            const v = primitives[fn.value](environment, args);

                            // Set up for continuation application
                            mode = MODE.APPLY_CONTINUATION;
                            control = v;
                        } else {
                            throw ("Unknown primitive function " + fn.name);
                        }
                    } else {
                        // Set up for application
                        // mode = MODE.APPLY;
                        // control = {'fn':fn, 'args':args};

                        let env = environment;
                        // Bind arguments
                        for (let i = 0; i < args.length; i++)
                            env = Env.extend(env, fn.params[i], args[i]);

                        mode = MODE.COMPUTATION;
                        control = fn.body;
                        environment = env;
                    }
                    continue
                    break;
                }
                case IR.IFTHENELSE: {
                    const cond = value(environment, control.cond);
                    // dump(cond);
                    const branch = cond.value === true ? control.tt : control.ff;
                    //print("BRANCH");  dump(branch); dump(control.ff);
                    // Prepare to evaluate branch
                    mode = MODE.COMPUTATION;
                    control = branch;
                    break; }
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
                    //dump(control.bindings);
                    const b = List.hd(control.bindings);
                    const bs = List.tl(control.bindings);
                    const tc = b.tail_comp;

                    // Augment continuation
                    const frame = K.Frame.make(environment, b.name, IR.Make.computation(bs, control.tail_comp));
                    // print("AUGMENT");
                    // dump(b.name);
                    // dump(frame);
                    const kappa = K.augment(frame, kontinuation);

                    // Set up next instruction
                    // dump(kontinuation); // FIXME
                    mode = MODE.TAIL_COMPUTATION;
                    control = tc;
                    kontinuation = kappa;
                    continue;
                    break; }
                }
            }
            case MODE.SPECIAL: /* Special interpretation */
                break;
            case MODE.APPLY: /* Function application interpretation */
                break;
            case MODE.APPLY_CONTINUATION: { /* Continuation application interpretation */
                switch (K.match(kontinuation)) {
                case K.EMPTY: return control.value;
                case K.RETURN: { // Invoke handler
                    const { handler, kappa } = K.popHandler(kontinuation);

                    // if (handler.tag === K.Handler.IDENTITY) {
                    //     kontinuation = kappa;
                    //     mode = MODE.COMPUTATION;
                    //     control = IR.Make.computation([], IR.Make.ret(control));
                    // } else {
                        // Prepare to evaluate the body of the return clause
                    const b = handler['return'].binders[0];
                    const env = Env.extend(handler.env, b, control);
                    mode = MODE.COMPUTATION;
                    control = handler['return'].body;
                    // dump(environment);
                    // dump(control);
                    environment = env;
                    kontinuation = kappa;
                    // }
                    continue;
                    break; }

                case K.NEXT: {
                    const { frame, kappa } = K.popFrame(kontinuation);
                    print("Binding " + frame.binder + " := " + JSON.stringify(control));
                    const env = Env.extend(Env.shadow(environment, frame.env), frame.binder, control);

                    // Prepare to evaluate computation
                    mode = MODE.COMPUTATION;
                    control = frame.computation;
                    environment = env;
                    kontinuation = kappa;
                    continue;
                    break; }
                }
            }
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

let bindings = [IR.Make.binding("foo", IR.Make.ret(IR.Make.constant(43)))];
let factorial = IR.Make.fn("fact", ["n"],
                           IR.Make.computation([IR.Make.binding("b", IR.Make.apply(IR.Make.variable("%eq"), [IR.Make.variable("n"), IR.Make.constant(0)]))],
                                                IR.Make.ifthenelse(IR.Make.variable("b"),
                                                                   IR.Make.computation([], IR.Make.ret(IR.Make.constant(1))),
                                                                   IR.Make.computation([IR.Make.binding("n1", IR.Make.apply(IR.Make.variable("%sub"), [IR.Make.variable("n"), IR.Make.constant(1)])),
                                                                                        IR.Make.binding("m", IR.Make.apply(IR.Make.variable("fact"), [IR.Make.variable("n1")])),
                                                                                        IR.Make.binding("res", IR.Make.apply(IR.Make.variable("%mult"), [IR.Make.variable("n"), IR.Make.variable("m")]))
                                                                                       ],
                                                                                       IR.Make.ret(IR.Make.variable("res"))))));

//let program = IR.Make.computation(bindings, IR.Make.apply(IR.Make.variable("%print"), [IR.Make.variable("foo")]));
let program = IR.Make.computation([], IR.Make.apply(IR.Make.variable("fact"), [IR.Make.constant(4)]));
let result  = CEK.run({'%eq': {'value': "%eq"}, '%sub': {'value': "%sub"}, '%mult': {'value': "%mult"}, '%print': {'value':"%print"}, 'fact': factorial}, program);
print(JSON.stringify(result));
