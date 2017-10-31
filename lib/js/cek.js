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
    const INJECT      = 16; const VARIANT          = 17;
    const EXTEND      = 18; const RECORD           = 19;
    const PROJECT     = 20; const PRIMITIVE        = 21;


    function record(fields) {
        return {'tag': RECORD, 'fields': fields};
    }

    function tuple(values) {
        const t = {};
        for (let i = 0; i < values.length; i++)
            t[i+1] = values[i];
        return record(t);
    }

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
        'INJECT': INJECT,
        'VARIANT': VARIANT,
        'EXTEND': EXTEND,
        'RECORD': RECORD,
        'PROJECT': PROJECT,
        'PRIMITIVE': PRIMITIVE,
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
            'casesplit': function(scrutinee, clauses) {
                return {'tag': CASESPLIT, 'scrutinee': scrutinee, 'clauses': clauses};
            },
            'clause': function(binders, body) {
                return {'tag': CLAUSE, 'binders': binders, 'body': body};
            },
            'opclause': function(binders, resumptionbinder, body) {
                return {'tag': OPCLAUSE, 'binders': binders, 'resumption': resumptionbinder, 'body': body};
            },
            'handle': function(computation, retClause, opClauses, depth) {
                return {'tag': HANDLE, 'comp': computation, 'return': retClause, 'operations': opClauses, 'depth': depth};
            },
            'resumption': function(depth, handler, frames, stack) {
                let kappa = undefined;
                if (depth === "deep")
                    kappa = List.rev(List.cons({'handler': handler, 'frames': frames}, stack));
                else
                    kappa = {'frames': frames, 'continuation': List.rev(stack)};
                return {'tag': RESUMPTION, 'depth': depth, 'kappa': kappa};
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
            'fn': function(name, params, venv, body) {
                return {'tag': FUNCTION, 'name': name, 'params': params, 'venv': venv, 'body':body};
            },
            'inject': function(label, args) {
                return {'tag': INJECT, 'label': label, 'args': args};
            },
            'variant': function(label, args) {
                return {'tag': VARIANT, 'label': label, 'args': args};
            },
            'value': function(value) {
                return {'value': value};
            },
            'unit': record({}),
            'record': record,
            'extend': function(fields, record) {
                return {'tag': EXTEND, 'fields': fields, 'record': record};
            },
            'project': function(record, label) {
                return {'tag': PROJECT, 'record': record, 'label': label};
            },
            'tuple': tuple,
            'closure': function(fn, fvs) {
                return {'tag': CLOSURE, 'fn': fn, 'fvs': fvs};
            },
            'primitive': function(name) {
                return {'tag': PRIMITIVE, 'name': name};
            }
        },
        'match': function(ir) {
            return ir.tag;
        }
    };
}());

const Clause = (function() {
    return {
        'lookup': function(clauses, label) {
            return clauses[label];
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
            return {'tag': IDENTITY, 'env': Env.empty, 'return': IR.Make.clause([x], IR.Make.computation([], IR.Make.ret(IR.Make.variable(x)))), 'clauses': {}, 'depth': "deep"};
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
            return List.append(kappa1, kappa2);
        },
        'prependFrames': function(frames, kappa) {
            // assertion: non empty kappa
            const top = List.hd(kappa);
            return List.cons({'handler': top.handler, 'frames': List.append(frames, top.frames)}, List.tl(kappa));
        },
        'Handler': Handler,
        'trap': function(kappa, label, args) {
            let k = kappa;
            let handler = undefined;
            let frames  = undefined;
            let found = false;
            let stack = List.nil;
            while (!found || List.match(k) !== List.NIL) {
                const top = List.hd(k);
                if (top.handler.clauses[label] !== undefined) {
                    handler = top.handler;
                    frames  = top.frames;
                    found = true;
                    break;
                }

                // Peel off the top impure continuation
                stack = List.cons(top, stack);
                k = List.tl(k);
            }

            // Check whether we found a suitable handler
            if (!found) throw ("No suitable handler for " + label + " is installed.");

            // Bind arguments
            let henv = Env.extend(handler.env, handler.clauses[label].binders, args);
            // Bind resumption
            henv = Env.extend(henv, handler.clauses[label].resumptionbinder, IR.Make.resumption(depth, handler, frames, stack));

            return {'env': henv, 'body': handler.clauses[label].body, 'kappa': k};
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
                const top = List.hd(kappa);
                return {'handler': top.handler, 'frames': top.frames, 'kappa': List.tl(kappa)};
            }
        }
    };
})();

const primitives = (function() {
    return {
        '%print': function(env, args) {
            print(args[0].value);
            return IR.Make.unit;
        },
        '%eq': function(env, args) {
            return IR.Make.constant(args[0].value === args[1].value);
        },
        '%leq': function(env, args) {
            return IR.Make.constant(args[0].value <= args[1].value);
        },
        '%sub': function(env, args) {
            return IR.Make.constant(args[0].value - args[1].value);
        },
        '%mult': function(env, args) {
            return IR.Make.constant(args[0].value * args[1].value);
        }
    };
}());

const Value = (function() {
    function toString(val) {
        if (val.tag === undefined) return "" + val;
        //print(JSON.stringify(val));
        switch (IR.match(val)) {
        case IR.CONSTANT:
            return ("" + val.value);
        case IR.VARIANT:
            return ("" + val.label + toString(val.args));
        case IR.RECORD:
            let row = "";
            let fields = [];
            // Copy fields
            for (let label in val.fields)
                fields.push({'label': label, 'value': toString(val.fields[label])});

            if (fields.length > 0) {
                const is_tuple = fields.every(v => isNaN(v.label));
                row = (is_tuple ? fields[0].label + "=" + fields[0].value : fields[0].value);
                fields = fields.slice(1, fields.length);
                for (let i = 0; i < fields.length; i++)
                    row += "," + (is_tuple ? fields[i].label + "=" + fields[i].value : fields[i].value);
            }
            return "(" + row + ")";
        default:
            throw "Cannot convert " + JSON.stringify(val) + " to string.";
        }
    }
    return {
        'toString': toString
    }
}());

const CEK = (function() {
    /* Machine modes */
    const MODE = {
            'TAIL_COMPUTATION': 0,
            'COMPUTATION': 1,
            'APPLY_CONTINUATION': 2
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
                let v = Env.unsafeLookup(env, val.name) || Env.unsafeLookup(funs, val.name);

                if (v === undefined) {
                    if (primitives[val.name] !== undefined) {
                        v = IR.Make.primitive(val.name);
                    } else throw ("Unknown variable " + val.name);
                }

                //dump(v);
                result = v;
                break;
            }

            case IR.INJECT: {
                const args = value(env, val.args);
                result = IR.Make.variant(val.label, args);
                break;
            }

            case IR.EXTEND: {
                const record = val.record === undefined ? {} : value(env, val.record);
                for (let field in val.fields) {
                    record[field] = value(env, val.fields[field]);
                }
                result = IR.Make.record(record);
                break;
            }

            case IR.RECORD: {
                result = val;
                break;
            }

            case IR.PROJECT: {
                const record = value(env, val.record);
                result = record.fields[val.label];
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
            //dump(mode);
            //dump(environment);
            switch (mode) {

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
                    const args = control.args.map(v => value(environment, v));
                    //dump(fn);
                    let env = environment;
                    switch (IR.match(fn)) {
                    case IR.PRIMITIVE: {
                        if (primitives[fn.name] !== undefined) {
                            //                            dump(args);
                            const v = primitives[fn.name](environment, args);

                            // Set up for continuation application
                            mode = MODE.APPLY_CONTINUATION;
                            control = v;
                        } else {
                            throw ("Unknown primitive function " + fn.name);
                        }
                        continue;
                        break; }
                    case IR.CLOSURE:
                        const fvs = value(environment, fn.fvs);
                        env = Env.extend(env, fn.venv, fvs);
                        // FALL THROUGH
                    case IR.FUNCTION:
                    // print("APPLY"); dump(fn);
                    // dump(control.args);
                    // dump(environment);
                    //dump(control);
                    //dump(fn);
                    // Bind arguments
                        for (let i = 0; i < args.length; i++)
                            env = Env.extend(env, fn.params[i], args[i]);

                        mode = MODE.COMPUTATION;
                        control = fn.body;
                        environment = env;
                        continue;
                        break;

                    case IR.RESUMPTION: {
                        // Alpha renaming
                        const resume = fn;
                        let kappa = kontinuation;

                        // Restore the continuation stack accordingly
                        if (resume.depth === "deep") {
                            kappa = K.compose(resume.kappa, kappa);
                        } else { // shallow
                            kappa = K.prependFrames(resume.frames);
                            kappa = K.compose(resume.kappa, kappa);
                        }

                        // Prepare to invoke the resumption
                        mode = MODE.APPLY_CONTINUATION;
                        control = args[0]; // resumptions are unary
                        kontinuation = kappa;
                        continue;
                        break;
                    }

                    default:
                        throw ("Unknown function object " + JSON.stringify(fn));
                    }
                }

                case IR.IFTHENELSE: {
                    const cond = value(environment, control.cond);
                    // dump(cond);
                    const branch = cond.value === true ? control.tt : control.ff;
                    //print("BRANCH");  dump(branch); dump(control.ff);
                    // Prepare to evaluate branch
                    mode = MODE.COMPUTATION;
                    control = branch;
                    continue;
                    break; }

                case IR.CASESPLIT: {
                    // Evaluate the scrutinee
                    const scrutinee = value(environment, control.scrutinee);
                    // Lookup corresponding clause...
                    const clause = Clause.lookup(control.clauses, scrutinee.label);
                    // ... or fail
                    if (clause === undefined) {
                        throw ("Pattern matching failure on " + scrutinee.label);
                    }
                    // Bind arguments
                    const env = Env.extend(environment, clause.binders, scrutinee.args);

                    // Prepare to evaluate body
                    mode = MODE.COMPUTATION;
                    control = clause.body;
                    environment = env;
                    continue;
                    break;
                }

                case IR.DO: {
                    // Interpret arguments
                    const args = control.args.map(v => value(environment, v));

                    // Look up corresponding handler
                    const { env, body, kappa } = K.trap(kontinuation, control.label, args);

                    // Prepare to invoke handler
                    mode = MODE.COMPUTATION;
                    control = body;
                    environment = env;
                    kontinuation = kappa;
                    continue;
                    break;
                }

                case IR.HANDLE: {
                    // Install handler
                    const handle = control;
                    const comp = handle.comp;
                    const handler = K.Handler.make(environment, handle.retClause, handle.opClauses, handle.depth);
                    const kappa = K.setTrap(kontinuation, handler);

                    // Prepare to evaluate the computation
                    mode = MODE.COMPUTATION;
                    control = comp;
                    kontinuation = kappa;
                    continue;
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
            case MODE.APPLY_CONTINUATION: { /* Continuation application interpretation */
                switch (K.match(kontinuation)) {
                case K.EMPTY: return Value.toString(control);
                case K.RETURN: { // Invoke handler
                    const { handler, kappa } = K.popHandler(kontinuation);

                    // if (handler.tag === K.Handler.IDENTITY) {
                    //     kontinuation = kappa;
                    //     mode = MODE.COMPUTATION;
                    //     control = IR.Make.computation([], IR.Make.ret(control));
                    // } else {
                        // Prepare to evaluate the body of the return clause
                    const b = handler['return'].binders[0];
                    //print("Binding " + JSON.stringify(b) + " -> " + JSON.stringify(control));
                    //dump(handler['return'].binders);
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
                    // print("Binding " + frame.binder + " := " + JSON.stringify(control));
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
let factorial = IR.Make.fn("fact", ["n"], undefined,
                           IR.Make.computation([IR.Make.binding("b", IR.Make.apply(IR.Make.variable("%eq"), [IR.Make.variable("n"), IR.Make.constant(0)]))],
                                                IR.Make.ifthenelse(IR.Make.variable("b"),
                                                                   IR.Make.computation([], IR.Make.ret(IR.Make.constant(1))),
                                                                   IR.Make.computation([IR.Make.binding("n1", IR.Make.apply(IR.Make.variable("%sub"), [IR.Make.variable("n"), IR.Make.constant(1)])),
                                                                                        IR.Make.binding("m", IR.Make.apply(IR.Make.variable("fact"), [IR.Make.variable("n1")]))
                                                                                       ],
                                                                                       IR.Make.apply(IR.Make.variable("%mult"), [IR.Make.variable("n"), IR.Make.variable("m")])))));

let count = IR.Make.fn("count", ["n"], undefined,
                       IR.Make.computation([IR.Make.binding("b", IR.Make.apply(IR.Make.variable("%leq"), [IR.Make.variable("n"), IR.Make.constant(0)]))],
                                           IR.Make.ifthenelse(IR.Make.variable("b"),
                                                              IR.Make.computation([], IR.Make.ret(IR.Make.variable("n"))),
                                                              IR.Make.computation([IR.Make.binding("m", IR.Make.apply(IR.Make.variable("%sub"), [IR.Make.variable("n"), IR.Make.constant(1)]))],
                                                                                  IR.Make.apply(IR.Make.variable("count"), [IR.Make.variable("m")])))));

//let program = IR.Make.computation(bindings, IR.Make.apply(IR.Make.variable("%print"), [IR.Make.variable("foo")]));
//let program = IR.Make.computation([], IR.Make.apply(IR.Make.variable("fact"), [IR.Make.constant(20)]));
//let program = IR.Make.computation([], IR.Make.apply(IR.Make.variable("count"), [IR.Make.constant(10)]));
let clauses =
    {'Foo': IR.Make.clause("n",
                           IR.Make.computation([IR.Make.binding("v", IR.Make.ret(IR.Make.inject("Baz", IR.Make.tuple([IR.Make.constant(1), IR.Make.constant(2), IR.Make.constant(3)])))),
                                                IR.Make.binding("m", IR.Make.ret(IR.Make.project(IR.Make.variable("n"), 1))),
                                                IR.Make.binding("_", IR.Make.apply(IR.Make.variable("%print"), [IR.Make.variable("m")]))],
                                               IR.Make.ret(IR.Make.variable("v")))),
     'Bar': IR.Make.clause([], IR.Make.computation([], IR.Make.apply(IR.Make.variable("%print"), [IR.Make.constant("BAR")])))
    };
//let program = IR.Make.computation([], IR.Make.casesplit(IR.Make.inject('Foo', IR.Make.tuple([IR.Make.constant(42), IR.Make.constant(30)])), clauses));
let program = IR.Make.computation([], IR.Make.ret(IR.Make.record({'a':IR.Make.constant(42),'b':IR.Make.constant(43),'c':IR.Make.constant(44)})));
//let program = IR.Make.computation([], IR.Make.ret(IR.Make.tuple([IR.Make.constant(42),IR.Make.constant(43),IR.Make.constant(44)])));
/*let program = IR.Make.computation([IR.Make.binding("r", IR.Make.ret(IR.Make.extend({'a':IR.Make.constant(1),'b':IR.Make.constant(2)}, undefined)))],
                                  IR.Make.ret(IR.Make.variable("r")));*/
let result  = CEK.run({'fact': factorial, 'count': count}, program);
print(JSON.stringify(result));
