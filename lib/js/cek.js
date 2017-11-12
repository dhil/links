/* CEK runtime */
'use strict';
load("immutable.js");

var print = print || console.log;

/* Immutable environments */
const EnvOrg = (function() {
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

const Env = (function() {
    const empty = Immutable.Map();

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
            if (Immutable.Map.isMap(env) === false) {
                print(JSON.stringify(env));
                throw ("Env is not a map");
            }
            //print(env);
            return env.set(k, v);
        },
        'lookup': function(env, k) {
            const v = env.get(k);
            if (v === undefined) throw ("[Env] Lookup failure: " + JSON.stringify(k) + " is not present.");
            else return v;
        },
        'unsafeLookup': function(env, k) {
            // print(JSON.stringify(env));
            return env.get(k);
        },
        'shadow': function(env0, /* by */ env1) {
            return env0.merge(env1);
        },
        'of': function(key_value_pairs) {
            const env = empty.withMutations(function(map) {
                for (let p in key_value_pairs) {
                    //print("Setting " + p + " ==> " + key_value_pairs[p]);
                    map.set(p, key_value_pairs[p]);
                }
            });
            return env;
        }
    };
}());

let env0 = Env.empty;
let env1 = Env.extend(env0, "foo", 42);
let env2 = Env.extend(env1, "bar", -42);
let env3 = Env.extend(env1, "foo", 0);

/* Immutable lists */
const OrgList = (function() {
    const NIL = 0;
    const CONS = 1;

    function toString(xs) {
        return JSON.stringify(xs);
    }

    function fromString(stringified_list) {
        return JSON.parse(stringified_list);
    }

    function copy(xs) {
        return fromString(toString(xs));
    }

    const nil = [];

    function cons(x, xs) {
        const ys = copy(xs);
        return [x].concat(ys);
    }

    function head(xs) {
        return xs[0];
    }

    function tail(xs) {
        const ys = copy(xs);
        return ys.slice(1, xs.length);
    }

    function rev(xs) {
        const ys = copy(xs);
        return ys.reverse();
    }

    function append(xs, ys) {
        return xs.concat(ys);
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
        'rev': rev,
        'append': append
    };
}());

const List = (function() {
    const NIL = 0;
    const CONS = 1;

    function toString(xs) {
        return JSON.stringify(xs);
    }

    function fromString(stringified_list) {
        return Immutable.List(JSON.parse(stringified_list));
    }

    const nil = Immutable.List();

    function cons(x, xs) {
        return xs.unshift(x);
    }

    function head(xs) {
        const v = xs.first();
        if (v === undefined) throw "List.hd error";
        return v;
    }

    function tail(xs) {
        return xs.shift();
    }

    function rev(xs) {
        return xs.reverse();
    }

    function append(xs, ys) {
        return xs.concat(ys);
    }

    return {
        'nil': nil,
        'cons': cons,
        'hd': head,
        'tl': tail,
        'NIL': NIL,
        'CONS': CONS,
        'match': function(xs) {
            return xs.size === 0 ? NIL : CONS;
        },
        'fromJSList': function(xs) {
            return Immutable.List(xs);
        },
        'map': function(f, xs) {
            // Sigh... every operation on JS lists mutates its list argument, expect for map...
            return xs.map(f);
        },
        'rev': rev,
        'append': append,
        'of': function(xs) {
            return Immutable.List(xs);
        }
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

/* Abstract syntax */
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
    const PROJECT     = 20; const PRIMITIVE_FUN    = 21;
    const RESUMPTION  = 22; const DEEP             = 23;
    const SHALLOW     = 24; const LIST             = 25;
    const PRIMITIVE_CONSTANT = 26; const CLOSURE   = 27;


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
        'HANDLE': HANDLE,
        'DO': DO,
        'RETURN': RETURN,
        'FUNCTION': FUNCTION,
        'INJECT': INJECT,
        'VARIANT': VARIANT,
        'EXTEND': EXTEND,
        'RECORD': RECORD,
        'PROJECT': PROJECT,
        'PRIMITIVE_FUN': PRIMITIVE_FUN,
        'PRIMITIVE_CONSTANT': PRIMITIVE_CONSTANT,
        'RESUMPTION': RESUMPTION,
        'DEEP': DEEP,
        'SHALLOW': SHALLOW,
        'LIST': LIST,
        'CLOSURE': CLOSURE,
        'Make': {
            'computation': function(bindings, tail_comp) {
                return {'tag': COMPUTATION, 'bindings': List.of(bindings), 'tail_comp': tail_comp};
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
            'casesplit': function(scrutinee, clauses, catchall = undefined) {
                return {'tag': CASESPLIT, 'scrutinee': scrutinee, 'clauses': clauses, 'default': catchall};
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
                if (depth === DEEP)
                    kappa = List.rev(List.cons({'handler': handler, 'frames': frames}, stack));
                else
                    kappa = {'frames': frames, 'continuation': List.rev(stack)};
                return {'tag': RESUMPTION, 'depth': depth, 'kappa': kappa};
            },
            'doOperation': function(label, args) {
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
            'value': function(tag=undefined, value) {
                return {'tag': tag, 'value': value};
            },
            'unit': record({}, undefined),
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
            'primitiveFun': function(name) {
                return {'tag': PRIMITIVE_FUN, 'name': name};
            },
            'list': function(head, tail) {
                return {'tag': LIST, '_head': head, '_tail': tail};
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
            return {'tag': IDENTITY, 'env': Env.empty, 'return': IR.Make.clause([x], IR.Make.computation([], IR.Make.ret(IR.Make.variable(x)))), 'clauses': {}, 'depth': IR.DEEP};
        }());

        return {
            'IDENTITY': IDENTITY,
            'USER_DEFINED': USER_DEFINED,
            'make': function(env, retClause, opClauses, depth) {
                if (Immutable.Map.isMap(env) === false) {
                    print("NOT MAP");
                }
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
                // Grab the closest enclosing handler for inspection
                const top = List.hd(k);
                // Remove the handler (the topmost impure continuation)
                k = List.tl(k);
                // Test whether it handles `label'...
                if (top.handler.clauses[label] !== undefined) {
                    handler = top.handler;
                    frames  = top.frames;
                    found = true;
                    break;
                }

                // ... if it does not handle the operation then add
                // the handler to the reversed resumption stack
                stack = List.cons(top, stack);
            }

            // Check whether we found a suitable handler
            if (!found) throw ("No suitable handler for " + label + " is installed.");

            // Bind arguments
            const binders = handler.clauses[label].binders;
            let henv = binders !== undefined ? Env.extend(handler.env, binders, args) : handler.env;
            // Bind resumption
            const resume = handler.clauses[label].resumption;
            henv = resume !== undefined ? Env.extend(henv, resume, IR.Make.resumption(handler.depth, handler, frames, stack)) : henv;

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
                if (Immutable.Map.isMap(top.handler.env) === false) print("NOT MAP " + JSON.stringify(top.handler.env));
                return {'handler': top.handler, 'frames': top.frames, 'kappa': List.tl(kappa)};
            }
        }
    };
})();

const Primitive = (function() {
    return {
        'Function': {
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
            },
            /* Lists */
            '%List.cons': function(env, args) {
                return IR.Make.list(args[0], args[1]);
            },
            '%List.hd': function(env, args) {
                if (args[0]._head !== undefined) throw "List.hd error.";
                else return args[0]._head;
            },
            '%List.tl': function(env, args) {
                if (args[0]._tail !== undefined) throw "List.tl error.";
                else return args[0]._tail;
            },
            '%List.concat': function(env, args) {
                const xs = args[0];
                let zs = xs;
                while (zs.head !== undefined) { zs = zs._tail; }
                zs._tail = args[1];
                return xs;
            },
            '%List.length': function(env, args) {
                let xs = args[0];
                let n = 0;
                while (xs._tail !== null) { n++; }
                return IR.Make.constant(n);
            }
        },
        'Constant': {
            '%List.nil': {'tag': IR.LIST}
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
            const args = toString(val.args);
            if (args === "()")
                return ("" + val.label);
            else
                return ("" + val.label + args);
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
        case IR.LIST:
            if (val._head === undefined)
                return "[]";
            else {
                let xs = toString(val._head);
                val = val._tail;
                while (val._head !== undefined) {
                    xs = xs + ", " + toString(val._head);
                    val = val._tail;
                }
                return "[" + xs + "]"
            }
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
                    if (Primitive.Function[val.name] !== undefined) {
                        v = IR.Make.primitiveFun(val.name);
                    } else if (Primitive.Constant[val.name] !== undefined) {
                        v = Primitive.Constant[val.name];
                    } else throw ("Unknown variable " + val.name);
                }

                //dump(v);
                result = v;
                break;
            }

            case IR.CLOSURE: {
                const fn = Env.lookup(funs, val.fn);
                const fvs = value(env, val.fvs);
                result = {'tag': IR.CLOSURE, 'name': fn.name, 'params': fn.params, 'body': fn.body, 'venv': fn.venv, 'fvs': fvs};
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

            case IR.RECORD: { /* Dead code */
                result = val;
                break;
            }

            case IR.PROJECT: {
                const record = value(env, val.record);
                result = record.fields[val.label];
                break;
            }

            case IR.LIST: {
                throw "List interpretation";
                break;
            }

            default:
                throw ("Value interpretation (" + IR.match(val) + "): " + (JSON.stringify(val)));
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
                    //dump(environment);
                    // Transform
                    // dump(environment);
                    // dump(control.args);
                    const fn = value(environment, control.fn);
                    const args = control.args.map(v => value(environment, v));
                    let env = environment;
                    switch (IR.match(fn)) {
                    case IR.PRIMITIVE_FUN: {
                        if (Primitive.Function[fn.name] !== undefined) {
                            //                            dump(args);
                            const v = Primitive.Function[fn.name](environment, args);

                            // Set up for continuation application
                            mode = MODE.APPLY_CONTINUATION;
                            control = v;
                        } else {
                            throw ("Unknown primitive function " + fn.name);
                        }
                        continue;
                        break; }
                    case IR.CLOSURE:
                        //print("APPLYING CLOSURE");
                        //const fvs = value(environment, fn.fvs);
                        //print("fvs name: " + JSON.stringify(fn.venv));
                        env = Env.extend(env, fn.venv, fn.fvs);
                        // FALL THROUGH
                    case IR.FUNCTION:
                        //print("APPLY");
                        //dump(fn.name);
                        //dump(control.args);
                    // dump(environment);
                    //dump(control);
                    //dump(fn);
                    // Bind arguments
                        for (let i = 0; i < args.length; i++)
                            env = Env.extend(env, fn.params[i], args[i]);

                        //dump(fn);
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
                        if (resume.depth === IR.DEEP) {
                            kappa = K.compose(resume.kappa, kappa);
                        } else { // shallow
                            kappa = K.prependFrames(resume.kappa.frames, kappa); // <---
                            kappa = K.compose(resume.kappa.continuation, kappa);
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
                    let comp = undefined;
                    let env  = environment;
                    // ... determine whether there is a match
                    if (clause !== undefined) {
                        // Bind arguments
                        env = Env.extend(env, clause.binders, scrutinee.args);
                        comp = clause.body;
                    } else if (clause === undefined && control['default'] !== undefined) {
                        env = Env.extend(env, control['default'].binders, scrutinee);
                        comp = control['default'].body;
                    } else {
                        throw ("Pattern matching failure on " + scrutinee.label);
                    }

                    // Prepare to evaluate body
                    mode = MODE.COMPUTATION;
                    control = comp;
                    environment = env;
                    continue;
                    break;
                }

                case IR.DO: {
                    // Interpret arguments
                    const args = value(environment, control.args);

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
                    const handler = K.Handler.make(environment, handle['return'], handle.operations, handle.depth);
                    const kappa = K.setTrap(kontinuation, handler);

                    // Prepare to evaluate the computation
                    mode = MODE.COMPUTATION;
                    control = comp;
                    kontinuation = kappa;
                    continue;
                    break;
                }

                default: {
                    dump(control);
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
                case K.EMPTY: return control;
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
                    //print("Binding " + frame.binder + " := " + JSON.stringify(control));
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

        return IR.Make.unit;
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

let count0 = IR.Make.fn("count", ["n"], undefined,
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
//let program = IR.Make.computation([], IR.Make.casesplit(IR.Make.inject('Foo', IR.Make.tuple([IR.Make.constant(42), IR.Make.constant(30)])), clauses, undefined));
//let program = IR.Make.computation([], IR.Make.ret(IR.Make.record({'a':IR.Make.constant(42),'b':IR.Make.constant(43),'c':IR.Make.constant(44)})));
//let program = IR.Make.computation([], IR.Make.ret(IR.Make.tuple([IR.Make.constant(42),IR.Make.constant(43),IR.Make.constant(44)])));
/*let program = IR.Make.computation([IR.Make.binding("r", IR.Make.ret(IR.Make.extend({'a':IR.Make.constant(1),'b':IR.Make.constant(2)}, undefined)))],
  IR.Make.ret(IR.Make.variable("r")));*/

let identity_handler = function(comp) {
    return IR.Make.handle(comp,
                          IR.Make.clause("x", IR.Make.computation([], IR.Make.ret(IR.Make.variable("x")))),
                          {},
                          IR.DEEP);
}

//let program = IR.Make.computation([], identity_handler(IR.Make.computation([], IR.Make.ret(IR.Make.constant(42)))));

let choose_handler = function(comp) {
    return IR.Make.handle(comp,
                          IR.Make.clause("x", IR.Make.computation([], IR.Make.ret(IR.Make.variable("x")))),
                          {'Choose': IR.Make.opclause(undefined, "resume",
                                                      IR.Make.computation([], IR.Make.apply(IR.Make.variable("resume"), [IR.Make.constant(true)])))
                          },
                          IR.DEEP);
}

//let program = IR.Make.computation([], choose_handler(IR.Make.computation([], IR.Make.doOperation("Choose", IR.Make.unit))));

let choice_comp = IR.Make.fn("choice", [], undefined,
                             IR.Make.computation([IR.Make.binding("c1", IR.Make.doOperation("Choose", IR.Make.unit)),
                                                  IR.Make.binding("_", IR.Make.apply(IR.Make.variable("%print"), [IR.Make.variable("c1")])),
                                                  IR.Make.binding("c2", IR.Make.doOperation("Choose", IR.Make.unit))],
                                                 IR.Make.apply(IR.Make.variable("%print"), [IR.Make.variable("c2")])));
//let program = IR.Make.computation([], choose_handler(IR.Make.computation([], IR.Make.apply(IR.Make.variable("choice"), []))));

let toss_comp = IR.Make.fn("toss", [], undefined,
                           IR.Make.computation([IR.Make.binding("c", IR.Make.doOperation("Choose", IR.Make.unit))],
                                               IR.Make.ifthenelse(IR.Make.variable("c"),
                                                                  IR.Make.computation([], IR.Make.ret(IR.Make.inject("Heads", IR.Make.unit))),
                                                                  IR.Make.computation([], IR.Make.ret(IR.Make.inject("Tails", IR.Make.unit))))));

let all_choices_handler = function(comp) {
    return IR.Make.handle(comp,
                          IR.Make.clause("x",
                                         IR.Make.computation([IR.Make.binding("singleton", IR.Make.apply(IR.Make.variable("%List.cons"), [IR.Make.variable("x"), IR.Make.variable("%List.nil")]))],
                                                             IR.Make.ret(IR.Make.variable("singleton")))),
                          {'Choose': IR.Make.opclause(undefined, "resume",
                                                      IR.Make.computation([IR.Make.binding("xs", IR.Make.apply(IR.Make.variable("resume"), [IR.Make.constant(true)])),
                                                                           IR.Make.binding("ys", IR.Make.apply(IR.Make.variable("resume"), [IR.Make.constant(false)]))],
                                                                          IR.Make.apply(IR.Make.variable("%List.concat"), [IR.Make.variable("xs"), IR.Make.variable("ys")])))
                          },
                          IR.DEEP);
}

//let program = IR.Make.computation([], all_choices_handler(IR.Make.computation([], IR.Make.apply(IR.Make.variable("toss"), []))));

let count_eff0 = IR.Make.fn("count_eff", [], undefined,
                           IR.Make.computation([IR.Make.binding("n", IR.Make.doOperation("Get", IR.Make.unit)),
                                                IR.Make.binding("b", IR.Make.apply(IR.Make.variable("%leq"), [IR.Make.variable("n"), IR.Make.constant(0)]))],
                                               IR.Make.ifthenelse(IR.Make.variable("b"),
                                                              IR.Make.computation([], IR.Make.ret(IR.Make.variable("n"))),
                                                                  IR.Make.computation([IR.Make.binding("m", IR.Make.apply(IR.Make.variable("%sub"), [IR.Make.variable("n"), IR.Make.constant(1)])),
                                                                                       IR.Make.binding("_", IR.Make.doOperation("Put", IR.Make.variable("m")))],
                                                                                      IR.Make.apply(IR.Make.variable("count_eff"), [])))));

let shallow_state_get_fn0 =
    IR.Make.fn("shallow_state_get_fn", [], "e",
               IR.Make.computation([IR.Make.binding("resume", IR.Make.ret(IR.Make.project(IR.Make.variable("e"), "resume"))),
                                    IR.Make.binding("s", IR.Make.ret(IR.Make.project(IR.Make.variable("e"), "st")))],
                                   IR.Make.apply(IR.Make.variable("resume"), [IR.Make.variable("s")])));

let shallow_state_put_fn0 =
    IR.Make.fn("shallow_state_get_fn", [], "e",
               IR.Make.computation([IR.Make.binding("resume", IR.Make.ret(IR.Make.project(IR.Make.variable("e"), "resume")))],
                                   IR.Make.apply(IR.Make.variable("resume"), [IR.Make.unit])));

let shallow_state_handler0 =
    IR.Make.fn("state", ["st", "comp"], undefined,
               IR.Make.computation([],
                                   IR.Make.handle(IR.Make.computation([], IR.Make.apply(IR.Make.variable("comp"), [])),
                                                  IR.Make.clause("x",
                                                                 IR.Make.computation([], IR.Make.ret(IR.Make.variable("x")))),
                                                  {'Get': IR.Make.opclause(undefined, "resume",
                                                                           IR.Make.computation([IR.Make.binding("thunk1", IR.Make.ret(IR.Make.closure("shallow_state_get_fn", IR.Make.extend({'st':IR.Make.variable("st"), 'resume': IR.Make.variable("resume")}))))],
                                                                                               IR.Make.apply(IR.Make.variable("state"), [IR.Make.variable("st"), IR.Make.variable("thunk1")]))),
                                                   'Put': IR.Make.opclause("p", "resume",
                                                                           IR.Make.computation([IR.Make.binding("thunk2", IR.Make.ret(IR.Make.closure("shallow_state_put_fn", IR.Make.extend({'resume': IR.Make.variable("resume")}))))],
                                                                                               IR.Make.apply(IR.Make.variable("state"), [IR.Make.variable("p"), IR.Make.variable("thunk2")])))},
                                                  IR.SHALLOW)));

let program = IR.Make.computation([], IR.Make.apply(IR.Make.variable("state"), [IR.Make.constant(100), IR.Make.variable("count_eff")]));

// Function definitions
let funs = Env.of({'fact': factorial, 'count': count0, 'choice':choice_comp, 'toss':toss_comp,
                   'count_eff': count_eff0, 'shallow_state_get_fn': shallow_state_get_fn0, 'shallow_state_put_fn': shallow_state_put_fn0, 'state': shallow_state_handler0});

let result  = Value.toString(CEK.run(funs, program));
print(result);
