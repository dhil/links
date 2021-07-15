class Yield {
    constructor(state, obj, k) {
        this.state = state;
        this.obj = obj;
        this.k = k;
    }
}

let resolveDepth = 0;

function resolveServerValue(state, obj) {
    let k = function(x) { return x; };
    while (true) {
        try {
            return resolveServerValueCPS(state, obj, k);
        } catch (e) {
            if (e instanceof Yield) {
                state = e.state;
                obj   = e.obj;
                k     = e.k;
            } else {
                throw e;
            }
        }
    }
}

function resolveServerValueCPS(state, obj, k) {
    resolveDepth++;
    if (resolveDepth >= 5) {
        resolveDepth = 0;
        // console.log("Yielding obj: " + JSON.stringify(obj) + ", k: " + k);
        throw new Yield(state, obj, k);
    }

    if (obj._tag == null && obj.key != null) {
        obj.key = _lookupMobileKey(state, obj.key)
        return k(obj);
    }
    // console.log("Obj: " + JSON.stringify(obj));
    switch (obj._tag) {
    case "Bool":
    case "Float":
    case "Int":
    case "String":
    case "XML":
    case "Database":
    case "Table":
        return k(obj._value);
    case "Char":
    case "ClientDomRef":
    case "ClientAccessPoint":
    case "ServerAccessPoint":
    case "ClientPid":
    case "ServerPid":
    case "SessionChannel":
    case "ServerSpawnLoc":
    case "ClientSpawnLoc":
        delete obj._tag;
        return k(obj);
    case "List":
        if (obj._head == null && obj._tail == null) return k({});
        return resolveServerValueCPS(state, obj._head, function(head) {
            return resolveServerValueCPS(state, obj._tail,
                                         function(tail) {
                                             return k({"_head": head, "_tail": tail})
                                         })
        });
    case "Record":
        delete obj._tag;
        var chain = function () { return k(obj) };
        function createChain(obj, i, k) {
            return function() {
                return resolveServerValueCPS(state, obj[i], function(ith) {
                    obj[i] = ith;
                    // console.log(k + '');
                    return k();
                });
            };
        }
        for (let i in obj) {
            chain = createChain(obj, i, chain);
        }
        return chain();
        // return resolveServerValueCPS(state, obj[1], function(fst) {
        //     obj[1] = fst;
        //     return resolveServerValueCPS(state, obj[2], function(snd) {
        //         obj[2] = snd;
        //         return k(obj);
        //     });
        // });
    case "Variant":
        delete obj._tag;
        return resolveServerValueCPS(state, obj._value, function(value) {
            return k({'_label': obj._label, '_value': value});
        });
    case "FunctionPtr":
    case "ClientFunction":
        if (obj.environment == null) {
            let f = eval(obj.func);
            // console.log(f + "");
            f.location = obj.location;
            f.func = obj.func;
            return k(f);
        } else {
            console.log(obj.environment);
            return resolveServerValueCPS(state, obj.environment, function(env) {
                // console.log("env: " + obj.environment + ", func: " + obj.func);
                let f = eval(obj.func)(env);
                // console.log(f + "");
                f.location = obj.location;
                f.func = obj.func;
                return k(f);
            });
        }
    case "Alien":
        return k(eval(obj._value));
    case "Process":
        delete obj._tag;
        return resolveServerValue(state, obj.process, function(process) {
            obj.process = process;
            function createChain(messages, i, k) {
                return function() {
                    return resolveServerValueCPS(state, messages[i], function(ith) {
                        messages[i] = ith;
                        return k(messages);
                    });
                };
            }

            var chain = function(messages) {
                obj.messages = messages;
                return k(obj);
            };

            for (let i in obj) {
                chain = createChain(obj, i, chain);
            }
            return chain();
        });
    default:
        throw "Untagged server value \"" + JSON.stringify(obj) + "\"";
    }
}

// function resolveServerValue(state, obj) {
//     let worklist = [obj];
//     let instrlist = [];
//     let valuelist = [];
//     while (worklist.length > 0) {
//         // console.log("Worklist: " + worklist.map(JSON.stringify));
//         let o = worklist.pop();
//         // console.log("Working on " + JSON.stringify(o));
//         let tag = o._tag;
//         delete o._tag;
//         switch (tag) {
//         case "Bool":
//         case "Float":
//         case "Int":
//         case "String":
//         case "XML":
//         case "Database":
//         case "Table":
//             instrlist.push(tag);
//             valuelist.push(o._value);
//             continue;
//         case "Char":
//         case "ClientDomRef":
//         case "ClientAccessPoint":
//         case "ServerAccessPoint":
//         case "ClientPid":
//         case "ServerPid":
//         case "SessionChannel":
//         case "ServerSpawnLoc":
//         case "ClientSpawnLoc":
//             instrlist.push(tag);
//             valuelist.push(o);
//             continue;
//         case "List":
//             // console.log("List: " + JSON.stringify(o));
//             if (o._head == null && o._tail == null)  {
//                 instrlist.push("Nil");
//                 valuelist.push({});
//             } else {
//                 instrlist.push(tag);
//                 // console.log("Pushing " + JSON.stringify(o._head));
//                 worklist.push(o._head);
//                 worklist.push(o._tail);
//             }
//             continue;
//         case "Record":
//             instrlist.push(Object.keys(o));
//             instrlist.push(tag);
//             let constituents = [];
//             for (let k in o)
//                 constituents.push(o[k]);
//             for (let i = constituents.length - 1; 0 <= i; i--)
//                 worklist.push(constituents[i]);
//             continue;
//         case "Variant":
//             instrlist.push(o._label);
//             instrlist.push(tag);
//             worklist.push(o._value);
//             continue;
//         case "FunctionPtr":
//         case "ClientFunction":
//             instrlist.push(tag);
//             const f = (!TYPES.isObject(o.environment)) ?
//                   eval(o.func) :
//                   partialApply(eval(o.func), eval(resolveServerValue(state, o.environment)));
//             f.location = o.location; // This may be set to 'server' by the server serializer.
//             f.func = o.func;
//             valuelist.push(f);
//             continue;
//         case "Alien":
//             instrlist.push(tag);
//             valuelist.push(o._value);
//         case "Process":
//             instrlist.push(tag);
//             instrlist.push(Object.keys(obj.messages));
//             worklist.push(o.process);
//             for (let k in obj.messages) {
//                 worklist.push(obj.messages[k]);
//             }
//             continue;
//         default:
//             throw "Untagged server value \"" + JSON.stringify(obj) + "\"";
//         }
//     }

//     let valuelist2 = [];
//     while (instrlist.length > 0) {
//         console.log("Instrlist: " + JSON.stringify(instrlist));
//         console.log("Valuelist: " + JSON.stringify(valuelist));
//         console.log("Valuelist2: " + JSON.stringify(valuelist2));
//         let instr = instrlist.pop();
//         switch (instr) {
//         case "Bool":
//         case "Float":
//         case "Int":
//         case "String":
//         case "XML":
//         case "Database":
//         case "Table":
//         case "Char":
//         case "ClientDomRef":
//         case "ClientAccessPoint":
//         case "ServerAccessPoint":
//         case "ClientPid":
//         case "ServerPid":
//         case "SessionChannel":
//         case "ServerSpawnLoc":
//         case "ClientSpawnLoc":
//         case "FunctionPtr":
//         case "ClientFunction":
//         case "Alien":
//         case "Nil":
//             valuelist2.push(valuelist.pop());
//             continue;
//         case "List":
//             let tail = valuelist2.pop();
//             let head = valuelist2.pop();
//             valuelist2.push({"_head": head, "_tail": tail});
//             continue;
//         case "Record":
//             let keys = instrlist.pop();
//             let values = [];
//             for (let i = 0; i < keys.length; i++)
//                 values.push(valuelist2.pop());
//             /* Zip keys and values */
//             // console.log("keys: " + JSON.stringify(keys));
//             // console.log("values: " + JSON.stringify(values));
//             let record = {};
//             for (let i = 0; i < keys.length; i++) {
//                 record[keys[i]] = values[i];
//             }
//             valuelist2.push(record);
//             continue;
//         case "Variant":
//             let label = instrlist.pop();
//             let value = valuelist2.pop();
//             valuelist2.push({"_label": label, "_value": value});
//             continue;
//         case "Process":
//             stk.push(o._tag);
//             delete o._tag;
//             stk.push(Object.keys(obj.messages));
//             worklist.push(o.process);
//             for (let k in obj.messages) {
//                 worklist.push(obj.messages[k]);
//             }
//             continue;
//         default:
//             throw "Unrecognised instruction \"" + JSON.stringify(instr) + "\"";
//         }
//     }
//     return valuelist2;
// }

// function test() {
//     // var record = {"_tag": "Record", "foo": {"_tag": "Record", "1":{"_tag": "Bool", "_value": true}, "2":{"_tag": "Int", "_value": 42}}, "bar": {"_tag": "String", "_value": "quux"}};
//     var bazVariant = {"_tag": "Variant", "_label": "Baz", "_value": {"_tag": "List"}};
//     var quuxVariant = {"_tag": "Variant", "_label": "Quux", "_value": {"_tag": "List", "_head": {"_tag": "Int", "_value": 99}, "_tail": {"_tag": "List"}}};
//     var pair = {"_tag": "Record", "1": bazVariant, "2": quuxVariant};
//     var list = {"_tag": "List", "_head": {"_tag": "Int", "_value": 1}, "_tail": {"_tag": "List", "_head": pair, "_tail": {"_tag": "List", "_head": {"_tag": "Int", "_value": 3}, "_tail": {"_tag": "List"}}}};
//     var record = {"_tag": "Record", "foo": {"_tag": "Variant", "_label": "Bar", "_value": {"_tag": "String", "_value": "fooBar"}}
//                                   , "bar": {"_tag": "Record", "1":{"_tag": "Bool", "_value": true}
//                                                             , "2": list
//                                                             , "3":{"_tag": "Int", "_value": 42}}
//                                   , "baz": {"_tag": "Float", "_value": 3.14}};
//     console.log(JSON.stringify(record));
//     return resolveServerValue(null, record);
// }

// console.log(JSON.stringify(test()));

function foobar() { return "foobar"; }
function quux(env) { return function() { return "inner" + env[1] } }

function testCPS() {
    // var bazVariant = {"_tag": "Variant", "_label": "Baz", "_value": {"_tag": "List"}};
    // var quuxVariant = {"_tag": "Variant", "_label": "Quux", "_value": {"_tag": "List", "_head": {"_tag": "Int", "_value": 99}, "_tail": {"_tag": "List"}}};
    // var pair = {"_tag": "Record", "1": bazVariant, "2": quuxVariant};
    // var list = {"_tag": "List", "_head": {"_tag": "Int", "_value": 1}, "_tail": {"_tag": "List", "_head": {"_tag": "Int", "_value": 2}, "_tail": {"_tag": "List", "_head": {"_tag": "Int", "_value": 3}, "_tail": {"_tag": "List"}}}};
    // var list2 = {"_tag": "List", "_head": {"_tag": "Int", "_value": 1}, "_tail": {"_tag": "List", "_head": quuxVariant, "_tail": {"_tag": "List", "_head": {"_tag": "Int", "_value": 3}, "_tail": {"_tag": "List"}}}};

    var func = {"_tag": "FunctionPtr", "func": "quux", "location": "unknown", "environment": {"_tag": "Record", "1": {"_tag": "Int", "_value": 42}}};
    var bazVariant = {"_tag": "Variant", "_label": "Baz", "_value": func};
    var quuxVariant = {"_tag": "Variant", "_label": "Quux", "_value": {"_tag": "List", "_head": {"_tag": "Int", "_value": 99}, "_tail": {"_tag": "List"}}};
    var pair = {"_tag": "Record", "1": bazVariant, "2": quuxVariant};
    var list = {"_tag": "List", "_head": {"_tag": "Int", "_value": 1}, "_tail": {"_tag": "List", "_head": pair, "_tail": {"_tag": "List", "_head": {"_tag": "Int", "_value": 3}, "_tail": {"_tag": "List"}}}};
    var record = {"_tag": "Record", "foo": {"_tag": "Variant", "_label": "Bar", "_value": {"_tag": "String", "_value": "fooBar"}}
                                  , "bar": {"_tag": "Record", "1":{"_tag": "Bool", "_value": true}
                                                            , "2": list
                                                            , "3":{"_tag": "Int", "_value": 42}}
                                  , "baz": {"_tag": "Float", "_value": 3.14}};

    var obj = record;
    console.log(JSON.stringify(obj));
    return resolveServerValue(null, obj);
}

var result = testCPS();
// console.log(JSON.stringify(result));
console.log(result.bar[2]._tail._head[1]._value());
