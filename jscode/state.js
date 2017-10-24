/* Program generated... */
'use strict';


/* [Begin] Include /home/dhil/projects/links/my-links/lib/js/base.js */
/* Taken from https://github.com/v8/v8/blob/master/test/js-perf-test/base.js
   Performance.now is used in latency benchmarks, the fallback is Date.now. */
var _performance = performance || {};
_performance.now = (function() {
  return _performance.now       ||
         _performance.mozNow    ||
         _performance.msNow     ||
         _performance.oNow      ||
         _performance.webkitNow ||
         Date.now;
})();

const _now = _performance.now;

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

/* Console */
const __print = console.log || debug;

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
            let zs = xs;
            while (zs !== nil && tail(zs) !== nil) { zs = tail(zs); }
            return cons(zs, ys);
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
        }
    };
}());
/* [End] Include /home/dhil/projects/links/my-links/lib/js/base.js */
/* [Begin] Include /home/dhil/projects/links/my-links/lib/js/geniter.js */
/* Generator/Iterator runtime */

/* IO operations */
const _IO = (function() {
    return {
        'print': function(msg) {
            __print(msg);
            return {};
        },
        'error': function(msg) {
            throw msg;
        },
        'debug': function(msg) {
            __print("[DEBUG] " + msg);
            return {};
        }
    }
}());

/* Toplevel handler */
const _Toplevel = (function() {
    return {
        'run': function(f) {
            const result = f().next();
            if (result.done) {
                return result.value;
            } else {
                print(result.value);
                throw ("Unhandled: " + result.value._label);
            }
        }
    }
}());
/* [End] Include /home/dhil/projects/links/my-links/lib/js/geniter.js */

/* [Begin] Bindings */
function* concatMap_251(_249, _250) {
  const l_247 = _250;
  const f_248 = _249;
  const _254 = l_247;
  if (_254 === _List.nil) {
    return _List.nil;
  } else {
    const _255 = _List.head(_254);
    const _256 = _List.tail(_254);
    const tl_253 = _256;
    const hd_252 = _255;
    const _258 = yield* f_248(hd_252);
    const _257 = yield* concatMap_251(f_248, tl_253);
    return _List.concat(_258, _257);
  }
}
function* rev_aux_264(_262, _263) {
  const acc_260 = _263;
  const xs_261 = _262;
  const _267 = xs_261;
  if (_267 === _List.nil) {
    return acc_260;
  } else {
    const _268 = _List.head(_267);
    const _269 = _List.tail(_267);
    const xs_266 = _269;
    const x_265 = _268;
    return yield* rev_aux_264(xs_266, _List.cons(x_265, acc_260));
  }
}

function* reverse_271(_270) {
  const xs_259 = _270;
  return yield* rev_aux_264(xs_259, _List.nil);
}

function* ignore_273(_272) {
  return {};
}

function* assert_287(_283, _284, _285, _286) {
  const actual_274 = _286;
  const expected_275 = _285;
  const toString_276 = _284;
  const eq_277 = _283;
  const _281 = yield* eq_277(expected_275, actual_274);
  const _282 = yield* function*() {
    if (!_281) {
        const _279 = yield* toString_276(expected_275);
        const _278 = yield* toString_276(actual_274);
        const _280 = _IO.error(_String.concat("Assertion error: ", _String.concat(_279, _String.concat(" != ", _278))));
        return _280;
      } else {
        return false;
      }
  }();
  return yield* ignore_273(_282);
}

function* _fun__g1_293(env_293_348, _292) {
  const x_290 = _292;
  const _291 = env_293_348['289'](x_290);
  if (_291) {
    return _List.cons(x_290, _List.nil);
  } else {
    return _List.nil;
  }
}

function* filter_296(_294, _295) {
  const l_288 = _295;
  const p_289 = _294;
  return yield* concatMap_251(_Closure.apply(_fun__g1_293, { '289': p_289 }), l_288);
}

function* _fun__g2_311(env_311_349, _310) {
  const y_307 = _310;
  const _309 = env_311_349['298'](y_307);
  const _308 = env_311_349['298'](env_311_349['302']);
  return _309 < _308;
}

function* _fun__g3_317(env_317_350, _316) {
  const y_313 = _316;
  const _315 = env_317_350['298'](y_313);
  const _314 = env_317_350['298'](env_317_350['302']);
  return _315 >= _314;
}

function* sortBy_301(_299, _300) {
  const l_297 = _300;
  const f_298 = _299;
  const _304 = l_297;
  if (_304 === _List.nil) {
    return _List.nil;
  } else {
    const _305 = _List.head(_304);
    const _306 = _List.tail(_304);
    const xs_303 = _306;
    const x_302 = _305;
    const lt_312 = yield* filter_296(_Closure.apply(_fun__g2_311, { '302': x_302, '298': f_298 }), xs_303);
    const ge_318 = yield* filter_296(_Closure.apply(_fun__g3_317, { '302': x_302, '298': f_298 }), xs_303);
    const _320 = yield* sortBy_301(f_298, lt_312);
    const _319 = yield* sortBy_301(f_298, ge_318);
    return _List.concat(_320, _List.concat(_List.cons(x_302, _List.nil), _319));
  }
}

function* sortByBase_325(_323, _324) {
  const l_321 = _324;
  const f_322 = _323;
  return yield* sortBy_301(f_322, l_321);
}

function* id_328(_327) {
  const x_326 = _327;
  return x_326;
}

function* map_aux_337(_334, _335, _336) {
  const acc_331 = _336;
  const xs_332 = _335;
  const f_333 = _334;
  const _340 = xs_332;
  if (_340 === _List.nil) {
    return acc_331;
  } else {
    const _341 = _List.head(_340);
    const _342 = _List.tail(_340);
    const xs_339 = _342;
    const x_338 = _341;
    const _343 = yield* f_333(x_338);
    return yield* map_aux_337(f_333, xs_339, _List.cons(_343, acc_331));
  }
}

function* map_347(_345, _346) {
  const l_329 = _346;
  const f_330 = _345;
  const _344 = yield* map_aux_337(f_330, l_329, _List.nil);
  return yield* reverse_271(_344);
}

const x_351 = 42;
function* main_357() {
  function* _fun__g4_355(_354) {
    const x_352 = _354;
      const a = yield { '_label': "Get", '_value': { '1': {} } };
      print(a);
      const b = a + 1;
      yield { '_label': "Put", '_value': { '1': b } };
      const c = yield { '_label': "Get", '_value': { '1': {} } };
      print(c);
      return c;
  }

  const xs_356 = yield* map_347(_fun__g4_355, _List.cons(1, _List.cons(2, _List.cons(3, _List.nil))));
  return _IO.print(_String.ofNumber(_List.length(xs_356)));
}

/* [End] Bindings */

function* main_357() {
  function* _fun__g4_355(_354) {
    const x_352 = _354;
      const a = yield { '_label': "Get", '_value': { '1': {} } };
      print(a);
      const b = a + 1;
      yield { '_label': "Put", '_value': { '1': b } };
      const c = yield { '_label': "Get", '_value': { '1': {} } };
      print(c);
      return c;
  }

  const xs_356 = yield* map_347(_fun__g4_355, _List.cons(1, _List.cons(2, _List.cons(3, _List.nil))));
  return _IO.print(_String.ofNumber(_List.length(xs_356)));
}

function* evalState(f) {
    const m = yield* f();

    function* handle(result) {
        if (result.done === true) {
            return function* (st) { return result.value; };
        } else {
            switch (result.value._label) {
            case 'Get': {
                const resume = function*(y) {
                    return yield* handle(m.next(y))
                };
                return function*(st) {
                    const f = yield* resume(st);
                    return yield* f(st)
                };
                break;
            }
            case 'Put': {
                const arg = result.value._value['1'];
                const resume = function*(x) {
                    return yield* handle(m.next(x));
                };
                return function*(y) {
                    const f = yield* resume({});
                    return yield* f(arg);
                };
                break;
            }
            default: {
                var x = yield result.value;
                return yield* handle(m.next(x));
            }
            }
        }
    }

    return yield* handle(m.next());
}

_Toplevel.run(function*() {
    const f = yield* evalState(main_357);
    return yield* f(42);
});
