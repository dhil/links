/* jshint globalstrict: true */
/* jshint esversion:6 */
"use strict";

// focus stuff
let _focused = null;
function _focus() {
  if (_focused) {
    let y = document.getElementById(_focused);
    if (y) {
      y.focus();
    }
  }
  return;
}

// Page update

//  _replaceDocument(tree)
//    Replace the current page with `tree'.
function _replaceDocument(tree) {
  _$Debug.assert(function() { return tree != null; }, "No argument given to _replaceDocument");
  const firstChild = _$List.head(tree);
  _$Debug.assert(function() { return firstChild != null; }, "Null tree passed to _replaceDocument");
  _$Debug.assert(function() {
      return firstChild.type === "ELEMENT";
  }, "New document value was not an XML element (it was non-XML or was an XML text node).");
  tree = _$Links.XmlToDomNodes(tree);

  // save here
  const _saved_fieldvals = [];
  const inputFields = document.getElementsByTagName("input");
  for (let i = 0; i < inputFields.length; i++) {
     let current = inputFields[i];
     if (current.id != null && current.id != "") { // only store fields with an id!
       _saved_fieldvals.push({'field' : current.id, 'value' : current.value});
     }
  }

  // delete the DOM except for the html tag and the body
  // (IE) IE doesn't allow these tags to be deleted
  let d = document.documentElement;
  let body;
  while (d.hasChildNodes()) {
    if (isElementWithTag(d.firstChild, 'body')) {
      body = d.firstChild;
      let bodyLength = body.childNodes.length;
      while (body.hasChildNodes()) {
        body.removeChild(body.firstChild);
      }
      break; // (IE) no more nodes allowed after the body
    }
    d.removeChild(d.firstChild);
  }

  // insert new dom nodes
  for (let p = tree[0].firstChild; p != null; p = p.nextSibling) {
    if (isElementWithTag(p, 'body')) {
     // insert body nodes inside the existing body node
      for (let q = p.firstChild; q != null; q = q.nextSibling) {
        let it = q.cloneNode(true);
        body.appendChild(it);
        _$Links.activateHandlers(it);
      }
      break; // (IE) no more nodes allowed after the body
    }
    let it = p.cloneNode(true);
    d.insertBefore(it, body);
    _$Links.activateHandlers(it);
  }

  // (IE) hack to activate a style element in IE
  _activateStyleElement();

  // restore here
  for (let i = 0; i < _saved_fieldvals.length; i++) {
     let current = _saved_fieldvals[i];
     let elem = document.getElementById(current.field);
     if (elem) {
        elem.value = current.value;
     }
  }

  _focus();

  return _$Constants.UNIT;
}

function _startRealPage() {
  const parsedState = JSON.parse(_jsonState);
  let state = _$Links.resolveJsonState(parsedState);
  _initVars(state); // resolve JSONized values for toplevel let bindings received from the server
  _$Links.activateJsonState(state, parsedState); // register event handlers + spawn processes
  _$Links.activateHandlers(_getDocumentNode());
  // Create a websocket connection
  return _$Websocket.connect_if_required(parsedState);
}

// generate a fresh key for each node
let _node_key = 0;
function _get_fresh_node_key() {
  return _node_key++;
}

let _eventHandlers = {};

// SL: I think this function is no longer used
function _registerFormEventHandlers(actions) {
   const key = '_key' + _get_fresh_node_key();

  for (let i = 0; i < actions.length; i++) {
    const action = actions[i];
    // FIXME: Shouldn't we need to clone the actions[i] objs?

    //_$Debug.debug("adding " + action.evName + " to object " + key);
    if (!_eventHandlers[key]) _eventHandlers[key] = [];
    _eventHandlers[key][action.evName] = _$Proc.Sched.wrapEventHandler(action.handler);
  }

  return key; // returns the ID to give to the elt
}

//
//
// LINKS GAME LIBRARY
//
//

function _jsSetInterval(fn, interval) {
  window.setInterval(function () { return fn(_$K.idy); }, interval);
  return;
}
function jsSetInterval(fn, interval, kappa) {
  _jsSetInterval(fn, interval);
  return _$K.apply(kappa,_$Constants.UNIT);
}

// NOTE: requestAnimationFrame can also take a callback that has one argument
function _jsRequestAnimationFrame(fn) {
  window.requestAnimationFrame(function () { return fn(_$K.idy); });
  return;
}
function jsRequestAnimationFrame(fn, kappa) {
  _jsRequestAnimationFrame(fn);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsSave(ctx) {
  ctx.save();
  return;
}
function jsSave(ctx, kappa) {
  _jsSave(ctx);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsRestore(ctx) {
  ctx.restore();
  return;
}
function jsRestore(ctx, kappa) {
  _jsRestore(ctx);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsSetOnKeyDown(node, fn) {
  // note: node has to exist in the document, otherwise we get a JavaScript error
  node.addEventListener('keydown', function(e) { return fn(e, _$K.idy); }, true);
  return;
}
function jsSetOnKeyDown(node, fn, kappa) {
  _jsSetOnKeyDown(node, fn);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsSetOnEvent(node, event, fn, capture) {
  node.addEventListener(event, function(e) { return fn(e, _$K.idy); }, capture);
  return;
}
function jsSetOnEvent(node, event, fn, capture, kappa) {
  _jsSetOnEvent(node, event, fn, capture);
  return _$K.apply(kappa, _$Constants.UNIT);
}

function _jsSetWindowEvent(event, fn, capture) {
  window.addEventListener(event, function(e) { return fn(e, _$K.idy); }, capture);
  return _$Constants.UNIT;
}

const jsSetWindowEvent = _$Links.kify(_jsSetWindowEvent);

function _jsSetOnLoad(fn) {
  window.addEventListener('load', function(e) { return fn(e, _$K.idy); }, false);
  return;
}
function jsSetOnLoad(fn, kappa) {
  _jsSetOnEvent(fn);
  return _$K.apply(kappa,_$Constants.UNIT);
}

const globalObjects = {};

function _jsSaveGlobalObject(name, obj) {
  globalObjects[name] = obj;
  return;
}
function jsSaveGlobalObject(name, obj, kappa) {
  _jsSaveGlobalObject(name, obj);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsLoadGlobalObject(name) {
  return globalObjects[name];
}
function jsLoadGlobalObject(name, kappa) {
  _jsSaveGlobalObject(name);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsGetContext2D(node) {
  return node.getContext('2d');
}
function jsGetContext2D(node, kappa) {
  _jsGetContext2D(node);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsFillText(ctx, text, x, y) {
  return ctx.fillText(text, x, y);
}
function jsFillText(ctx, text, x, y, kappa) {
  _jsFillText(ctx, text, x, y);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsCanvasFont(ctx, font) {
  ctx.font = font;
  return;
}
function jsCanvasFont(ctx, font, kappa) {
  _jsCanvasFont(ctx, font);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsDrawImage(ctx, node, x, y) {
  return ctx.drawImage(node, x, y);
}
function jsDrawImage(ctx, node, x, y, kappa) {
  _jsDrawImage(ctx, node, x, y);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsFillRect(ctx, x, y, width, height) {
  return ctx.fillRect(x, y, width, height);
}
function jsFillRect(ctx, x, y, width, height, kappa) {
  _jsFillRect(ctx, x, y, width, height);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsFillCircle(ctx, x, y, radius) {
  ctx.beginPath();
  ctx.arc(x, y, radius, 0, 2 * Math.PI, true);
  ctx.fill();
  return ctx.closePath();
}
function jsFillCircle(ctx, x, y, radius, kappa) {
  _jsFillCircle(ctx, x, y, radius);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsFill(ctx) {
  return ctx.fill();
}
const jsFill = _jsFill;

function _jsBeginPath(ctx) {
  return ctx.beginPath();
}
function jsBeginPath(ctx, kappa) {
  _jsBeginPath(ctx);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsClosePath(ctx) {
  return ctx.closePath();
}
const jsClosePath = _jsClosePath;

function _jsArc(ctx, x, y, radius, startAngle, endAngle, clockwise) {
  return ctx.arc(x, y, radius, startAngle, endAngle, clockwise);
}
const jsArc = _jsArc;

function _jsStrokeStyle(ctx, style) {
  ctx.strokeStyle = style;
  return;
}
function jsStrokeStyle(ctx, style, kappa) {
  _jsStrokeStyle(ctx, style);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsStroke(ctx) {
  return ctx.stroke();
}
function jsStroke(ctx, kappa) {
  _jsStroke(ctx);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsMoveTo(ctx, x, y) {
  return ctx.moveTo(x, y);
}
function jsMoveTo(ctx, x, y, kappa) {
  _jsMoveTo(ctx, x, y);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsLineTo(ctx, x, y) {
  return ctx.lineTo(x, y);
}
function jsLineTo(ctx, x, y, kappa) {
  _jsLineTo(ctx, x, y);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsLineWidth(ctx, width) {
  ctx.lineWidth = width;
  return;
}
let jsLineWidth = _jsLineWidth;

function _jsScale(ctx, x, y) {
  return ctx.scale(x, y);
}
function jsScale(ctx, x, y, kappa) {
  _jsScale(ctx, x, y);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsTranslate(ctx, x, y) {
  return ctx.translate(x, y);
}
function jsTranslate(ctx, x, y, kappa) {
  _jsTranslate(ctx, x, y);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsSetFillColor(ctx, color) {
  ctx.fillStyle = color;
  return;
}
function jsSetFillColor(ctx, color, kappa) {
  _jsSetFillColor(ctx, color);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsClearRect(ctx, x, y, width, height) {
  return ctx.clearRect(x, y, width, height);
}
function jsClearRect(ctx, x, y, width, height, kappa) {
  _jsClearRect(ctx, x, y, width, height);
  return _$K.apply(kappa,_$Constants.UNIT);
}

function _jsCanvasWidth(ctx) {
  return ctx.canvas.width;
}
const jsCanvasWidth = _jsCanvasWidth;

function _jsCanvasHeight(ctx) {
  return ctx.canvas.height;
}
const jsCanvasHeight = _jsCanvasHeight;

function _jsSaveCanvas(canvas, node, mime) {
  const imageData = canvas.toDataURL(mime);//.replace("image/png", "image/octet-stream");;
  node.href = imageData; // window.location.
  return;
}
const jsSaveCanvas = _jsSaveCanvas;
