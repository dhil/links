/** Primitive CPS bindings **/
function _makeCont(k) {
    return _lsCons(k, _lsSingleton(_efferr));
}
var _idy = _makeCont(function(x, ks) { return; });
var _idk = function(x,ks) { };
function _applyCont(x, ks) {
    var k = _lsHead(ks);
    var ks = _lsTail(ks);

    return k(arg, ks);
}
function is_continuation(kappa) {
    return kappa !== null && typeof kappa === 'object' && _lsHead(kappa) !== undefined && _lsTail(kappa) !== undefined;
}
