function sysYield(f, kappa) {
    return setTimeout(function() {
        return _applyCont(_makeCont(f), kappa);
    }, 0);
}
