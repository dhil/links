/* Generalised Stack Inspection runtime */

/* IO operations */
const _IO = (function() {
    return {
        'print': function(msg) {
            _print(msg);
            return {};
        },
        'error': function(msg) {
            throw msg;
        },
        'debug': function(msg) {
            print("[DEBUG] " + msg);
            return {};
        }
    }
}());
