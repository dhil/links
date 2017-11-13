/* Generator/Iterator runtime */

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

/* Toplevel handler */
const _Toplevel = (function() {
    return {
        'run': function(f) {
            const result = f().next();
            if (result.done) {
                return result.value;
            } else {
                throw ("Unhandled: " + result.value.op);
            }
        }
    }
}());
