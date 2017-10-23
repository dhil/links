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
                throw ("Unhandled: " + result.value.op);
            }
        }
    }
}());
