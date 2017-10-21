/* CPS runtime */
const _IO = (function() {
    return {
        'print': function*(msg) {
            __print(msg);
            return {};
        },
        'error': function*(msg) {
            throw msg;
        },
        'debug': function*(msg) {
            __print("[DEBUG] " + msg);
            return {};
        }
    }
}());
