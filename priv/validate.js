
let isArray = Array.isArray;

function validate(funStr, ddocStr, argsStr) {
    try {
        const ddoc = JSON.parse(ddocStr);
        const args = JSON.parse(argsStr);
        const fun = eval(JSON.parse(funStr))
        fun.apply(ddoc, args);
        return true;
    } catch (error) {
        if (error.name && error.message) {
            return {
                compilation_error: error.toString()
            }
        }
        return error;
    }
}