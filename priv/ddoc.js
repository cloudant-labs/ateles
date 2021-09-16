
let isArray = Array.isArray;

const jsonizeParams = (funStr, ddocStr, argsStr) => {
    const ddoc = JSON.parse(ddocStr);
    const args = JSON.parse(argsStr);
    const fun = eval(JSON.parse(funStr))

    return [ddoc, fun, args];
}

function validate_doc_update(funStr, ddocStr, argsStr) {
    try {
        const [ddoc, fun, args] = jsonizeParams(funStr, ddocStr, argsStr);
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

function filter_view(funStr, ddocStr, docsStr) {
    try {
        let viewEmit = false;
        // this emit will override the emit in map.js
        // so we create it in this function so that it is only available
        // in scope. This is also why we have to do the eval in this function
        // so that the emit is included in the function conversion
        const emit = (_key, _value) => {
            viewEmit = true;
        }
        const ddoc = JSON.parse(ddocStr);
        const docs = JSON.parse(docsStr);
        const fun = eval(JSON.parse(funStr));

        const results = docs.map(doc => {
            viewEmit = false;
            fun.apply(ddoc, [doc]);
            return viewEmit;
        });

        return [true, results];
    } catch (error) {
        if (error.name && error.message) {
            return {
                compilation_error: error.toString()
            }
        }
        return error;
    }
}

function filter_docs(funStr, ddocStr, argsStr) {
    try {
        const [ddoc, fun, args] = jsonizeParams(funStr, ddocStr, argsStr);
        const [req, docs] = args;
        const results = docs.map(doc => {
            return !!fun.apply(ddoc, [doc, req]);
        })
        return [true, results];
    } catch (error) {
        if (error.name && error.message) {
            return {
                compilation_error: error.toString()
            }
        }
        return error;
    }
}