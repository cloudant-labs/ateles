let lib = {};
let map_funs = [];
let results = [];

function emit(key, value) {
    results.push([key, value]);
}

const mapFunRunner = (doc, mapFn) => {
    try {
        results = [];
        mapFn(doc);
        return results;
    } catch (ex) {
        return { error: ex.toString() };
    }
};

function mapDoc(doc_str) {
    const doc = JSON.parse(doc_str);
    const mapResults = Array.from(mapFuns, ([id, mapFun]) => {
        const mapResult = { id };

        const result = mapFunRunner(doc, mapFun);
        if (result.error) {
            mapResult.error = result.error;
        } else {
            mapResult.result = result;
        }

        return mapResult;
    });

    return JSON.stringify(mapResults);
}

function init(lib, mapFuns) {

}