"use strict";

exports.throwImpl = function(type) {
    return function(term) {
        throw [new Error(type), term];
    };
};

exports.tryCatchFinallyImpl =
    function(buildError) {
        return function(exprC) {
            return function(handler) {
                return function(afterC) {
                    try {
                        let result = exprC()();
                        return function() { return result; };
                    } catch(error) {
                        let built = buildError(error[0].message)(error[1])(error[0].stack);
                        let result = handler(built)();
                        return function() { return result; };
                    } finally { afterC()(); }
                };
            };
        };
    };

exports.tryOfCatchFinallyImpl =
    function(buildError) {
        return function(exprC) {
            return function(ofHandler) {
                return function(handler) {
                    return function(afterC) {
                        var computed;
                        try {
                            try { computed = exprC()(); }
                            catch(error) {
                                let built = buildError(error[0].message)(error[1])(error[0].stack);
                                let resultEr = handler(built)();
                                return function() { return resultEr; };
                            }
                            let result = ofHandler(computed)();
                            return result;
                        } finally { afterC()(); }
                    };
                };
            };
        };
    };

