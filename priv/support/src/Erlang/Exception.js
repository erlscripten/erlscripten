"use strict";

exports.raise = function(ex) {
    throw ex;
};

exports.getStack = function() {
    return new Error().stack;
};


let tryCatch =
    function(exprC) {
        return function(handler) {
            try {
                let result = exprC();
                return result;
            } catch(error) {
                //console.log("gggg")
                //console.log(typeof(error))
                let resultEr = handler(error);
                return resultEr;
            }
        };
    };
exports.tryCatch = tryCatch;

let tryOfCatch =
    function(exprC) {
        return function(ofHandler) {
            return function(handler) {
                var computed;
                try { computed = exprC(); }
                catch(error) {
                    //console.log("gggg")
                    //console.log(typeof(error))
                    let resultEr = handler(error);
                    return resultEr;
                }
                let result = ofHandler(computed);
                return result;
            };
        };
    };
exports.tryOfCatch = tryOfCatch;


exports.tryCatchFinally =
    function(exprC) {
        return function(handler) {
            return function(afterC) {
                try {
                    let result = tryCatch(exprC)(handler);
                    return result;
                } finally { afterC(); }
            };
        };
    };

exports.tryOfCatchFinally =
    function(exprC) {
        return function(ofHandler) {
            return function(handler) {
                return function(afterC) {
                    var computed;
                    try {
                        let result = tryOfCatch(exprC)(ofHandler)(handler);
                        return result;
                    } finally { afterC(); }
                };
            };
        };
    };
