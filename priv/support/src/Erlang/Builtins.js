"use strict";

exports.do_apply_4 =
    function(moduleName) {
        return function(functionName) {
            return function(argumentArray) {
                return function(failCallback) {
                    var module = undefined;
                    var f = undefined;
                    try {
                        let name = moduleName.replace(/\w\S*/g, function(txt){return txt.charAt(0).toUpperCase() + txt.substr(1).toLowerCase();});
                        try {
                            module = PS[name];
                        } catch(e) {
                            module = require("../"+name+"/index.js");
                        }
                        f = module["erlps__" + functionName + "__" + argumentArray.length]
                    } catch(e) {
                        module = undefined;
                        f = undefined;
                    }
                    if (module !== undefined && f !== undefined) {
                        return f()(argumentArray);
                    } else {
                        failCallback()
                    }
                }
            }
        }
    };
