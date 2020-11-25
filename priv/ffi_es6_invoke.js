"use strict";

exports.do_run_erlang = function(fun) {
    return function(args) {
        if (fun.constructor.name === 'GeneratorFunction') {
            // Ok we have a generator :)
            var it = fun(args);
            var r = it.next();
            if (r.done) return r.value;
            throw Error("Generator would block :(")
        } else {
            return fun(args);
        }
    }
};
