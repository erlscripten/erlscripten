"use strict";

// https://github.com/elixirscript/erlang-types/blob/master/src/erlang-types/bit_string.ts
exports.float32ToArray = function(f) {
    var bytes = [];

    var buf = new ArrayBuffer(4);
    new Float32Array(buf)[0] = f;

    let intVersion = new Uint32Array(buf)[0];

    bytes.push((intVersion >> 24) & 0xff);
    bytes.push((intVersion >> 16) & 0xff);
    bytes.push((intVersion >> 8) & 0xff);
    bytes.push(intVersion & 0xff);

    return bytes;
};

exports.float64ToArray = function(f) {
    var bytes = [];

    var buf = new ArrayBuffer(8);
    new Float64Array(buf)[0] = f;

    var intVersion1 = new Uint32Array(buf)[0];
    var intVersion2 = new Uint32Array(buf)[1];

    bytes.push((intVersion2 >> 24) & 0xff);
    bytes.push((intVersion2 >> 16) & 0xff);
    bytes.push((intVersion2 >> 8) & 0xff);
    bytes.push(intVersion2 & 0xff);

    bytes.push((intVersion1 >> 24) & 0xff);
    bytes.push((intVersion1 >> 16) & 0xff);
    bytes.push((intVersion1 >> 8) & 0xff);
    bytes.push(intVersion1 & 0xff);

    return bytes;
};

exports.arrayToFloat32 = function(arr) {
    return Buffer.from(arr).readFloatBE(0);
};

exports.arrayToFloat64 = function(arr) {
    return Buffer.from(arr).readDoubleBE(0);
};
