"use strict";

const path = require("path");

function loadNative() {
  const candidates = [
    path.join(__dirname, "index.node"),
    path.join(__dirname, "ffi.node"),
    path.join(__dirname, "dist", "index.node"),
    path.join(__dirname, "dist", "ffi.node"),
  ];

  let lastErr;
  for (const p of candidates) {
    try {
      // eslint-disable-next-line import/no-dynamic-require, global-require
      return require(p);
    } catch (err) {
      lastErr = err;
    }
  }

  const tried = candidates.map((p) => `- ${p}`).join("\n");
  const msg = `Failed to load native addon. Tried:\n${tried}\n\nLast error: ${lastErr}`;
  const e = new Error(msg);
  e.cause = lastErr;
  throw e;
}

const native = loadNative();

function authorize(request) {
  const json = typeof request === "string" ? request : JSON.stringify(request);
  const res = native.authorize(json);
  return typeof res === "string" ? JSON.parse(res) : res;
}

module.exports = {
  authorize,
  _native: native,
};
