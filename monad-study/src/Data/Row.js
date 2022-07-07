"use strict";

export const unsafeGet = function (s) {
  return function (o) {
    return o[s];
  };
};

export const unsafeSet = function(s) {
  return function(a) {
    return function (o) {
      var o1 = {};
      o1[s] = a;
      return Object.assign({}, o, o1);
    };
  };
};
