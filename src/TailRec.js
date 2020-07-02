exports.setTimeout = function(duration) {
  return function(cb) {
    return function() {
      return setTimeout(cb, duration);
    };
  };
};

exports.cancelTimeout = function(tid) {
  return function() {
    cancelTimeout(tid);
  };
}
