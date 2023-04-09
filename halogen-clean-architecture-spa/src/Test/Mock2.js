export const store = function(v) {
  let results = [];
  let push = function(result) {
    results.push(result);
  }
  return {
    results,
    store: push
  }
}
