export const store = function(v) {
  const argsList = [];
  const push = function(args) {
    argsList.push(args);
  }
  return {
    argsList,
    store: push
  }
}
