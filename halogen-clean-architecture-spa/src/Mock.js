export const store = function(v) {
  let args = [];
  let push = function(arg) {
    args.push(arg);
  }
  return {
    args,
    save: push
  }
}
