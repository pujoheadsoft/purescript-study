/* eslint-disable no-constant-condition */
/* eslint-disable no-redeclare */
/* globals setImmediate, clearImmediate, setTimeout, clearTimeout */
/* eslint-disable no-unused-vars, no-prototype-builtins, no-use-before-define, no-unused-labels, no-param-reassign */
var Aff = function () {
  // A unique value for empty.
  var EMPTY = {};

  /*

  An awkward approximation. We elide evidence we would otherwise need in PS for
  efficiency sake.

  data Aff eff a
    = Pure a
    | Throw Error
    | Catch (Aff eff a) (Error -> Aff eff a)
    | Sync (Eff eff a)
    | Async ((Either Error a -> Eff eff Unit) -> Eff eff (Canceler eff))
    | forall b. Bind (Aff eff b) (b -> Aff eff a)
    | forall b. Bracket (Aff eff b) (BracketConditions eff b) (b -> Aff eff a)
    | forall b. Fork Boolean (Aff eff b) ?(Fiber eff b -> a)
    | Sequential (ParAff aff a)

  */
  var PURE    = "Pure";
  var SYNC    = "Sync";
  var ASYNC   = "Async";
  var BIND    = "Bind";
  var FORK    = "Fork";

  // Various constructors used in interpretation
  var CONS      = "Cons";      // Cons-list, for stacks

  function Aff(tag, _1, _2, _3) {
    this.tag = tag;
    this._1  = _1;
    this._2  = _2;
    this._3  = _3;
  }

  function AffCtr(tag) {
    var fn = function (_1, _2, _3) {
      return new Aff(tag, _1, _2, _3);
    };
    fn.tag = tag;
    return fn;
  }

  function runEff(eff) {
    eff();
  }

  function runSync(eff) {
    return eff();
  }

  function runAsync(eff, k) {
    return eff(k)();
  }

  var Scheduler = function () {
    var limit    = 1024;
    var size     = 0;
    var ix       = 0;
    var queue    = new Array(limit);
    var draining = false;

    function drain() {
      var thunk;
      draining = true;
      while (size !== 0) {
        size--;
        thunk     = queue[ix];
        queue[ix] = void 0;
        ix        = (ix + 1) % limit;
        thunk();
      }
      draining = false;
    }

    return {
      isDraining: function () {
        return draining;
      },
      enqueue: function (cb) {
        var i, tmp;
        if (size === limit) {
          tmp = draining;
          drain();
          draining = tmp;
        }

        queue[(ix + size) % limit] = cb;
        console.log("enqueue", queue)
        size++;

        if (!draining) {
          drain();
        }
      }
    };
  }();


  // Fiber state machine
  var SUSPENDED   = 0; // Suspended, pending a join.
  var CONTINUE    = 1; // Interpret the next instruction.
  var STEP_BIND   = 2; // Apply the next bind.
  var STEP_RESULT = 3; // Handle potential failure from a result.
  var PENDING     = 4; // An async effect is running.
  var RETURN      = 5; // The current stack has returned.
  var COMPLETED   = 6; // The entire fiber has completed.

  function Fiber(aff) {
    console.log("myaff", aff)
    // Monotonically increasing tick, increased on each asynchronous turn.
    var runTick = 0;

    // The current branch of the state machine.
    var status = SUSPENDED;

    // The current point of interest for the state machine branch.
    var step      = aff;  // Successful step

    // Stack of continuations for the current fiber.
    var bhead = null;
    var btail = null;
    
    // Each join gets a new id so they can be revoked.
    var joinId  = 0;
    var joins   = null;

    function run(localRunTick) {
      var tmp, result;
      while (true) {
        tmp       = null;
        result    = null;

        switch (status) {
        case STEP_BIND:
          status = CONTINUE;
          step   = bhead(step);
          if (btail === null) {
            bhead = null;
          } else {
            bhead = btail._1;
            btail = btail._2;
          }
          break;

        case STEP_RESULT:
          if (bhead === null) {
            status = RETURN;
          } else {
            status = STEP_BIND;
          }
          break;

        case CONTINUE:
          switch (step.tag) {
          case BIND:
            if (bhead) {
              btail = new Aff(CONS, bhead, btail);
            }
            bhead  = step._2;
            status = CONTINUE;
            step   = step._1;
            break;

          case PURE:
            if (bhead === null) {
              status = RETURN;
              step = step._1;
            } else {
              status = STEP_BIND;
              step   = step._1;
            }
            break;

          case SYNC:
            status = STEP_RESULT;
            step   = runSync(step._1);
            break;

          case ASYNC:
            status = PENDING;
            // step._1 は joinFiber のときは次の関数
            //   \k -> effectCanceler <$> t.join k
            // kはrunAsyncの引数の関数(function (result) {....})である。
            // この関数の結果が次の step になり return される
            step   = runAsync(step._1, function (result) {
              console.log("result", result)
              return function () {
                if (runTick !== localRunTick) {
                  return;
                }
                runTick++;
                Scheduler.enqueue(function () {
                  if (runTick !== localRunTick + 1) {
                    return;
                  }
                  status = STEP_RESULT;
                  step   = result;
                  run(runTick);
                });
              };
            });
            return;

          case FORK:
            status = STEP_RESULT;
            tmp    = Fiber(step._2);
            if (step._1) {
              tmp.run();
            }
            step = tmp;
            break;
          }
          break;

        case RETURN:
          bhead = null;
          btail = null;
          status = COMPLETED;
          break;

        case COMPLETED:
          for (var k in joins) {
            if (joins.hasOwnProperty(k)) {
              runEff(joins[k].handler(step));
            }
          }
          joins = null;
          return;
        case SUSPENDED:
          status = CONTINUE;
          break;
        case PENDING: return;
        }
      }
    }

    function onComplete(join) {
      return function () {
        if (status === COMPLETED) {
          join.handler(step)();
          return function () {};
        }

        var jid    = joinId++;
        joins      = joins || {};
        joins[jid] = join;
      };
    }

    function join(cb) {
      return function () {
        onComplete({ handler: cb })();
        if (status === SUSPENDED) {
          run(runTick);
        }
        return;
      };
    }

    return {
      join: join,
      onComplete: onComplete,
      isSuspended: function () {
        return status === SUSPENDED;
      },
      run: function () {
        if (status === SUSPENDED) {
          if (!Scheduler.isDraining()) {
            Scheduler.enqueue(function () {
              run(runTick);
            });
          } else {
            run(runTick);
          }
        }
      }
    };
  }


  Aff.EMPTY       = EMPTY;
  Aff.Pure        = AffCtr(PURE);
  Aff.Sync        = AffCtr(SYNC);
  Aff.Async       = AffCtr(ASYNC);
  Aff.Bind        = AffCtr(BIND);
  Aff.Fork        = AffCtr(FORK);
  Aff.Fiber       = Fiber;
  Aff.Scheduler   = Scheduler;

  return Aff;
}();

export const _pure = Aff.Pure;

export function _map(f) {
  return function (aff) {
    if (aff.tag === Aff.Pure.tag) {
      return Aff.Pure(f(aff._1));
    } else {
      return Aff.Bind(aff, function (value) {
        return Aff.Pure(f(value));
      });
    }
  };
}

export function _bind(aff) {
  return function (k) {
    return Aff.Bind(aff, k);
  };
}

export function _fork(immediate) {
  return function (aff) {
    return Aff.Fork(immediate, aff);
  };
}

export const _liftEffect = Aff.Sync;

export const makeAff = Aff.Async;

export function _makeFiber(aff) {
  return function () {
    return Aff.Fiber(aff);
  };
}

export const _delay = function () {
  function setDelay(n, k) {
    if (n === 0 && typeof setImmediate !== "undefined") {
      return setImmediate(k);
    } else {
      return setTimeout(k, n);
    }
  }

  function clearDelay(n, t) {
    if (n === 0 && typeof clearImmediate !== "undefined") {
      return clearImmediate(t);
    } else {
      return clearTimeout(t);
    }
  }

  return function (right, ms) {
    return Aff.Async(function (cb) {
      return function () {
        var timer = setDelay(ms, cb(right()));
        return function () {
          return Aff.Sync(function () {
            return right(clearDelay(ms, timer));
          });
        };
      };
    });
  };
}();

