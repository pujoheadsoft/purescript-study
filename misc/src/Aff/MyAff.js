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
  var THROW   = "Throw";
  var CATCH   = "Catch";
  var SYNC    = "Sync";
  var ASYNC   = "Async";
  var BIND    = "Bind";
  var FORK    = "Fork";

  // Various constructors used in interpretation
  var CONS      = "Cons";      // Cons-list, for stacks
  var RESUME    = "Resume";    // Continue indiscriminately
  var RELEASE   = "Release";   // Continue with bracket finalizers
  var FINALIZED = "Finalized"; // Marker for finalization

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

  function nonCanceler(error) {
    return new Aff(PURE, void 0);
  }

  function runEff(eff) {
    try {
      eff();
    } catch (error) {
      setTimeout(function () {
        throw error;
      }, 0);
    }
  }

  function runSync(left, right, eff) {
    try {
      return right(eff());
    } catch (error) {
      return left(error);
    }
  }

  function runAsync(left, eff, k) {
    try {
      return eff(k)();
    } catch (error) {
      k(left(error))();
      return nonCanceler;
    }
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

  function Fiber(util, aff) {
    console.log("aff", aff)
    // Monotonically increasing tick, increased on each asynchronous turn.
    var runTick = 0;

    // The current branch of the state machine.
    var status = SUSPENDED;

    // The current point of interest for the state machine branch.
    var step      = aff;  // Successful step
    var fail      = null; // Failure step

    // Stack of continuations for the current fiber.
    var bhead = null;
    var btail = null;

    // Stack of attempts and finalizers for error recovery. Every `Cons` is also
    // tagged with current `interrupt` state. We use this to track which items
    // should be ignored or evaluated as a result of a kill.
    var attempts = null;

    
    // Each join gets a new id so they can be revoked.
    var joinId  = 0;
    var joins   = null;
    var rethrow = true;

    // Each invocation of `run` requires a tick. When an asynchronous effect is
    // resolved, we must check that the local tick coincides with the fiber
    // tick before resuming. This prevents multiple async continuations from
    // accidentally resuming the same fiber. A common example may be invoking
    // the provided callback in `makeAff` more than once, but it may also be an
    // async effect resuming after the fiber was already cancelled.
    function run(localRunTick) {
      var tmp, result, attempt;
      while (true) {
        tmp       = null;
        result    = null;
        attempt   = null;

        switch (status) {
        case STEP_BIND:
          status = CONTINUE;
          try {
            step   = bhead(step);
            if (btail === null) {
              bhead = null;
            } else {
              bhead = btail._1;
              btail = btail._2;
            }
          } catch (e) {
            status = RETURN;
            fail   = util.left(e);
            step   = null;
          }
          break;

        case STEP_RESULT:
          if (util.isLeft(step)) {
            status = RETURN;
            fail   = step;
            step   = null;
          } else if (bhead === null) {
            status = RETURN;
          } else {
            status = STEP_BIND;
            step   = util.fromRight(step);
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
              step   = util.right(step._1);
            } else {
              status = STEP_BIND;
              step   = step._1;
            }
            break;

          case SYNC:
            status = STEP_RESULT;
            step   = runSync(util.left, util.right, step._1);
            break;

          case ASYNC:
            status = PENDING;
            step   = runAsync(util.left, step._1, function (result) {
              return function () {
                if (runTick !== localRunTick) {
                  return;
                }
                runTick++;
                Scheduler.enqueue(function () {
                  // It's possible to interrupt the fiber between enqueuing and
                  // resuming, so we need to check that the runTick is still
                  // valid.
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

          case THROW:
            status = RETURN;
            fail   = util.left(step._1);
            step   = null;
            break;

          // Enqueue the Catch so that we can call the error handler later on
          // in case of an exception.
          case CATCH:
            if (bhead === null) {
              attempts = new Aff(CONS, step, attempts, null);
            } else {
              attempts = new Aff(CONS, step, new Aff(CONS, new Aff(RESUME, bhead, btail), attempts, null), null);
            }
            bhead    = null;
            btail    = null;
            status   = CONTINUE;
            step     = step._1;
            break;

          case FORK:
            status = STEP_RESULT;
            tmp    = Fiber(util, step._2);
            if (step._1) {
              tmp.run();
            }
            step = util.right(tmp);
            break;
          }
          break;

        case RETURN:
          bhead = null;
          btail = null;
          // If the current stack has returned, and we have no other stacks to
          // resume or finalizers to run, the fiber has halted and we can
          // invoke all join callbacks. Otherwise we need to resume.
          if (attempts === null) {
            status = COMPLETED;
            step   = fail || step;
          } else {
            // The interrupt status for the enqueued item.
            tmp      = attempts._3;
            attempt  = attempts._1;
            attempts = attempts._2;

            switch (attempt.tag) {
            // We cannot recover from an unmasked interrupt. Otherwise we should
            // continue stepping, or run the exception handler if an exception
            // was raised.
            case CATCH:
              // We should compare the interrupt status as well because we
              // only want it to apply if there has been an interrupt since
              // enqueuing the catch.
              if (fail) {
                status = CONTINUE;
                step   = attempt._2(util.fromLeft(fail));
                fail   = null;
              }
              break;

            // We cannot resume from an unmasked interrupt or exception.
            case RESUME:
              // As with Catch, we only want to ignore in the case of an
              // interrupt since enqueing the item.
              if (fail) {
                status = RETURN;
              } else {
                bhead  = attempt._1;
                btail  = attempt._2;
                status = STEP_BIND;
                step   = util.fromRight(step);
              }
              break;

            // Enqueue the appropriate handler. We increase the bracket count
            // because it should not be cancelled.
            case RELEASE:
              attempts = new Aff(CONS, new Aff(FINALIZED, step, fail), attempts, null);
              status   = CONTINUE;
              // It has only been killed if the interrupt status has changed
              // since we enqueued the item, and the bracket count is 0. If the
              // bracket count is non-zero then we are in a masked state so it's
              // impossible to be killed.
              if (fail) {
                step = attempt._1.failed(util.fromLeft(fail))(attempt._2);
              } else {
                step = attempt._1.completed(util.fromRight(step))(attempt._2);
              }
              fail = null;
              break;

            case FINALIZED:
              status = RETURN;
              step   = attempt._1;
              fail   = attempt._2;
              break;
            }
          }
          break;

        case COMPLETED:
          for (var k in joins) {
            if (joins.hasOwnProperty(k)) {
              rethrow = rethrow && joins[k].rethrow;
              runEff(joins[k].handler(step));
            }
          }
          joins = null;
          // If we have an interrupt and a fail, then the thread threw while
          // running finalizers. This should always rethrow in a fresh stack.
          if (fail) {
            setTimeout(function () {
              throw util.fromLeft(fail);
            }, 0);
          // If we have an unhandled exception, and no other fiber has joined
          // then we need to throw the exception in a fresh stack.
          } else if (util.isLeft(step) && rethrow) {
            setTimeout(function () {
              // Guard on reathrow because a completely synchronous fiber can
              // still have an observer which was added after-the-fact.
              if (rethrow) {
                throw util.fromLeft(step);
              }
            }, 0);
          }
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
          rethrow = rethrow && join.rethrow;
          join.handler(step)();
          return function () {};
        }

        var jid    = joinId++;
        joins      = joins || {};
        joins[jid] = join;

        return function() {
          if (joins !== null) {
            delete joins[jid];
          }
        };
      };
    }

    function join(cb) {
      return function () {
        var canceler = onComplete({
          rethrow: false,
          handler: cb
        })();
        if (status === SUSPENDED) {
          run(runTick);
        }
        return canceler;
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
  Aff.Throw       = AffCtr(THROW);
  Aff.Catch       = AffCtr(CATCH);
  Aff.Sync        = AffCtr(SYNC);
  Aff.Async       = AffCtr(ASYNC);
  Aff.Bind        = AffCtr(BIND);
  Aff.Fork        = AffCtr(FORK);
  Aff.Fiber       = Fiber;
  Aff.Scheduler   = Scheduler;
  Aff.nonCanceler = nonCanceler;

  return Aff;
}();

export const _pure = Aff.Pure;
export const _throwError = Aff.Throw;

export function _catchError(aff) {
  return function (k) {
    return Aff.Catch(aff, k);
  };
}

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

export function _makeFiber(util, aff) {
  return function () {
    return Aff.Fiber(util, aff);
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

