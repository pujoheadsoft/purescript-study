// modules are defined as an array
// [ module function, map of requires ]
//
// map of requires is short require name -> numeric require
//
// anything defined in a previous bundle is accessed via the
// orig method which is the require for previous bundles
parcelRequire = (function (modules, cache, entry, globalName) {
  // Save the require from previous bundle to this closure if any
  var previousRequire = typeof parcelRequire === 'function' && parcelRequire;
  var nodeRequire = typeof require === 'function' && require;

  function newRequire(name, jumped) {
    if (!cache[name]) {
      if (!modules[name]) {
        // if we cannot find the module within our internal map or
        // cache jump to the current global require ie. the last bundle
        // that was added to the page.
        var currentRequire = typeof parcelRequire === 'function' && parcelRequire;
        if (!jumped && currentRequire) {
          return currentRequire(name, true);
        }

        // If there are other bundles on this page the require from the
        // previous one is saved to 'previousRequire'. Repeat this as
        // many times as there are bundles until the module is found or
        // we exhaust the require chain.
        if (previousRequire) {
          return previousRequire(name, true);
        }

        // Try the node require function if it exists.
        if (nodeRequire && typeof name === 'string') {
          return nodeRequire(name);
        }

        var err = new Error('Cannot find module \'' + name + '\'');
        err.code = 'MODULE_NOT_FOUND';
        throw err;
      }

      localRequire.resolve = resolve;
      localRequire.cache = {};

      var module = cache[name] = new newRequire.Module(name);

      modules[name][0].call(module.exports, localRequire, module, module.exports, this);
    }

    return cache[name].exports;

    function localRequire(x){
      return newRequire(localRequire.resolve(x));
    }

    function resolve(x){
      return modules[name][1][x] || x;
    }
  }

  function Module(moduleName) {
    this.id = moduleName;
    this.bundle = newRequire;
    this.exports = {};
  }

  newRequire.isParcelRequire = true;
  newRequire.Module = Module;
  newRequire.modules = modules;
  newRequire.cache = cache;
  newRequire.parent = previousRequire;
  newRequire.register = function (id, exports) {
    modules[id] = [function (require, module) {
      module.exports = exports;
    }, {}];
  };

  var error;
  for (var i = 0; i < entry.length; i++) {
    try {
      newRequire(entry[i]);
    } catch (e) {
      // Save first error but execute all entries
      if (!error) {
        error = e;
      }
    }
  }

  if (entry.length) {
    // Expose entry point to Node, AMD or browser globals
    // Based on https://github.com/ForbesLindesay/umd/blob/master/template.js
    var mainExports = newRequire(entry[entry.length - 1]);

    // CommonJS
    if (typeof exports === "object" && typeof module !== "undefined") {
      module.exports = mainExports;

    // RequireJS
    } else if (typeof define === "function" && define.amd) {
     define(function () {
       return mainExports;
     });

    // <script>
    } else if (globalName) {
      this[globalName] = mainExports;
    }
  }

  // Override the current require with this new one
  parcelRequire = newRequire;

  if (error) {
    // throw error from earlier, _after updating parcelRequire_
    throw error;
  }

  return newRequire;
})({"../output/Effect.Aff/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports._bind = _bind;
exports._catchError = _catchError;
exports._delay = void 0;
exports._fork = _fork;
exports._killAll = _killAll;
exports._liftEffect = void 0;
exports._makeFiber = _makeFiber;
exports._makeSupervisedFiber = _makeSupervisedFiber;
exports._map = _map;
exports._parAffAlt = _parAffAlt;
exports._parAffApply = _parAffApply;
exports._parAffMap = _parAffMap;
exports._throwError = exports._sequential = exports._pure = void 0;
exports.generalBracket = generalBracket;
exports.makeAff = void 0;

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

  var PURE = "Pure";
  var THROW = "Throw";
  var CATCH = "Catch";
  var SYNC = "Sync";
  var ASYNC = "Async";
  var BIND = "Bind";
  var BRACKET = "Bracket";
  var FORK = "Fork";
  var SEQ = "Sequential";
  /*
   data ParAff eff a
    = forall b. Map (b -> a) (ParAff eff b)
    | forall b. Apply (ParAff eff (b -> a)) (ParAff eff b)
    | Alt (ParAff eff a) (ParAff eff a)
    | ?Par (Aff eff a)
   */

  var MAP = "Map";
  var APPLY = "Apply";
  var ALT = "Alt"; // Various constructors used in interpretation

  var CONS = "Cons"; // Cons-list, for stacks

  var RESUME = "Resume"; // Continue indiscriminately

  var RELEASE = "Release"; // Continue with bracket finalizers

  var FINALIZER = "Finalizer"; // A non-interruptible effect

  var FINALIZED = "Finalized"; // Marker for finalization

  var FORKED = "Forked"; // Reference to a forked fiber, with resumption stack

  var FIBER = "Fiber"; // Actual fiber reference

  var THUNK = "Thunk"; // Primed effect, ready to invoke

  function Aff(tag, _1, _2, _3) {
    this.tag = tag;
    this._1 = _1;
    this._2 = _2;
    this._3 = _3;
  }

  function AffCtr(tag) {
    var fn = function fn(_1, _2, _3) {
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
    var limit = 1024;
    var size = 0;
    var ix = 0;
    var queue = new Array(limit);
    var draining = false;

    function drain() {
      var thunk;
      draining = true;

      while (size !== 0) {
        size--;
        thunk = queue[ix];
        queue[ix] = void 0;
        ix = (ix + 1) % limit;
        thunk();
      }

      draining = false;
    }

    return {
      isDraining: function isDraining() {
        return draining;
      },
      enqueue: function enqueue(cb) {
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

  function Supervisor(util) {
    var fibers = {};
    var fiberId = 0;
    var count = 0;
    return {
      register: function register(fiber) {
        var fid = fiberId++;
        fiber.onComplete({
          rethrow: true,
          handler: function handler(result) {
            return function () {
              count--;
              delete fibers[fid];
            };
          }
        })();
        fibers[fid] = fiber;
        count++;
      },
      isEmpty: function isEmpty() {
        return count === 0;
      },
      killAll: function killAll(killError, cb) {
        return function () {
          if (count === 0) {
            return cb();
          }

          var killCount = 0;
          var kills = {};

          function kill(fid) {
            kills[fid] = fibers[fid].kill(killError, function (result) {
              return function () {
                delete kills[fid];
                killCount--;

                if (util.isLeft(result) && util.fromLeft(result)) {
                  setTimeout(function () {
                    throw util.fromLeft(result);
                  }, 0);
                }

                if (killCount === 0) {
                  cb();
                }
              };
            })();
          }

          for (var k in fibers) {
            if (fibers.hasOwnProperty(k)) {
              killCount++;
              kill(k);
            }
          }

          fibers = {};
          fiberId = 0;
          count = 0;
          return function (error) {
            return new Aff(SYNC, function () {
              for (var k in kills) {
                if (kills.hasOwnProperty(k)) {
                  kills[k]();
                }
              }
            });
          };
        };
      }
    };
  } // Fiber state machine


  var SUSPENDED = 0; // Suspended, pending a join.

  var CONTINUE = 1; // Interpret the next instruction.

  var STEP_BIND = 2; // Apply the next bind.

  var STEP_RESULT = 3; // Handle potential failure from a result.

  var PENDING = 4; // An async effect is running.

  var RETURN = 5; // The current stack has returned.

  var COMPLETED = 6; // The entire fiber has completed.

  function Fiber(util, supervisor, aff) {
    // Monotonically increasing tick, increased on each asynchronous turn.
    var runTick = 0; // The current branch of the state machine.

    var status = SUSPENDED; // The current point of interest for the state machine branch.

    var step = aff; // Successful step

    var fail = null; // Failure step

    var interrupt = null; // Asynchronous interrupt
    // Stack of continuations for the current fiber.

    var bhead = null;
    var btail = null; // Stack of attempts and finalizers for error recovery. Every `Cons` is also
    // tagged with current `interrupt` state. We use this to track which items
    // should be ignored or evaluated as a result of a kill.

    var attempts = null; // A special state is needed for Bracket, because it cannot be killed. When
    // we enter a bracket acquisition or finalizer, we increment the counter,
    // and then decrement once complete.

    var bracketCount = 0; // Each join gets a new id so they can be revoked.

    var joinId = 0;
    var joins = null;
    var rethrow = true; // Each invocation of `run` requires a tick. When an asynchronous effect is
    // resolved, we must check that the local tick coincides with the fiber
    // tick before resuming. This prevents multiple async continuations from
    // accidentally resuming the same fiber. A common example may be invoking
    // the provided callback in `makeAff` more than once, but it may also be an
    // async effect resuming after the fiber was already cancelled.

    function _run(localRunTick) {
      var tmp, result, attempt;

      while (true) {
        tmp = null;
        result = null;
        attempt = null;

        switch (status) {
          case STEP_BIND:
            status = CONTINUE;

            try {
              step = bhead(step);

              if (btail === null) {
                bhead = null;
              } else {
                bhead = btail._1;
                btail = btail._2;
              }
            } catch (e) {
              status = RETURN;
              fail = util.left(e);
              step = null;
            }

            break;

          case STEP_RESULT:
            if (util.isLeft(step)) {
              status = RETURN;
              fail = step;
              step = null;
            } else if (bhead === null) {
              status = RETURN;
            } else {
              status = STEP_BIND;
              step = util.fromRight(step);
            }

            break;

          case CONTINUE:
            switch (step.tag) {
              case BIND:
                if (bhead) {
                  btail = new Aff(CONS, bhead, btail);
                }

                bhead = step._2;
                status = CONTINUE;
                step = step._1;
                break;

              case PURE:
                if (bhead === null) {
                  status = RETURN;
                  step = util.right(step._1);
                } else {
                  status = STEP_BIND;
                  step = step._1;
                }

                break;

              case SYNC:
                status = STEP_RESULT;
                step = runSync(util.left, util.right, step._1);
                break;

              case ASYNC:
                status = PENDING;
                step = runAsync(util.left, step._1, function (result) {
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
                      step = result;

                      _run(runTick);
                    });
                  };
                });
                return;

              case THROW:
                status = RETURN;
                fail = util.left(step._1);
                step = null;
                break;
              // Enqueue the Catch so that we can call the error handler later on
              // in case of an exception.

              case CATCH:
                if (bhead === null) {
                  attempts = new Aff(CONS, step, attempts, interrupt);
                } else {
                  attempts = new Aff(CONS, step, new Aff(CONS, new Aff(RESUME, bhead, btail), attempts, interrupt), interrupt);
                }

                bhead = null;
                btail = null;
                status = CONTINUE;
                step = step._1;
                break;
              // Enqueue the Bracket so that we can call the appropriate handlers
              // after resource acquisition.

              case BRACKET:
                bracketCount++;

                if (bhead === null) {
                  attempts = new Aff(CONS, step, attempts, interrupt);
                } else {
                  attempts = new Aff(CONS, step, new Aff(CONS, new Aff(RESUME, bhead, btail), attempts, interrupt), interrupt);
                }

                bhead = null;
                btail = null;
                status = CONTINUE;
                step = step._1;
                break;

              case FORK:
                status = STEP_RESULT;
                tmp = Fiber(util, supervisor, step._2);

                if (supervisor) {
                  supervisor.register(tmp);
                }

                if (step._1) {
                  tmp.run();
                }

                step = util.right(tmp);
                break;

              case SEQ:
                status = CONTINUE;
                step = sequential(util, supervisor, step._1);
                break;
            }

            break;

          case RETURN:
            bhead = null;
            btail = null; // If the current stack has returned, and we have no other stacks to
            // resume or finalizers to run, the fiber has halted and we can
            // invoke all join callbacks. Otherwise we need to resume.

            if (attempts === null) {
              status = COMPLETED;
              step = interrupt || fail || step;
            } else {
              // The interrupt status for the enqueued item.
              tmp = attempts._3;
              attempt = attempts._1;
              attempts = attempts._2;

              switch (attempt.tag) {
                // We cannot recover from an unmasked interrupt. Otherwise we should
                // continue stepping, or run the exception handler if an exception
                // was raised.
                case CATCH:
                  // We should compare the interrupt status as well because we
                  // only want it to apply if there has been an interrupt since
                  // enqueuing the catch.
                  if (interrupt && interrupt !== tmp && bracketCount === 0) {
                    status = RETURN;
                  } else if (fail) {
                    status = CONTINUE;
                    step = attempt._2(util.fromLeft(fail));
                    fail = null;
                  }

                  break;
                // We cannot resume from an unmasked interrupt or exception.

                case RESUME:
                  // As with Catch, we only want to ignore in the case of an
                  // interrupt since enqueing the item.
                  if (interrupt && interrupt !== tmp && bracketCount === 0 || fail) {
                    status = RETURN;
                  } else {
                    bhead = attempt._1;
                    btail = attempt._2;
                    status = STEP_BIND;
                    step = util.fromRight(step);
                  }

                  break;
                // If we have a bracket, we should enqueue the handlers,
                // and continue with the success branch only if the fiber has
                // not been interrupted. If the bracket acquisition failed, we
                // should not run either.

                case BRACKET:
                  bracketCount--;

                  if (fail === null) {
                    result = util.fromRight(step); // We need to enqueue the Release with the same interrupt
                    // status as the Bracket that is initiating it.

                    attempts = new Aff(CONS, new Aff(RELEASE, attempt._2, result), attempts, tmp); // We should only coninue as long as the interrupt status has not changed or
                    // we are currently within a non-interruptable finalizer.

                    if (interrupt === tmp || bracketCount > 0) {
                      status = CONTINUE;
                      step = attempt._3(result);
                    }
                  }

                  break;
                // Enqueue the appropriate handler. We increase the bracket count
                // because it should not be cancelled.

                case RELEASE:
                  attempts = new Aff(CONS, new Aff(FINALIZED, step, fail), attempts, interrupt);
                  status = CONTINUE; // It has only been killed if the interrupt status has changed
                  // since we enqueued the item, and the bracket count is 0. If the
                  // bracket count is non-zero then we are in a masked state so it's
                  // impossible to be killed.

                  if (interrupt && interrupt !== tmp && bracketCount === 0) {
                    step = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                  } else if (fail) {
                    step = attempt._1.failed(util.fromLeft(fail))(attempt._2);
                  } else {
                    step = attempt._1.completed(util.fromRight(step))(attempt._2);
                  }

                  fail = null;
                  bracketCount++;
                  break;

                case FINALIZER:
                  bracketCount++;
                  attempts = new Aff(CONS, new Aff(FINALIZED, step, fail), attempts, interrupt);
                  status = CONTINUE;
                  step = attempt._1;
                  break;

                case FINALIZED:
                  bracketCount--;
                  status = RETURN;
                  step = attempt._1;
                  fail = attempt._2;
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

            joins = null; // If we have an interrupt and a fail, then the thread threw while
            // running finalizers. This should always rethrow in a fresh stack.

            if (interrupt && fail) {
              setTimeout(function () {
                throw util.fromLeft(fail);
              }, 0); // If we have an unhandled exception, and no other fiber has joined
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

          case PENDING:
            return;
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

        var jid = joinId++;
        joins = joins || {};
        joins[jid] = join;
        return function () {
          if (joins !== null) {
            delete joins[jid];
          }
        };
      };
    }

    function kill(error, cb) {
      return function () {
        if (status === COMPLETED) {
          cb(util.right(void 0))();
          return function () {};
        }

        var canceler = onComplete({
          rethrow: false,
          handler: function handler() {
            return cb(util.right(void 0));
          }
        })();

        switch (status) {
          case SUSPENDED:
            interrupt = util.left(error);
            status = COMPLETED;
            step = interrupt;

            _run(runTick);

            break;

          case PENDING:
            if (interrupt === null) {
              interrupt = util.left(error);
            }

            if (bracketCount === 0) {
              if (status === PENDING) {
                attempts = new Aff(CONS, new Aff(FINALIZER, step(error)), attempts, interrupt);
              }

              status = RETURN;
              step = null;
              fail = null;

              _run(++runTick);
            }

            break;

          default:
            if (interrupt === null) {
              interrupt = util.left(error);
            }

            if (bracketCount === 0) {
              status = RETURN;
              step = null;
              fail = null;
            }

        }

        return canceler;
      };
    }

    function join(cb) {
      return function () {
        var canceler = onComplete({
          rethrow: false,
          handler: cb
        })();

        if (status === SUSPENDED) {
          _run(runTick);
        }

        return canceler;
      };
    }

    return {
      kill: kill,
      join: join,
      onComplete: onComplete,
      isSuspended: function isSuspended() {
        return status === SUSPENDED;
      },
      run: function run() {
        if (status === SUSPENDED) {
          if (!Scheduler.isDraining()) {
            Scheduler.enqueue(function () {
              _run(runTick);
            });
          } else {
            _run(runTick);
          }
        }
      }
    };
  }

  function runPar(util, supervisor, par, cb) {
    // Table of all forked fibers.
    var fiberId = 0;
    var fibers = {}; // Table of currently running cancelers, as a product of `Alt` behavior.

    var killId = 0;
    var kills = {}; // Error used for early cancelation on Alt branches.

    var early = new Error("[ParAff] Early exit"); // Error used to kill the entire tree.

    var interrupt = null; // The root pointer of the tree.

    var root = EMPTY; // Walks a tree, invoking all the cancelers. Returns the table of pending
    // cancellation fibers.

    function kill(error, par, cb) {
      var step = par;
      var head = null;
      var tail = null;
      var count = 0;
      var kills = {};
      var tmp, kid;

      loop: while (true) {
        tmp = null;

        switch (step.tag) {
          case FORKED:
            if (step._3 === EMPTY) {
              tmp = fibers[step._1];
              kills[count++] = tmp.kill(error, function (result) {
                return function () {
                  count--;

                  if (count === 0) {
                    cb(result)();
                  }
                };
              });
            } // Terminal case.


            if (head === null) {
              break loop;
            } // Go down the right side of the tree.


            step = head._2;

            if (tail === null) {
              head = null;
            } else {
              head = tail._1;
              tail = tail._2;
            }

            break;

          case MAP:
            step = step._2;
            break;

          case APPLY:
          case ALT:
            if (head) {
              tail = new Aff(CONS, head, tail);
            }

            head = step;
            step = step._1;
            break;
        }
      }

      if (count === 0) {
        cb(util.right(void 0))();
      } else {
        // Run the cancelation effects. We alias `count` because it's mutable.
        kid = 0;
        tmp = count;

        for (; kid < tmp; kid++) {
          kills[kid] = kills[kid]();
        }
      }

      return kills;
    } // When a fiber resolves, we need to bubble back up the tree with the
    // result, computing the applicative nodes.


    function join(result, head, tail) {
      var fail, step, lhs, rhs, tmp, kid;

      if (util.isLeft(result)) {
        fail = result;
        step = null;
      } else {
        step = result;
        fail = null;
      }

      loop: while (true) {
        lhs = null;
        rhs = null;
        tmp = null;
        kid = null; // We should never continue if the entire tree has been interrupted.

        if (interrupt !== null) {
          return;
        } // We've made it all the way to the root of the tree, which means
        // the tree has fully evaluated.


        if (head === null) {
          cb(fail || step)();
          return;
        } // The tree has already been computed, so we shouldn't try to do it
        // again. This should never happen.
        // TODO: Remove this?


        if (head._3 !== EMPTY) {
          return;
        }

        switch (head.tag) {
          case MAP:
            if (fail === null) {
              head._3 = util.right(head._1(util.fromRight(step)));
              step = head._3;
            } else {
              head._3 = fail;
            }

            break;

          case APPLY:
            lhs = head._1._3;
            rhs = head._2._3; // If we have a failure we should kill the other side because we
            // can't possible yield a result anymore.

            if (fail) {
              head._3 = fail;
              tmp = true;
              kid = killId++;
              kills[kid] = kill(early, fail === lhs ? head._2 : head._1, function () {
                return function () {
                  delete kills[kid];

                  if (tmp) {
                    tmp = false;
                  } else if (tail === null) {
                    join(fail, null, null);
                  } else {
                    join(fail, tail._1, tail._2);
                  }
                };
              });

              if (tmp) {
                tmp = false;
                return;
              }
            } else if (lhs === EMPTY || rhs === EMPTY) {
              // We can only proceed if both sides have resolved.
              return;
            } else {
              step = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
              head._3 = step;
            }

            break;

          case ALT:
            lhs = head._1._3;
            rhs = head._2._3; // We can only proceed if both have resolved or we have a success

            if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
              return;
            } // If both sides resolve with an error, we should continue with the
            // first error


            if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
              fail = step === lhs ? rhs : lhs;
              step = null;
              head._3 = fail;
            } else {
              head._3 = step;
              tmp = true;
              kid = killId++; // Once a side has resolved, we need to cancel the side that is still
              // pending before we can continue.

              kills[kid] = kill(early, step === lhs ? head._2 : head._1, function () {
                return function () {
                  delete kills[kid];

                  if (tmp) {
                    tmp = false;
                  } else if (tail === null) {
                    join(step, null, null);
                  } else {
                    join(step, tail._1, tail._2);
                  }
                };
              });

              if (tmp) {
                tmp = false;
                return;
              }
            }

            break;
        }

        if (tail === null) {
          head = null;
        } else {
          head = tail._1;
          tail = tail._2;
        }
      }
    }

    function resolve(fiber) {
      return function (result) {
        return function () {
          delete fibers[fiber._1];
          fiber._3 = result;
          join(result, fiber._2._1, fiber._2._2);
        };
      };
    } // Walks the applicative tree, substituting non-applicative nodes with
    // `FORKED` nodes. In this tree, all applicative nodes use the `_3` slot
    // as a mutable slot for memoization. In an unresolved state, the `_3`
    // slot is `EMPTY`. In the cases of `ALT` and `APPLY`, we always walk
    // the left side first, because both operations are left-associative. As
    // we `RETURN` from those branches, we then walk the right side.


    function run() {
      var status = CONTINUE;
      var step = par;
      var head = null;
      var tail = null;
      var tmp, fid;

      loop: while (true) {
        tmp = null;
        fid = null;

        switch (status) {
          case CONTINUE:
            switch (step.tag) {
              case MAP:
                if (head) {
                  tail = new Aff(CONS, head, tail);
                }

                head = new Aff(MAP, step._1, EMPTY, EMPTY);
                step = step._2;
                break;

              case APPLY:
                if (head) {
                  tail = new Aff(CONS, head, tail);
                }

                head = new Aff(APPLY, EMPTY, step._2, EMPTY);
                step = step._1;
                break;

              case ALT:
                if (head) {
                  tail = new Aff(CONS, head, tail);
                }

                head = new Aff(ALT, EMPTY, step._2, EMPTY);
                step = step._1;
                break;

              default:
                // When we hit a leaf value, we suspend the stack in the `FORKED`.
                // When the fiber resolves, it can bubble back up the tree.
                fid = fiberId++;
                status = RETURN;
                tmp = step;
                step = new Aff(FORKED, fid, new Aff(CONS, head, tail), EMPTY);
                tmp = Fiber(util, supervisor, tmp);
                tmp.onComplete({
                  rethrow: false,
                  handler: resolve(step)
                })();
                fibers[fid] = tmp;

                if (supervisor) {
                  supervisor.register(tmp);
                }

            }

            break;

          case RETURN:
            // Terminal case, we are back at the root.
            if (head === null) {
              break loop;
            } // If we are done with the right side, we need to continue down the
            // left. Otherwise we should continue up the stack.


            if (head._1 === EMPTY) {
              head._1 = step;
              status = CONTINUE;
              step = head._2;
              head._2 = EMPTY;
            } else {
              head._2 = step;
              step = head;

              if (tail === null) {
                head = null;
              } else {
                head = tail._1;
                tail = tail._2;
              }
            }

        }
      } // Keep a reference to the tree root so it can be cancelled.


      root = step;

      for (fid = 0; fid < fiberId; fid++) {
        fibers[fid].run();
      }
    } // Cancels the entire tree. If there are already subtrees being canceled,
    // we need to first cancel those joins. We will then add fresh joins for
    // all pending branches including those that were in the process of being
    // canceled.


    function cancel(error, cb) {
      interrupt = util.left(error);
      var innerKills;

      for (var kid in kills) {
        if (kills.hasOwnProperty(kid)) {
          innerKills = kills[kid];

          for (kid in innerKills) {
            if (innerKills.hasOwnProperty(kid)) {
              innerKills[kid]();
            }
          }
        }
      }

      kills = null;
      var newKills = kill(error, root, cb);
      return function (killError) {
        return new Aff(ASYNC, function (killCb) {
          return function () {
            for (var kid in newKills) {
              if (newKills.hasOwnProperty(kid)) {
                newKills[kid]();
              }
            }

            return nonCanceler;
          };
        });
      };
    }

    run();
    return function (killError) {
      return new Aff(ASYNC, function (killCb) {
        return function () {
          return cancel(killError, killCb);
        };
      });
    };
  }

  function sequential(util, supervisor, par) {
    return new Aff(ASYNC, function (cb) {
      return function () {
        return runPar(util, supervisor, par, cb);
      };
    });
  }

  Aff.EMPTY = EMPTY;
  Aff.Pure = AffCtr(PURE);
  Aff.Throw = AffCtr(THROW);
  Aff.Catch = AffCtr(CATCH);
  Aff.Sync = AffCtr(SYNC);
  Aff.Async = AffCtr(ASYNC);
  Aff.Bind = AffCtr(BIND);
  Aff.Bracket = AffCtr(BRACKET);
  Aff.Fork = AffCtr(FORK);
  Aff.Seq = AffCtr(SEQ);
  Aff.ParMap = AffCtr(MAP);
  Aff.ParApply = AffCtr(APPLY);
  Aff.ParAlt = AffCtr(ALT);
  Aff.Fiber = Fiber;
  Aff.Supervisor = Supervisor;
  Aff.Scheduler = Scheduler;
  Aff.nonCanceler = nonCanceler;
  return Aff;
}();

var _pure = Aff.Pure;
exports._pure = _pure;
var _throwError = Aff.Throw;
exports._throwError = _throwError;

function _catchError(aff) {
  return function (k) {
    return Aff.Catch(aff, k);
  };
}

function _map(f) {
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

function _bind(aff) {
  return function (k) {
    return Aff.Bind(aff, k);
  };
}

function _fork(immediate) {
  return function (aff) {
    return Aff.Fork(immediate, aff);
  };
}

var _liftEffect = Aff.Sync;
exports._liftEffect = _liftEffect;

function _parAffMap(f) {
  return function (aff) {
    return Aff.ParMap(f, aff);
  };
}

function _parAffApply(aff1) {
  return function (aff2) {
    return Aff.ParApply(aff1, aff2);
  };
}

function _parAffAlt(aff1) {
  return function (aff2) {
    return Aff.ParAlt(aff1, aff2);
  };
}

var makeAff = Aff.Async;
exports.makeAff = makeAff;

function generalBracket(acquire) {
  return function (options) {
    return function (k) {
      return Aff.Bracket(acquire, options, k);
    };
  };
}

function _makeFiber(util, aff) {
  return function () {
    return Aff.Fiber(util, null, aff);
  };
}

function _makeSupervisedFiber(util, aff) {
  return function () {
    var supervisor = Aff.Supervisor(util);
    return {
      fiber: Aff.Fiber(util, supervisor, aff),
      supervisor: supervisor
    };
  };
}

function _killAll(error, supervisor, cb) {
  return supervisor.killAll(error, cb);
}

var _delay = function () {
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

exports._delay = _delay;
var _sequential = Aff.Seq;
exports._sequential = _sequential;
},{}],"../output/Control.Apply/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.arrayApply = void 0;

var arrayApply = function arrayApply(fs) {
  return function (xs) {
    var l = fs.length;
    var k = xs.length;
    var result = new Array(l * k);
    var n = 0;

    for (var i = 0; i < l; i++) {
      var f = fs[i];

      for (var j = 0; j < k; j++) {
        result[n++] = f(xs[j]);
      }
    }

    return result;
  };
};

exports.arrayApply = arrayApply;
},{}],"../output/Control.Semigroupoid/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.semigroupoidFn = exports.composeFlipped = exports.compose = void 0;
// Generated by purs version 0.15.10
var semigroupoidFn = {
  compose: function compose(f) {
    return function (g) {
      return function (x) {
        return f(g(x));
      };
    };
  }
};
exports.semigroupoidFn = semigroupoidFn;

var compose = function compose(dict) {
  return dict.compose;
};

exports.compose = compose;

var composeFlipped = function composeFlipped(dictSemigroupoid) {
  var compose1 = compose(dictSemigroupoid);
  return function (f) {
    return function (g) {
      return compose1(g)(f);
    };
  };
};

exports.composeFlipped = composeFlipped;
},{}],"../output/Control.Category/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.categoryFn = void 0;
Object.defineProperty(exports, "compose", {
  enumerable: true,
  get: function () {
    return Control_Semigroupoid.compose;
  }
});
exports.identity = void 0;

var Control_Semigroupoid = _interopRequireWildcard(require("../Control.Semigroupoid/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var identity = function identity(dict) {
  return dict.identity;
};

exports.identity = identity;
var categoryFn = {
  identity: function identity(x) {
    return x;
  },
  Semigroupoid0: function Semigroupoid0() {
    return Control_Semigroupoid.semigroupoidFn;
  }
};
exports.categoryFn = categoryFn;
},{"../Control.Semigroupoid/index.js":"../output/Control.Semigroupoid/index.js"}],"../output/Data.Boolean/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.otherwise = void 0;
// Generated by purs version 0.15.10
var otherwise = true;
exports.otherwise = otherwise;
},{}],"../output/Data.Function/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.applyN = exports.applyFlipped = exports.apply = void 0;
Object.defineProperty(exports, "compose", {
  enumerable: true,
  get: function () {
    return Control_Category.compose;
  }
});
exports.flip = exports.const = void 0;
Object.defineProperty(exports, "identity", {
  enumerable: true,
  get: function () {
    return Control_Category.identity;
  }
});
exports.on = void 0;

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Data_Boolean = _interopRequireWildcard(require("../Data.Boolean/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var on = function on(f) {
  return function (g) {
    return function (x) {
      return function (y) {
        return f(g(x))(g(y));
      };
    };
  };
};

exports.on = on;

var flip = function flip(f) {
  return function (b) {
    return function (a) {
      return f(a)(b);
    };
  };
};

exports.flip = flip;

var $$const = function $$const(a) {
  return function (v) {
    return a;
  };
};

exports.const = $$const;

var applyN = function applyN(f) {
  var go = function go($copy_n) {
    return function ($copy_acc) {
      var $tco_var_n = $copy_n;
      var $tco_done = false;
      var $tco_result;

      function $tco_loop(n, acc) {
        if (n <= 0) {
          $tco_done = true;
          return acc;
        }

        ;

        if (Data_Boolean.otherwise) {
          $tco_var_n = n - 1 | 0;
          $copy_acc = f(acc);
          return;
        }

        ;
        throw new Error("Failed pattern match at Data.Function (line 107, column 3 - line 109, column 37): " + [n.constructor.name, acc.constructor.name]);
      }

      ;

      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_n, $copy_acc);
      }

      ;
      return $tco_result;
    };
  };

  return go;
};

exports.applyN = applyN;

var applyFlipped = function applyFlipped(x) {
  return function (f) {
    return f(x);
  };
};

exports.applyFlipped = applyFlipped;

var apply = function apply(f) {
  return function (x) {
    return f(x);
  };
};

exports.apply = apply;
},{"../Control.Category/index.js":"../output/Control.Category/index.js","../Data.Boolean/index.js":"../output/Data.Boolean/index.js"}],"../output/Data.Functor/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.arrayMap = void 0;

var arrayMap = function arrayMap(f) {
  return function (arr) {
    var l = arr.length;
    var result = new Array(l);

    for (var i = 0; i < l; i++) {
      result[i] = f(arr[i]);
    }

    return result;
  };
};

exports.arrayMap = arrayMap;
},{}],"../output/Data.Unit/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.unit = void 0;
var unit = undefined;
exports.unit = unit;
},{}],"../output/Data.Unit/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "unit", {
  enumerable: true,
  get: function () {
    return $foreign.unit;
  }
});

var $foreign = _interopRequireWildcard(require("./foreign.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }
},{"./foreign.js":"../output/Data.Unit/foreign.js"}],"../output/Type.Proxy/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Proxy = void 0;

// Generated by purs version 0.15.10
var $$Proxy =
/* #__PURE__ */
function () {
  function $$Proxy() {}

  ;
  $$Proxy.value = new $$Proxy();
  return $$Proxy;
}();

exports.Proxy = $$Proxy;
},{}],"../output/Data.Functor/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.voidRight = exports.voidLeft = exports.void = exports.mapFlipped = exports.map = exports.functorProxy = exports.functorFn = exports.functorArray = exports.flap = void 0;

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Control_Semigroupoid = _interopRequireWildcard(require("../Control.Semigroupoid/index.js"));

var Data_Function = _interopRequireWildcard(require("../Data.Function/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Type_Proxy = _interopRequireWildcard(require("../Type.Proxy/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var map = function map(dict) {
  return dict.map;
};

exports.map = map;

var mapFlipped = function mapFlipped(dictFunctor) {
  var map1 = map(dictFunctor);
  return function (fa) {
    return function (f) {
      return map1(f)(fa);
    };
  };
};

exports.mapFlipped = mapFlipped;

var $$void = function $$void(dictFunctor) {
  return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
};

exports.void = $$void;

var voidLeft = function voidLeft(dictFunctor) {
  var map1 = map(dictFunctor);
  return function (f) {
    return function (x) {
      return map1(Data_Function["const"](x))(f);
    };
  };
};

exports.voidLeft = voidLeft;

var voidRight = function voidRight(dictFunctor) {
  var map1 = map(dictFunctor);
  return function (x) {
    return map1(Data_Function["const"](x));
  };
};

exports.voidRight = voidRight;
var functorProxy = {
  map: function map(v) {
    return function (v1) {
      return Type_Proxy["Proxy"].value;
    };
  }
};
exports.functorProxy = functorProxy;
var functorFn = {
  map:
  /* #__PURE__ */
  Control_Semigroupoid.compose(Control_Semigroupoid.semigroupoidFn)
};
exports.functorFn = functorFn;
var functorArray = {
  map: $foreign.arrayMap
};
exports.functorArray = functorArray;

var flap = function flap(dictFunctor) {
  var map1 = map(dictFunctor);
  return function (ff) {
    return function (x) {
      return map1(function (f) {
        return f(x);
      })(ff);
    };
  };
};

exports.flap = flap;
},{"./foreign.js":"../output/Data.Functor/foreign.js","../Control.Semigroupoid/index.js":"../output/Control.Semigroupoid/index.js","../Data.Function/index.js":"../output/Data.Function/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Type.Proxy/index.js":"../output/Type.Proxy/index.js"}],"../output/Control.Apply/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.lift5 = exports.lift4 = exports.lift3 = exports.lift2 = exports.applySecond = exports.applyProxy = exports.applyFn = exports.applyFirst = exports.applyArray = exports.apply = void 0;
Object.defineProperty(exports, "map", {
  enumerable: true,
  get: function () {
    return Data_Functor.map;
  }
});
Object.defineProperty(exports, "void", {
  enumerable: true,
  get: function () {
    return Data_Functor.void;
  }
});

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Data_Function = _interopRequireWildcard(require("../Data.Function/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Type_Proxy = _interopRequireWildcard(require("../Type.Proxy/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var identity =
/* #__PURE__ */
Control_Category.identity(Control_Category.categoryFn);
var applyProxy = {
  apply: function apply(v) {
    return function (v1) {
      return Type_Proxy["Proxy"].value;
    };
  },
  Functor0: function Functor0() {
    return Data_Functor.functorProxy;
  }
};
exports.applyProxy = applyProxy;
var applyFn = {
  apply: function apply(f) {
    return function (g) {
      return function (x) {
        return f(x)(g(x));
      };
    };
  },
  Functor0: function Functor0() {
    return Data_Functor.functorFn;
  }
};
exports.applyFn = applyFn;
var applyArray = {
  apply: $foreign.arrayApply,
  Functor0: function Functor0() {
    return Data_Functor.functorArray;
  }
};
exports.applyArray = applyArray;

var apply = function apply(dict) {
  return dict.apply;
};

exports.apply = apply;

var applyFirst = function applyFirst(dictApply) {
  var apply1 = apply(dictApply);
  var map = Data_Functor.map(dictApply.Functor0());
  return function (a) {
    return function (b) {
      return apply1(map(Data_Function["const"])(a))(b);
    };
  };
};

exports.applyFirst = applyFirst;

var applySecond = function applySecond(dictApply) {
  var apply1 = apply(dictApply);
  var map = Data_Functor.map(dictApply.Functor0());
  return function (a) {
    return function (b) {
      return apply1(map(Data_Function["const"](identity))(a))(b);
    };
  };
};

exports.applySecond = applySecond;

var lift2 = function lift2(dictApply) {
  var apply1 = apply(dictApply);
  var map = Data_Functor.map(dictApply.Functor0());
  return function (f) {
    return function (a) {
      return function (b) {
        return apply1(map(f)(a))(b);
      };
    };
  };
};

exports.lift2 = lift2;

var lift3 = function lift3(dictApply) {
  var apply1 = apply(dictApply);
  var map = Data_Functor.map(dictApply.Functor0());
  return function (f) {
    return function (a) {
      return function (b) {
        return function (c) {
          return apply1(apply1(map(f)(a))(b))(c);
        };
      };
    };
  };
};

exports.lift3 = lift3;

var lift4 = function lift4(dictApply) {
  var apply1 = apply(dictApply);
  var map = Data_Functor.map(dictApply.Functor0());
  return function (f) {
    return function (a) {
      return function (b) {
        return function (c) {
          return function (d) {
            return apply1(apply1(apply1(map(f)(a))(b))(c))(d);
          };
        };
      };
    };
  };
};

exports.lift4 = lift4;

var lift5 = function lift5(dictApply) {
  var apply1 = apply(dictApply);
  var map = Data_Functor.map(dictApply.Functor0());
  return function (f) {
    return function (a) {
      return function (b) {
        return function (c) {
          return function (d) {
            return function (e) {
              return apply1(apply1(apply1(apply1(map(f)(a))(b))(c))(d))(e);
            };
          };
        };
      };
    };
  };
};

exports.lift5 = lift5;
},{"./foreign.js":"../output/Control.Apply/foreign.js","../Control.Category/index.js":"../output/Control.Category/index.js","../Data.Function/index.js":"../output/Data.Function/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Type.Proxy/index.js":"../output/Type.Proxy/index.js"}],"../output/Control.Applicative/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.applicativeProxy = exports.applicativeFn = exports.applicativeArray = void 0;
Object.defineProperty(exports, "apply", {
  enumerable: true,
  get: function () {
    return Control_Apply.apply;
  }
});
exports.liftA1 = void 0;
Object.defineProperty(exports, "map", {
  enumerable: true,
  get: function () {
    return Data_Functor.map;
  }
});
exports.unless = exports.pure = void 0;
Object.defineProperty(exports, "void", {
  enumerable: true,
  get: function () {
    return Data_Functor.void;
  }
});
exports.when = void 0;

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Type_Proxy = _interopRequireWildcard(require("../Type.Proxy/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var pure = function pure(dict) {
  return dict.pure;
};

exports.pure = pure;

var unless = function unless(dictApplicative) {
  var pure1 = pure(dictApplicative);
  return function (v) {
    return function (v1) {
      if (!v) {
        return v1;
      }

      ;

      if (v) {
        return pure1(Data_Unit.unit);
      }

      ;
      throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
    };
  };
};

exports.unless = unless;

var when = function when(dictApplicative) {
  var pure1 = pure(dictApplicative);
  return function (v) {
    return function (v1) {
      if (v) {
        return v1;
      }

      ;

      if (!v) {
        return pure1(Data_Unit.unit);
      }

      ;
      throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
    };
  };
};

exports.when = when;

var liftA1 = function liftA1(dictApplicative) {
  var apply = Control_Apply.apply(dictApplicative.Apply0());
  var pure1 = pure(dictApplicative);
  return function (f) {
    return function (a) {
      return apply(pure1(f))(a);
    };
  };
};

exports.liftA1 = liftA1;
var applicativeProxy = {
  pure: function pure(v) {
    return Type_Proxy["Proxy"].value;
  },
  Apply0: function Apply0() {
    return Control_Apply.applyProxy;
  }
};
exports.applicativeProxy = applicativeProxy;
var applicativeFn = {
  pure: function pure(x) {
    return function (v) {
      return x;
    };
  },
  Apply0: function Apply0() {
    return Control_Apply.applyFn;
  }
};
exports.applicativeFn = applicativeFn;
var applicativeArray = {
  pure: function pure(x) {
    return [x];
  },
  Apply0: function Apply0() {
    return Control_Apply.applyArray;
  }
};
exports.applicativeArray = applicativeArray;
},{"../Control.Apply/index.js":"../output/Control.Apply/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Type.Proxy/index.js":"../output/Type.Proxy/index.js"}],"../output/Control.Bind/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.arrayBind = void 0;

var arrayBind = function arrayBind(arr) {
  return function (f) {
    var result = [];

    for (var i = 0, l = arr.length; i < l; i++) {
      Array.prototype.push.apply(result, f(arr[i]));
    }

    return result;
  };
};

exports.arrayBind = arrayBind;
},{}],"../output/Control.Bind/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "apply", {
  enumerable: true,
  get: function () {
    return Control_Apply.apply;
  }
});
exports.join = exports.ifM = exports.discardUnit = exports.discardProxy = exports.discard = exports.composeKleisliFlipped = exports.composeKleisli = exports.bindProxy = exports.bindFn = exports.bindFlipped = exports.bindArray = exports.bind = void 0;
Object.defineProperty(exports, "liftA1", {
  enumerable: true,
  get: function () {
    return Control_Applicative.liftA1;
  }
});
Object.defineProperty(exports, "map", {
  enumerable: true,
  get: function () {
    return Data_Functor.map;
  }
});
Object.defineProperty(exports, "pure", {
  enumerable: true,
  get: function () {
    return Control_Applicative.pure;
  }
});
Object.defineProperty(exports, "unless", {
  enumerable: true,
  get: function () {
    return Control_Applicative.unless;
  }
});
Object.defineProperty(exports, "void", {
  enumerable: true,
  get: function () {
    return Data_Functor.void;
  }
});
Object.defineProperty(exports, "when", {
  enumerable: true,
  get: function () {
    return Control_Applicative.when;
  }
});

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Data_Function = _interopRequireWildcard(require("../Data.Function/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Type_Proxy = _interopRequireWildcard(require("../Type.Proxy/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var identity =
/* #__PURE__ */
Control_Category.identity(Control_Category.categoryFn);

var discard = function discard(dict) {
  return dict.discard;
};

exports.discard = discard;
var bindProxy = {
  bind: function bind(v) {
    return function (v1) {
      return Type_Proxy["Proxy"].value;
    };
  },
  Apply0: function Apply0() {
    return Control_Apply.applyProxy;
  }
};
exports.bindProxy = bindProxy;
var bindFn = {
  bind: function bind(m) {
    return function (f) {
      return function (x) {
        return f(m(x))(x);
      };
    };
  },
  Apply0: function Apply0() {
    return Control_Apply.applyFn;
  }
};
exports.bindFn = bindFn;
var bindArray = {
  bind: $foreign.arrayBind,
  Apply0: function Apply0() {
    return Control_Apply.applyArray;
  }
};
exports.bindArray = bindArray;

var bind = function bind(dict) {
  return dict.bind;
};

exports.bind = bind;

var bindFlipped = function bindFlipped(dictBind) {
  return Data_Function.flip(bind(dictBind));
};

exports.bindFlipped = bindFlipped;

var composeKleisliFlipped = function composeKleisliFlipped(dictBind) {
  var bindFlipped1 = bindFlipped(dictBind);
  return function (f) {
    return function (g) {
      return function (a) {
        return bindFlipped1(f)(g(a));
      };
    };
  };
};

exports.composeKleisliFlipped = composeKleisliFlipped;

var composeKleisli = function composeKleisli(dictBind) {
  var bind1 = bind(dictBind);
  return function (f) {
    return function (g) {
      return function (a) {
        return bind1(f(a))(g);
      };
    };
  };
};

exports.composeKleisli = composeKleisli;
var discardProxy = {
  discard: function discard(dictBind) {
    return bind(dictBind);
  }
};
exports.discardProxy = discardProxy;
var discardUnit = {
  discard: function discard(dictBind) {
    return bind(dictBind);
  }
};
exports.discardUnit = discardUnit;

var ifM = function ifM(dictBind) {
  var bind1 = bind(dictBind);
  return function (cond) {
    return function (t) {
      return function (f) {
        return bind1(cond)(function (cond$prime) {
          if (cond$prime) {
            return t;
          }

          ;
          return f;
        });
      };
    };
  };
};

exports.ifM = ifM;

var join = function join(dictBind) {
  var bind1 = bind(dictBind);
  return function (m) {
    return bind1(m)(identity);
  };
};

exports.join = join;
},{"./foreign.js":"../output/Control.Bind/foreign.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Category/index.js":"../output/Control.Category/index.js","../Data.Function/index.js":"../output/Data.Function/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Type.Proxy/index.js":"../output/Type.Proxy/index.js"}],"../output/Control.Monad/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ap = void 0;
Object.defineProperty(exports, "apply", {
  enumerable: true,
  get: function () {
    return Control_Apply.apply;
  }
});
Object.defineProperty(exports, "bind", {
  enumerable: true,
  get: function () {
    return Control_Bind.bind;
  }
});
Object.defineProperty(exports, "ifM", {
  enumerable: true,
  get: function () {
    return Control_Bind.ifM;
  }
});
Object.defineProperty(exports, "join", {
  enumerable: true,
  get: function () {
    return Control_Bind.join;
  }
});
Object.defineProperty(exports, "liftA1", {
  enumerable: true,
  get: function () {
    return Control_Applicative.liftA1;
  }
});
exports.liftM1 = void 0;
Object.defineProperty(exports, "map", {
  enumerable: true,
  get: function () {
    return Data_Functor.map;
  }
});
exports.monadProxy = exports.monadFn = exports.monadArray = void 0;
Object.defineProperty(exports, "pure", {
  enumerable: true,
  get: function () {
    return Control_Applicative.pure;
  }
});
Object.defineProperty(exports, "unless", {
  enumerable: true,
  get: function () {
    return Control_Applicative.unless;
  }
});
exports.unlessM = void 0;
Object.defineProperty(exports, "void", {
  enumerable: true,
  get: function () {
    return Data_Functor.void;
  }
});
Object.defineProperty(exports, "when", {
  enumerable: true,
  get: function () {
    return Control_Applicative.when;
  }
});
exports.whenM = void 0;

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var whenM = function whenM(dictMonad) {
  var bind = Control_Bind.bind(dictMonad.Bind1());
  var when = Control_Applicative.when(dictMonad.Applicative0());
  return function (mb) {
    return function (m) {
      return bind(mb)(function (b) {
        return when(b)(m);
      });
    };
  };
};

exports.whenM = whenM;

var unlessM = function unlessM(dictMonad) {
  var bind = Control_Bind.bind(dictMonad.Bind1());
  var unless = Control_Applicative.unless(dictMonad.Applicative0());
  return function (mb) {
    return function (m) {
      return bind(mb)(function (b) {
        return unless(b)(m);
      });
    };
  };
};

exports.unlessM = unlessM;
var monadProxy = {
  Applicative0: function Applicative0() {
    return Control_Applicative.applicativeProxy;
  },
  Bind1: function Bind1() {
    return Control_Bind.bindProxy;
  }
};
exports.monadProxy = monadProxy;
var monadFn = {
  Applicative0: function Applicative0() {
    return Control_Applicative.applicativeFn;
  },
  Bind1: function Bind1() {
    return Control_Bind.bindFn;
  }
};
exports.monadFn = monadFn;
var monadArray = {
  Applicative0: function Applicative0() {
    return Control_Applicative.applicativeArray;
  },
  Bind1: function Bind1() {
    return Control_Bind.bindArray;
  }
};
exports.monadArray = monadArray;

var liftM1 = function liftM1(dictMonad) {
  var bind = Control_Bind.bind(dictMonad.Bind1());
  var pure = Control_Applicative.pure(dictMonad.Applicative0());
  return function (f) {
    return function (a) {
      return bind(a)(function (a$prime) {
        return pure(f(a$prime));
      });
    };
  };
};

exports.liftM1 = liftM1;

var ap = function ap(dictMonad) {
  var bind = Control_Bind.bind(dictMonad.Bind1());
  var pure = Control_Applicative.pure(dictMonad.Applicative0());
  return function (f) {
    return function (a) {
      return bind(f)(function (f$prime) {
        return bind(a)(function (a$prime) {
          return pure(f$prime(a$prime));
        });
      });
    };
  };
};

exports.ap = ap;
},{"../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js"}],"../output/Data.Semigroup/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.concatString = exports.concatArray = void 0;

var concatString = function concatString(s1) {
  return function (s2) {
    return s1 + s2;
  };
};

exports.concatString = concatString;

var concatArray = function concatArray(xs) {
  return function (ys) {
    if (xs.length === 0) return ys;
    if (ys.length === 0) return xs;
    return xs.concat(ys);
  };
};

exports.concatArray = concatArray;
},{}],"../output/Data.Symbol/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.unsafeCoerce = void 0;

// module Data.Symbol
var unsafeCoerce = function unsafeCoerce(arg) {
  return arg;
};

exports.unsafeCoerce = unsafeCoerce;
},{}],"../output/Data.Symbol/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.reifySymbol = exports.reflectSymbol = void 0;

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Type_Proxy = _interopRequireWildcard(require("../Type.Proxy/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var reifySymbol = function reifySymbol(s) {
  return function (f) {
    return $foreign.unsafeCoerce(function (dictIsSymbol) {
      return f(dictIsSymbol);
    })({
      reflectSymbol: function reflectSymbol(v) {
        return s;
      }
    })(Type_Proxy["Proxy"].value);
  };
};

exports.reifySymbol = reifySymbol;

var reflectSymbol = function reflectSymbol(dict) {
  return dict.reflectSymbol;
};

exports.reflectSymbol = reflectSymbol;
},{"./foreign.js":"../output/Data.Symbol/foreign.js","../Type.Proxy/index.js":"../output/Type.Proxy/index.js"}],"../output/Data.Void/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.absurd = void 0;

// Generated by purs version 0.15.10
var Void = function Void(x) {
  return x;
};

var absurd = function absurd(a) {
  var spin = function spin($copy_v) {
    var $tco_result;

    function $tco_loop(v) {
      $copy_v = v;
      return;
    }

    ;

    while (!false) {
      $tco_result = $tco_loop($copy_v);
    }

    ;
    return $tco_result;
  };

  return spin(a);
};

exports.absurd = absurd;
},{}],"../output/Record.Unsafe/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.unsafeSet = exports.unsafeHas = exports.unsafeGet = exports.unsafeDelete = void 0;

var unsafeHas = function unsafeHas(label) {
  return function (rec) {
    return {}.hasOwnProperty.call(rec, label);
  };
};

exports.unsafeHas = unsafeHas;

var unsafeGet = function unsafeGet(label) {
  return function (rec) {
    return rec[label];
  };
};

exports.unsafeGet = unsafeGet;

var unsafeSet = function unsafeSet(label) {
  return function (value) {
    return function (rec) {
      var copy = {};

      for (var key in rec) {
        if ({}.hasOwnProperty.call(rec, key)) {
          copy[key] = rec[key];
        }
      }

      copy[label] = value;
      return copy;
    };
  };
};

exports.unsafeSet = unsafeSet;

var unsafeDelete = function unsafeDelete(label) {
  return function (rec) {
    var copy = {};

    for (var key in rec) {
      if (key !== label && {}.hasOwnProperty.call(rec, key)) {
        copy[key] = rec[key];
      }
    }

    return copy;
  };
};

exports.unsafeDelete = unsafeDelete;
},{}],"../output/Record.Unsafe/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "unsafeDelete", {
  enumerable: true,
  get: function () {
    return $foreign.unsafeDelete;
  }
});
Object.defineProperty(exports, "unsafeGet", {
  enumerable: true,
  get: function () {
    return $foreign.unsafeGet;
  }
});
Object.defineProperty(exports, "unsafeHas", {
  enumerable: true,
  get: function () {
    return $foreign.unsafeHas;
  }
});
Object.defineProperty(exports, "unsafeSet", {
  enumerable: true,
  get: function () {
    return $foreign.unsafeSet;
  }
});

var $foreign = _interopRequireWildcard(require("./foreign.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }
},{"./foreign.js":"../output/Record.Unsafe/foreign.js"}],"../output/Data.Semigroup/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.semigroupVoid = exports.semigroupUnit = exports.semigroupString = exports.semigroupRecordNil = exports.semigroupRecordCons = exports.semigroupRecord = exports.semigroupProxy = exports.semigroupFn = exports.semigroupArray = exports.appendRecord = exports.append = void 0;

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Data_Symbol = _interopRequireWildcard(require("../Data.Symbol/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Data_Void = _interopRequireWildcard(require("../Data.Void/index.js"));

var Record_Unsafe = _interopRequireWildcard(require("../Record.Unsafe/index.js"));

var Type_Proxy = _interopRequireWildcard(require("../Type.Proxy/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var semigroupVoid = {
  append: function append(v) {
    return Data_Void.absurd;
  }
};
exports.semigroupVoid = semigroupVoid;
var semigroupUnit = {
  append: function append(v) {
    return function (v1) {
      return Data_Unit.unit;
    };
  }
};
exports.semigroupUnit = semigroupUnit;
var semigroupString = {
  append: $foreign.concatString
};
exports.semigroupString = semigroupString;
var semigroupRecordNil = {
  appendRecord: function appendRecord(v) {
    return function (v1) {
      return function (v2) {
        return {};
      };
    };
  }
};
exports.semigroupRecordNil = semigroupRecordNil;
var semigroupProxy = {
  append: function append(v) {
    return function (v1) {
      return Type_Proxy["Proxy"].value;
    };
  }
};
exports.semigroupProxy = semigroupProxy;
var semigroupArray = {
  append: $foreign.concatArray
};
exports.semigroupArray = semigroupArray;

var appendRecord = function appendRecord(dict) {
  return dict.appendRecord;
};

exports.appendRecord = appendRecord;

var semigroupRecord = function semigroupRecord() {
  return function (dictSemigroupRecord) {
    return {
      append: appendRecord(dictSemigroupRecord)(Type_Proxy["Proxy"].value)
    };
  };
};

exports.semigroupRecord = semigroupRecord;

var append = function append(dict) {
  return dict.append;
};

exports.append = append;

var semigroupFn = function semigroupFn(dictSemigroup) {
  var append1 = append(dictSemigroup);
  return {
    append: function append(f) {
      return function (g) {
        return function (x) {
          return append1(f(x))(g(x));
        };
      };
    }
  };
};

exports.semigroupFn = semigroupFn;

var semigroupRecordCons = function semigroupRecordCons(dictIsSymbol) {
  var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
  return function () {
    return function (dictSemigroupRecord) {
      var appendRecord1 = appendRecord(dictSemigroupRecord);
      return function (dictSemigroup) {
        var append1 = append(dictSemigroup);
        return {
          appendRecord: function appendRecord(v) {
            return function (ra) {
              return function (rb) {
                var tail = appendRecord1(Type_Proxy["Proxy"].value)(ra)(rb);
                var key = reflectSymbol(Type_Proxy["Proxy"].value);
                var insert = Record_Unsafe.unsafeSet(key);
                var get = Record_Unsafe.unsafeGet(key);
                return insert(append1(get(ra))(get(rb)))(tail);
              };
            };
          }
        };
      };
    };
  };
};

exports.semigroupRecordCons = semigroupRecordCons;
},{"./foreign.js":"../output/Data.Semigroup/foreign.js","../Data.Symbol/index.js":"../output/Data.Symbol/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Data.Void/index.js":"../output/Data.Void/index.js","../Record.Unsafe/index.js":"../output/Record.Unsafe/index.js","../Type.Proxy/index.js":"../output/Type.Proxy/index.js"}],"../output/Control.Alt/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.altArray = exports.alt = void 0;
Object.defineProperty(exports, "map", {
  enumerable: true,
  get: function () {
    return Data_Functor.map;
  }
});
Object.defineProperty(exports, "void", {
  enumerable: true,
  get: function () {
    return Data_Functor.void;
  }
});

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var altArray = {
  alt:
  /* #__PURE__ */
  Data_Semigroup.append(Data_Semigroup.semigroupArray),
  Functor0: function Functor0() {
    return Data_Functor.functorArray;
  }
};
exports.altArray = altArray;

var alt = function alt(dict) {
  return dict.alt;
};

exports.alt = alt;
},{"../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js"}],"../output/Data.Bounded/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.topNumber = exports.topInt = exports.topChar = exports.bottomNumber = exports.bottomInt = exports.bottomChar = void 0;
var topInt = 2147483647;
exports.topInt = topInt;
var bottomInt = -2147483648;
exports.bottomInt = bottomInt;
var topChar = String.fromCharCode(65535);
exports.topChar = topChar;
var bottomChar = String.fromCharCode(0);
exports.bottomChar = bottomChar;
var topNumber = Number.POSITIVE_INFINITY;
exports.topNumber = topNumber;
var bottomNumber = Number.NEGATIVE_INFINITY;
exports.bottomNumber = bottomNumber;
},{}],"../output/Data.Ord/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ordStringImpl = exports.ordNumberImpl = exports.ordIntImpl = exports.ordCharImpl = exports.ordBooleanImpl = exports.ordArrayImpl = void 0;

var unsafeCompareImpl = function unsafeCompareImpl(lt) {
  return function (eq) {
    return function (gt) {
      return function (x) {
        return function (y) {
          return x < y ? lt : x === y ? eq : gt;
        };
      };
    };
  };
};

var ordBooleanImpl = unsafeCompareImpl;
exports.ordBooleanImpl = ordBooleanImpl;
var ordIntImpl = unsafeCompareImpl;
exports.ordIntImpl = ordIntImpl;
var ordNumberImpl = unsafeCompareImpl;
exports.ordNumberImpl = ordNumberImpl;
var ordStringImpl = unsafeCompareImpl;
exports.ordStringImpl = ordStringImpl;
var ordCharImpl = unsafeCompareImpl;
exports.ordCharImpl = ordCharImpl;

var ordArrayImpl = function ordArrayImpl(f) {
  return function (xs) {
    return function (ys) {
      var i = 0;
      var xlen = xs.length;
      var ylen = ys.length;

      while (i < xlen && i < ylen) {
        var x = xs[i];
        var y = ys[i];
        var o = f(x)(y);

        if (o !== 0) {
          return o;
        }

        i++;
      }

      if (xlen === ylen) {
        return 0;
      } else if (xlen > ylen) {
        return -1;
      } else {
        return 1;
      }
    };
  };
};

exports.ordArrayImpl = ordArrayImpl;
},{}],"../output/Data.Eq/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.eqStringImpl = exports.eqNumberImpl = exports.eqIntImpl = exports.eqCharImpl = exports.eqBooleanImpl = exports.eqArrayImpl = void 0;

var refEq = function refEq(r1) {
  return function (r2) {
    return r1 === r2;
  };
};

var eqBooleanImpl = refEq;
exports.eqBooleanImpl = eqBooleanImpl;
var eqIntImpl = refEq;
exports.eqIntImpl = eqIntImpl;
var eqNumberImpl = refEq;
exports.eqNumberImpl = eqNumberImpl;
var eqCharImpl = refEq;
exports.eqCharImpl = eqCharImpl;
var eqStringImpl = refEq;
exports.eqStringImpl = eqStringImpl;

var eqArrayImpl = function eqArrayImpl(f) {
  return function (xs) {
    return function (ys) {
      if (xs.length !== ys.length) return false;

      for (var i = 0; i < xs.length; i++) {
        if (!f(xs[i])(ys[i])) return false;
      }

      return true;
    };
  };
};

exports.eqArrayImpl = eqArrayImpl;
},{}],"../output/Data.Eq/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.notEq1 = exports.notEq = exports.eqVoid = exports.eqUnit = exports.eqString = exports.eqRowNil = exports.eqRowCons = exports.eqRecord = exports.eqRec = exports.eqProxy = exports.eqNumber = exports.eqInt = exports.eqChar = exports.eqBoolean = exports.eqArray = exports.eq1Array = exports.eq1 = exports.eq = void 0;

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Data_Symbol = _interopRequireWildcard(require("../Data.Symbol/index.js"));

var Record_Unsafe = _interopRequireWildcard(require("../Record.Unsafe/index.js"));

var Type_Proxy = _interopRequireWildcard(require("../Type.Proxy/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var eqVoid = {
  eq: function eq(v) {
    return function (v1) {
      return true;
    };
  }
};
exports.eqVoid = eqVoid;
var eqUnit = {
  eq: function eq(v) {
    return function (v1) {
      return true;
    };
  }
};
exports.eqUnit = eqUnit;
var eqString = {
  eq: $foreign.eqStringImpl
};
exports.eqString = eqString;
var eqRowNil = {
  eqRecord: function eqRecord(v) {
    return function (v1) {
      return function (v2) {
        return true;
      };
    };
  }
};
exports.eqRowNil = eqRowNil;

var eqRecord = function eqRecord(dict) {
  return dict.eqRecord;
};

exports.eqRecord = eqRecord;

var eqRec = function eqRec() {
  return function (dictEqRecord) {
    return {
      eq: eqRecord(dictEqRecord)(Type_Proxy["Proxy"].value)
    };
  };
};

exports.eqRec = eqRec;
var eqProxy = {
  eq: function eq(v) {
    return function (v1) {
      return true;
    };
  }
};
exports.eqProxy = eqProxy;
var eqNumber = {
  eq: $foreign.eqNumberImpl
};
exports.eqNumber = eqNumber;
var eqInt = {
  eq: $foreign.eqIntImpl
};
exports.eqInt = eqInt;
var eqChar = {
  eq: $foreign.eqCharImpl
};
exports.eqChar = eqChar;
var eqBoolean = {
  eq: $foreign.eqBooleanImpl
};
exports.eqBoolean = eqBoolean;

var eq1 = function eq1(dict) {
  return dict.eq1;
};

exports.eq1 = eq1;

var eq = function eq(dict) {
  return dict.eq;
};

exports.eq = eq;
var eq2 =
/* #__PURE__ */
eq(eqBoolean);

var eqArray = function eqArray(dictEq) {
  return {
    eq: $foreign.eqArrayImpl(eq(dictEq))
  };
};

exports.eqArray = eqArray;
var eq1Array = {
  eq1: function eq1(dictEq) {
    return eq(eqArray(dictEq));
  }
};
exports.eq1Array = eq1Array;

var eqRowCons = function eqRowCons(dictEqRecord) {
  var eqRecord1 = eqRecord(dictEqRecord);
  return function () {
    return function (dictIsSymbol) {
      var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
      return function (dictEq) {
        var eq3 = eq(dictEq);
        return {
          eqRecord: function eqRecord(v) {
            return function (ra) {
              return function (rb) {
                var tail = eqRecord1(Type_Proxy["Proxy"].value)(ra)(rb);
                var key = reflectSymbol(Type_Proxy["Proxy"].value);
                var get = Record_Unsafe.unsafeGet(key);
                return eq3(get(ra))(get(rb)) && tail;
              };
            };
          }
        };
      };
    };
  };
};

exports.eqRowCons = eqRowCons;

var notEq = function notEq(dictEq) {
  var eq3 = eq(dictEq);
  return function (x) {
    return function (y) {
      return eq2(eq3(x)(y))(false);
    };
  };
};

exports.notEq = notEq;

var notEq1 = function notEq1(dictEq1) {
  var eq11 = eq1(dictEq1);
  return function (dictEq) {
    var eq12 = eq11(dictEq);
    return function (x) {
      return function (y) {
        return eq2(eq12(x)(y))(false);
      };
    };
  };
};

exports.notEq1 = notEq1;
},{"./foreign.js":"../output/Data.Eq/foreign.js","../Data.Symbol/index.js":"../output/Data.Symbol/index.js","../Record.Unsafe/index.js":"../output/Record.Unsafe/index.js","../Type.Proxy/index.js":"../output/Type.Proxy/index.js"}],"../output/Data.Ordering/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showOrdering = exports.semigroupOrdering = exports.invert = exports.eqOrdering = exports.LT = exports.GT = exports.EQ = void 0;

// Generated by purs version 0.15.10
var LT =
/* #__PURE__ */
function () {
  function LT() {}

  ;
  LT.value = new LT();
  return LT;
}();

exports.LT = LT;

var GT =
/* #__PURE__ */
function () {
  function GT() {}

  ;
  GT.value = new GT();
  return GT;
}();

exports.GT = GT;

var EQ =
/* #__PURE__ */
function () {
  function EQ() {}

  ;
  EQ.value = new EQ();
  return EQ;
}();

exports.EQ = EQ;
var showOrdering = {
  show: function show(v) {
    if (v instanceof LT) {
      return "LT";
    }

    ;

    if (v instanceof GT) {
      return "GT";
    }

    ;

    if (v instanceof EQ) {
      return "EQ";
    }

    ;
    throw new Error("Failed pattern match at Data.Ordering (line 26, column 1 - line 29, column 17): " + [v.constructor.name]);
  }
};
exports.showOrdering = showOrdering;
var semigroupOrdering = {
  append: function append(v) {
    return function (v1) {
      if (v instanceof LT) {
        return LT.value;
      }

      ;

      if (v instanceof GT) {
        return GT.value;
      }

      ;

      if (v instanceof EQ) {
        return v1;
      }

      ;
      throw new Error("Failed pattern match at Data.Ordering (line 21, column 1 - line 24, column 18): " + [v.constructor.name, v1.constructor.name]);
    };
  }
};
exports.semigroupOrdering = semigroupOrdering;

var invert = function invert(v) {
  if (v instanceof GT) {
    return LT.value;
  }

  ;

  if (v instanceof EQ) {
    return EQ.value;
  }

  ;

  if (v instanceof LT) {
    return GT.value;
  }

  ;
  throw new Error("Failed pattern match at Data.Ordering (line 33, column 1 - line 33, column 31): " + [v.constructor.name]);
};

exports.invert = invert;
var eqOrdering = {
  eq: function eq(v) {
    return function (v1) {
      if (v instanceof LT && v1 instanceof LT) {
        return true;
      }

      ;

      if (v instanceof GT && v1 instanceof GT) {
        return true;
      }

      ;

      if (v instanceof EQ && v1 instanceof EQ) {
        return true;
      }

      ;
      return false;
    };
  }
};
exports.eqOrdering = eqOrdering;
},{}],"../output/Data.Ring/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.numSub = exports.intSub = void 0;

var intSub = function intSub(x) {
  return function (y) {
    /* jshint bitwise: false */
    return x - y | 0;
  };
};

exports.intSub = intSub;

var numSub = function numSub(n1) {
  return function (n2) {
    return n1 - n2;
  };
};

exports.numSub = numSub;
},{}],"../output/Data.Semiring/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.numMul = exports.numAdd = exports.intMul = exports.intAdd = void 0;

var intAdd = function intAdd(x) {
  return function (y) {
    /* jshint bitwise: false */
    return x + y | 0;
  };
};

exports.intAdd = intAdd;

var intMul = function intMul(x) {
  return function (y) {
    /* jshint bitwise: false */
    return x * y | 0;
  };
};

exports.intMul = intMul;

var numAdd = function numAdd(n1) {
  return function (n2) {
    return n1 + n2;
  };
};

exports.numAdd = numAdd;

var numMul = function numMul(n1) {
  return function (n2) {
    return n1 * n2;
  };
};

exports.numMul = numMul;
},{}],"../output/Data.Semiring/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.zeroRecord = exports.zero = exports.semiringUnit = exports.semiringRecordNil = exports.semiringRecordCons = exports.semiringRecord = exports.semiringProxy = exports.semiringNumber = exports.semiringInt = exports.semiringFn = exports.oneRecord = exports.one = exports.mulRecord = exports.mul = exports.addRecord = exports.add = void 0;

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Data_Symbol = _interopRequireWildcard(require("../Data.Symbol/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Record_Unsafe = _interopRequireWildcard(require("../Record.Unsafe/index.js"));

var Type_Proxy = _interopRequireWildcard(require("../Type.Proxy/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var zeroRecord = function zeroRecord(dict) {
  return dict.zeroRecord;
};

exports.zeroRecord = zeroRecord;

var zero = function zero(dict) {
  return dict.zero;
};

exports.zero = zero;
var semiringUnit = {
  add: function add(v) {
    return function (v1) {
      return Data_Unit.unit;
    };
  },
  zero: Data_Unit.unit,
  mul: function mul(v) {
    return function (v1) {
      return Data_Unit.unit;
    };
  },
  one: Data_Unit.unit
};
exports.semiringUnit = semiringUnit;
var semiringRecordNil = {
  addRecord: function addRecord(v) {
    return function (v1) {
      return function (v2) {
        return {};
      };
    };
  },
  mulRecord: function mulRecord(v) {
    return function (v1) {
      return function (v2) {
        return {};
      };
    };
  },
  oneRecord: function oneRecord(v) {
    return function (v1) {
      return {};
    };
  },
  zeroRecord: function zeroRecord(v) {
    return function (v1) {
      return {};
    };
  }
};
exports.semiringRecordNil = semiringRecordNil;

var semiringProxy =
/* #__PURE__ */
function () {
  return {
    add: function add(v) {
      return function (v1) {
        return Type_Proxy["Proxy"].value;
      };
    },
    mul: function mul(v) {
      return function (v1) {
        return Type_Proxy["Proxy"].value;
      };
    },
    one: Type_Proxy["Proxy"].value,
    zero: Type_Proxy["Proxy"].value
  };
}();

exports.semiringProxy = semiringProxy;
var semiringNumber = {
  add: $foreign.numAdd,
  zero: 0.0,
  mul: $foreign.numMul,
  one: 1.0
};
exports.semiringNumber = semiringNumber;
var semiringInt = {
  add: $foreign.intAdd,
  zero: 0,
  mul: $foreign.intMul,
  one: 1
};
exports.semiringInt = semiringInt;

var oneRecord = function oneRecord(dict) {
  return dict.oneRecord;
};

exports.oneRecord = oneRecord;

var one = function one(dict) {
  return dict.one;
};

exports.one = one;

var mulRecord = function mulRecord(dict) {
  return dict.mulRecord;
};

exports.mulRecord = mulRecord;

var mul = function mul(dict) {
  return dict.mul;
};

exports.mul = mul;

var addRecord = function addRecord(dict) {
  return dict.addRecord;
};

exports.addRecord = addRecord;

var semiringRecord = function semiringRecord() {
  return function (dictSemiringRecord) {
    return {
      add: addRecord(dictSemiringRecord)(Type_Proxy["Proxy"].value),
      mul: mulRecord(dictSemiringRecord)(Type_Proxy["Proxy"].value),
      one: oneRecord(dictSemiringRecord)(Type_Proxy["Proxy"].value)(Type_Proxy["Proxy"].value),
      zero: zeroRecord(dictSemiringRecord)(Type_Proxy["Proxy"].value)(Type_Proxy["Proxy"].value)
    };
  };
};

exports.semiringRecord = semiringRecord;

var add = function add(dict) {
  return dict.add;
};

exports.add = add;

var semiringFn = function semiringFn(dictSemiring) {
  var add1 = add(dictSemiring);
  var zero1 = zero(dictSemiring);
  var mul1 = mul(dictSemiring);
  var one1 = one(dictSemiring);
  return {
    add: function add(f) {
      return function (g) {
        return function (x) {
          return add1(f(x))(g(x));
        };
      };
    },
    zero: function zero(v) {
      return zero1;
    },
    mul: function mul(f) {
      return function (g) {
        return function (x) {
          return mul1(f(x))(g(x));
        };
      };
    },
    one: function one(v) {
      return one1;
    }
  };
};

exports.semiringFn = semiringFn;

var semiringRecordCons = function semiringRecordCons(dictIsSymbol) {
  var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
  return function () {
    return function (dictSemiringRecord) {
      var addRecord1 = addRecord(dictSemiringRecord);
      var mulRecord1 = mulRecord(dictSemiringRecord);
      var oneRecord1 = oneRecord(dictSemiringRecord);
      var zeroRecord1 = zeroRecord(dictSemiringRecord);
      return function (dictSemiring) {
        var add1 = add(dictSemiring);
        var mul1 = mul(dictSemiring);
        var one1 = one(dictSemiring);
        var zero1 = zero(dictSemiring);
        return {
          addRecord: function addRecord(v) {
            return function (ra) {
              return function (rb) {
                var tail = addRecord1(Type_Proxy["Proxy"].value)(ra)(rb);
                var key = reflectSymbol(Type_Proxy["Proxy"].value);
                var insert = Record_Unsafe.unsafeSet(key);
                var get = Record_Unsafe.unsafeGet(key);
                return insert(add1(get(ra))(get(rb)))(tail);
              };
            };
          },
          mulRecord: function mulRecord(v) {
            return function (ra) {
              return function (rb) {
                var tail = mulRecord1(Type_Proxy["Proxy"].value)(ra)(rb);
                var key = reflectSymbol(Type_Proxy["Proxy"].value);
                var insert = Record_Unsafe.unsafeSet(key);
                var get = Record_Unsafe.unsafeGet(key);
                return insert(mul1(get(ra))(get(rb)))(tail);
              };
            };
          },
          oneRecord: function oneRecord(v) {
            return function (v1) {
              var tail = oneRecord1(Type_Proxy["Proxy"].value)(Type_Proxy["Proxy"].value);
              var key = reflectSymbol(Type_Proxy["Proxy"].value);
              var insert = Record_Unsafe.unsafeSet(key);
              return insert(one1)(tail);
            };
          },
          zeroRecord: function zeroRecord(v) {
            return function (v1) {
              var tail = zeroRecord1(Type_Proxy["Proxy"].value)(Type_Proxy["Proxy"].value);
              var key = reflectSymbol(Type_Proxy["Proxy"].value);
              var insert = Record_Unsafe.unsafeSet(key);
              return insert(zero1)(tail);
            };
          }
        };
      };
    };
  };
};

exports.semiringRecordCons = semiringRecordCons;
},{"./foreign.js":"../output/Data.Semiring/foreign.js","../Data.Symbol/index.js":"../output/Data.Symbol/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Record.Unsafe/index.js":"../output/Record.Unsafe/index.js","../Type.Proxy/index.js":"../output/Type.Proxy/index.js"}],"../output/Data.Ring/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "add", {
  enumerable: true,
  get: function () {
    return Data_Semiring.add;
  }
});
Object.defineProperty(exports, "mul", {
  enumerable: true,
  get: function () {
    return Data_Semiring.mul;
  }
});
exports.negate = void 0;
Object.defineProperty(exports, "one", {
  enumerable: true,
  get: function () {
    return Data_Semiring.one;
  }
});
exports.subRecord = exports.sub = exports.ringUnit = exports.ringRecordNil = exports.ringRecordCons = exports.ringRecord = exports.ringProxy = exports.ringNumber = exports.ringInt = exports.ringFn = void 0;
Object.defineProperty(exports, "zero", {
  enumerable: true,
  get: function () {
    return Data_Semiring.zero;
  }
});

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Data_Semiring = _interopRequireWildcard(require("../Data.Semiring/index.js"));

var Data_Symbol = _interopRequireWildcard(require("../Data.Symbol/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Record_Unsafe = _interopRequireWildcard(require("../Record.Unsafe/index.js"));

var Type_Proxy = _interopRequireWildcard(require("../Type.Proxy/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var semiringRecord =
/* #__PURE__ */
Data_Semiring.semiringRecord();

var subRecord = function subRecord(dict) {
  return dict.subRecord;
};

exports.subRecord = subRecord;

var sub = function sub(dict) {
  return dict.sub;
};

exports.sub = sub;
var ringUnit = {
  sub: function sub(v) {
    return function (v1) {
      return Data_Unit.unit;
    };
  },
  Semiring0: function Semiring0() {
    return Data_Semiring.semiringUnit;
  }
};
exports.ringUnit = ringUnit;
var ringRecordNil = {
  subRecord: function subRecord(v) {
    return function (v1) {
      return function (v2) {
        return {};
      };
    };
  },
  SemiringRecord0: function SemiringRecord0() {
    return Data_Semiring.semiringRecordNil;
  }
};
exports.ringRecordNil = ringRecordNil;

var ringRecordCons = function ringRecordCons(dictIsSymbol) {
  var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
  var semiringRecordCons = Data_Semiring.semiringRecordCons(dictIsSymbol)();
  return function () {
    return function (dictRingRecord) {
      var subRecord1 = subRecord(dictRingRecord);
      var semiringRecordCons1 = semiringRecordCons(dictRingRecord.SemiringRecord0());
      return function (dictRing) {
        var sub1 = sub(dictRing);
        var semiringRecordCons2 = semiringRecordCons1(dictRing.Semiring0());
        return {
          subRecord: function subRecord(v) {
            return function (ra) {
              return function (rb) {
                var tail = subRecord1(Type_Proxy["Proxy"].value)(ra)(rb);
                var key = reflectSymbol(Type_Proxy["Proxy"].value);
                var insert = Record_Unsafe.unsafeSet(key);
                var get = Record_Unsafe.unsafeGet(key);
                return insert(sub1(get(ra))(get(rb)))(tail);
              };
            };
          },
          SemiringRecord0: function SemiringRecord0() {
            return semiringRecordCons2;
          }
        };
      };
    };
  };
};

exports.ringRecordCons = ringRecordCons;

var ringRecord = function ringRecord() {
  return function (dictRingRecord) {
    var semiringRecord1 = semiringRecord(dictRingRecord.SemiringRecord0());
    return {
      sub: subRecord(dictRingRecord)(Type_Proxy["Proxy"].value),
      Semiring0: function Semiring0() {
        return semiringRecord1;
      }
    };
  };
};

exports.ringRecord = ringRecord;
var ringProxy = {
  sub: function sub(v) {
    return function (v1) {
      return Type_Proxy["Proxy"].value;
    };
  },
  Semiring0: function Semiring0() {
    return Data_Semiring.semiringProxy;
  }
};
exports.ringProxy = ringProxy;
var ringNumber = {
  sub: $foreign.numSub,
  Semiring0: function Semiring0() {
    return Data_Semiring.semiringNumber;
  }
};
exports.ringNumber = ringNumber;
var ringInt = {
  sub: $foreign.intSub,
  Semiring0: function Semiring0() {
    return Data_Semiring.semiringInt;
  }
};
exports.ringInt = ringInt;

var ringFn = function ringFn(dictRing) {
  var sub1 = sub(dictRing);
  var semiringFn = Data_Semiring.semiringFn(dictRing.Semiring0());
  return {
    sub: function sub(f) {
      return function (g) {
        return function (x) {
          return sub1(f(x))(g(x));
        };
      };
    },
    Semiring0: function Semiring0() {
      return semiringFn;
    }
  };
};

exports.ringFn = ringFn;

var negate = function negate(dictRing) {
  var sub1 = sub(dictRing);
  var zero = Data_Semiring.zero(dictRing.Semiring0());
  return function (a) {
    return sub1(zero)(a);
  };
};

exports.negate = negate;
},{"./foreign.js":"../output/Data.Ring/foreign.js","../Data.Semiring/index.js":"../output/Data.Semiring/index.js","../Data.Symbol/index.js":"../output/Data.Symbol/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Record.Unsafe/index.js":"../output/Record.Unsafe/index.js","../Type.Proxy/index.js":"../output/Type.Proxy/index.js"}],"../output/Data.Ord/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "EQ", {
  enumerable: true,
  get: function () {
    return Data_Ordering.EQ;
  }
});
Object.defineProperty(exports, "GT", {
  enumerable: true,
  get: function () {
    return Data_Ordering.GT;
  }
});
Object.defineProperty(exports, "LT", {
  enumerable: true,
  get: function () {
    return Data_Ordering.LT;
  }
});
exports.signum = exports.ordVoid = exports.ordUnit = exports.ordString = exports.ordRecordNil = exports.ordRecordCons = exports.ordRecord = exports.ordProxy = exports.ordOrdering = exports.ordNumber = exports.ordInt = exports.ordChar = exports.ordBoolean = exports.ordArray = exports.ord1Array = exports.min = exports.max = exports.lessThanOrEq = exports.lessThan = exports.greaterThanOrEq = exports.greaterThan = exports.comparing = exports.compareRecord = exports.compare1 = exports.compare = exports.clamp = exports.between = exports.abs = void 0;

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_Ordering = _interopRequireWildcard(require("../Data.Ordering/index.js"));

var Data_Ring = _interopRequireWildcard(require("../Data.Ring/index.js"));

var Data_Semiring = _interopRequireWildcard(require("../Data.Semiring/index.js"));

var Data_Symbol = _interopRequireWildcard(require("../Data.Symbol/index.js"));

var Record_Unsafe = _interopRequireWildcard(require("../Record.Unsafe/index.js"));

var Type_Proxy = _interopRequireWildcard(require("../Type.Proxy/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var eqRec =
/* #__PURE__ */
Data_Eq.eqRec();
var notEq =
/* #__PURE__ */
Data_Eq.notEq(Data_Ordering.eqOrdering);
var ordVoid = {
  compare: function compare(v) {
    return function (v1) {
      return Data_Ordering.EQ.value;
    };
  },
  Eq0: function Eq0() {
    return Data_Eq.eqVoid;
  }
};
exports.ordVoid = ordVoid;
var ordUnit = {
  compare: function compare(v) {
    return function (v1) {
      return Data_Ordering.EQ.value;
    };
  },
  Eq0: function Eq0() {
    return Data_Eq.eqUnit;
  }
};
exports.ordUnit = ordUnit;

var ordString =
/* #__PURE__ */
function () {
  return {
    compare: $foreign.ordStringImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value),
    Eq0: function Eq0() {
      return Data_Eq.eqString;
    }
  };
}();

exports.ordString = ordString;
var ordRecordNil = {
  compareRecord: function compareRecord(v) {
    return function (v1) {
      return function (v2) {
        return Data_Ordering.EQ.value;
      };
    };
  },
  EqRecord0: function EqRecord0() {
    return Data_Eq.eqRowNil;
  }
};
exports.ordRecordNil = ordRecordNil;
var ordProxy = {
  compare: function compare(v) {
    return function (v1) {
      return Data_Ordering.EQ.value;
    };
  },
  Eq0: function Eq0() {
    return Data_Eq.eqProxy;
  }
};
exports.ordProxy = ordProxy;
var ordOrdering = {
  compare: function compare(v) {
    return function (v1) {
      if (v instanceof Data_Ordering.LT && v1 instanceof Data_Ordering.LT) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (v instanceof Data_Ordering.EQ && v1 instanceof Data_Ordering.EQ) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (v instanceof Data_Ordering.GT && v1 instanceof Data_Ordering.GT) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (v instanceof Data_Ordering.LT) {
        return Data_Ordering.LT.value;
      }

      ;

      if (v instanceof Data_Ordering.EQ && v1 instanceof Data_Ordering.LT) {
        return Data_Ordering.GT.value;
      }

      ;

      if (v instanceof Data_Ordering.EQ && v1 instanceof Data_Ordering.GT) {
        return Data_Ordering.LT.value;
      }

      ;

      if (v instanceof Data_Ordering.GT) {
        return Data_Ordering.GT.value;
      }

      ;
      throw new Error("Failed pattern match at Data.Ord (line 126, column 1 - line 133, column 20): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Eq0: function Eq0() {
    return Data_Ordering.eqOrdering;
  }
};
exports.ordOrdering = ordOrdering;

var ordNumber =
/* #__PURE__ */
function () {
  return {
    compare: $foreign.ordNumberImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value),
    Eq0: function Eq0() {
      return Data_Eq.eqNumber;
    }
  };
}();

exports.ordNumber = ordNumber;

var ordInt =
/* #__PURE__ */
function () {
  return {
    compare: $foreign.ordIntImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value),
    Eq0: function Eq0() {
      return Data_Eq.eqInt;
    }
  };
}();

exports.ordInt = ordInt;

var ordChar =
/* #__PURE__ */
function () {
  return {
    compare: $foreign.ordCharImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value),
    Eq0: function Eq0() {
      return Data_Eq.eqChar;
    }
  };
}();

exports.ordChar = ordChar;

var ordBoolean =
/* #__PURE__ */
function () {
  return {
    compare: $foreign.ordBooleanImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value),
    Eq0: function Eq0() {
      return Data_Eq.eqBoolean;
    }
  };
}();

exports.ordBoolean = ordBoolean;

var compareRecord = function compareRecord(dict) {
  return dict.compareRecord;
};

exports.compareRecord = compareRecord;

var ordRecord = function ordRecord() {
  return function (dictOrdRecord) {
    var eqRec1 = eqRec(dictOrdRecord.EqRecord0());
    return {
      compare: compareRecord(dictOrdRecord)(Type_Proxy["Proxy"].value),
      Eq0: function Eq0() {
        return eqRec1;
      }
    };
  };
};

exports.ordRecord = ordRecord;

var compare1 = function compare1(dict) {
  return dict.compare1;
};

exports.compare1 = compare1;

var compare = function compare(dict) {
  return dict.compare;
};

exports.compare = compare;
var compare2 =
/* #__PURE__ */
compare(ordInt);

var comparing = function comparing(dictOrd) {
  var compare3 = compare(dictOrd);
  return function (f) {
    return function (x) {
      return function (y) {
        return compare3(f(x))(f(y));
      };
    };
  };
};

exports.comparing = comparing;

var greaterThan = function greaterThan(dictOrd) {
  var compare3 = compare(dictOrd);
  return function (a1) {
    return function (a2) {
      var v = compare3(a1)(a2);

      if (v instanceof Data_Ordering.GT) {
        return true;
      }

      ;
      return false;
    };
  };
};

exports.greaterThan = greaterThan;

var greaterThanOrEq = function greaterThanOrEq(dictOrd) {
  var compare3 = compare(dictOrd);
  return function (a1) {
    return function (a2) {
      var v = compare3(a1)(a2);

      if (v instanceof Data_Ordering.LT) {
        return false;
      }

      ;
      return true;
    };
  };
};

exports.greaterThanOrEq = greaterThanOrEq;

var lessThan = function lessThan(dictOrd) {
  var compare3 = compare(dictOrd);
  return function (a1) {
    return function (a2) {
      var v = compare3(a1)(a2);

      if (v instanceof Data_Ordering.LT) {
        return true;
      }

      ;
      return false;
    };
  };
};

exports.lessThan = lessThan;

var signum = function signum(dictOrd) {
  var lessThan1 = lessThan(dictOrd);
  var greaterThan1 = greaterThan(dictOrd);
  return function (dictRing) {
    var Semiring0 = dictRing.Semiring0();
    var zero = Data_Semiring.zero(Semiring0);
    var negate1 = Data_Ring.negate(dictRing);
    var one = Data_Semiring.one(Semiring0);
    return function (x) {
      var $89 = lessThan1(x)(zero);

      if ($89) {
        return negate1(one);
      }

      ;
      var $90 = greaterThan1(x)(zero);

      if ($90) {
        return one;
      }

      ;
      return x;
    };
  };
};

exports.signum = signum;

var lessThanOrEq = function lessThanOrEq(dictOrd) {
  var compare3 = compare(dictOrd);
  return function (a1) {
    return function (a2) {
      var v = compare3(a1)(a2);

      if (v instanceof Data_Ordering.GT) {
        return false;
      }

      ;
      return true;
    };
  };
};

exports.lessThanOrEq = lessThanOrEq;

var max = function max(dictOrd) {
  var compare3 = compare(dictOrd);
  return function (x) {
    return function (y) {
      var v = compare3(x)(y);

      if (v instanceof Data_Ordering.LT) {
        return y;
      }

      ;

      if (v instanceof Data_Ordering.EQ) {
        return x;
      }

      ;

      if (v instanceof Data_Ordering.GT) {
        return x;
      }

      ;
      throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): " + [v.constructor.name]);
    };
  };
};

exports.max = max;

var min = function min(dictOrd) {
  var compare3 = compare(dictOrd);
  return function (x) {
    return function (y) {
      var v = compare3(x)(y);

      if (v instanceof Data_Ordering.LT) {
        return x;
      }

      ;

      if (v instanceof Data_Ordering.EQ) {
        return x;
      }

      ;

      if (v instanceof Data_Ordering.GT) {
        return y;
      }

      ;
      throw new Error("Failed pattern match at Data.Ord (line 172, column 3 - line 175, column 12): " + [v.constructor.name]);
    };
  };
};

exports.min = min;

var ordArray = function ordArray(dictOrd) {
  var compare3 = compare(dictOrd);
  var eqArray = Data_Eq.eqArray(dictOrd.Eq0());
  return {
    compare: function () {
      var toDelta = function toDelta(x) {
        return function (y) {
          var v = compare3(x)(y);

          if (v instanceof Data_Ordering.EQ) {
            return 0;
          }

          ;

          if (v instanceof Data_Ordering.LT) {
            return 1;
          }

          ;

          if (v instanceof Data_Ordering.GT) {
            return -1 | 0;
          }

          ;
          throw new Error("Failed pattern match at Data.Ord (line 79, column 7 - line 82, column 17): " + [v.constructor.name]);
        };
      };

      return function (xs) {
        return function (ys) {
          return compare2(0)($foreign.ordArrayImpl(toDelta)(xs)(ys));
        };
      };
    }(),
    Eq0: function Eq0() {
      return eqArray;
    }
  };
};

exports.ordArray = ordArray;
var ord1Array = {
  compare1: function compare1(dictOrd) {
    return compare(ordArray(dictOrd));
  },
  Eq10: function Eq10() {
    return Data_Eq.eq1Array;
  }
};
exports.ord1Array = ord1Array;

var ordRecordCons = function ordRecordCons(dictOrdRecord) {
  var compareRecord1 = compareRecord(dictOrdRecord);
  var eqRowCons = Data_Eq.eqRowCons(dictOrdRecord.EqRecord0())();
  return function () {
    return function (dictIsSymbol) {
      var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
      var eqRowCons1 = eqRowCons(dictIsSymbol);
      return function (dictOrd) {
        var compare3 = compare(dictOrd);
        var eqRowCons2 = eqRowCons1(dictOrd.Eq0());
        return {
          compareRecord: function compareRecord(v) {
            return function (ra) {
              return function (rb) {
                var key = reflectSymbol(Type_Proxy["Proxy"].value);
                var left = compare3(Record_Unsafe.unsafeGet(key)(ra))(Record_Unsafe.unsafeGet(key)(rb));
                var $95 = notEq(left)(Data_Ordering.EQ.value);

                if ($95) {
                  return left;
                }

                ;
                return compareRecord1(Type_Proxy["Proxy"].value)(ra)(rb);
              };
            };
          },
          EqRecord0: function EqRecord0() {
            return eqRowCons2;
          }
        };
      };
    };
  };
};

exports.ordRecordCons = ordRecordCons;

var clamp = function clamp(dictOrd) {
  var min1 = min(dictOrd);
  var max1 = max(dictOrd);
  return function (low) {
    return function (hi) {
      return function (x) {
        return min1(hi)(max1(low)(x));
      };
    };
  };
};

exports.clamp = clamp;

var between = function between(dictOrd) {
  var lessThan1 = lessThan(dictOrd);
  var greaterThan1 = greaterThan(dictOrd);
  return function (low) {
    return function (hi) {
      return function (x) {
        if (lessThan1(x)(low)) {
          return false;
        }

        ;

        if (greaterThan1(x)(hi)) {
          return false;
        }

        ;
        return true;
      };
    };
  };
};

exports.between = between;

var abs = function abs(dictOrd) {
  var greaterThanOrEq1 = greaterThanOrEq(dictOrd);
  return function (dictRing) {
    var zero = Data_Semiring.zero(dictRing.Semiring0());
    var negate1 = Data_Ring.negate(dictRing);
    return function (x) {
      var $99 = greaterThanOrEq1(x)(zero);

      if ($99) {
        return x;
      }

      ;
      return negate1(x);
    };
  };
};

exports.abs = abs;
},{"./foreign.js":"../output/Data.Ord/foreign.js","../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.Ordering/index.js":"../output/Data.Ordering/index.js","../Data.Ring/index.js":"../output/Data.Ring/index.js","../Data.Semiring/index.js":"../output/Data.Semiring/index.js","../Data.Symbol/index.js":"../output/Data.Symbol/index.js","../Record.Unsafe/index.js":"../output/Record.Unsafe/index.js","../Type.Proxy/index.js":"../output/Type.Proxy/index.js"}],"../output/Data.Bounded/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "EQ", {
  enumerable: true,
  get: function () {
    return Data_Ord.EQ;
  }
});
Object.defineProperty(exports, "GT", {
  enumerable: true,
  get: function () {
    return Data_Ord.GT;
  }
});
Object.defineProperty(exports, "LT", {
  enumerable: true,
  get: function () {
    return Data_Ord.LT;
  }
});
exports.boundedUnit = exports.boundedRecordNil = exports.boundedRecordCons = exports.boundedRecord = exports.boundedProxy = exports.boundedOrdering = exports.boundedNumber = exports.boundedInt = exports.boundedChar = exports.boundedBoolean = exports.bottomRecord = exports.bottom = void 0;
Object.defineProperty(exports, "compare", {
  enumerable: true,
  get: function () {
    return Data_Ord.compare;
  }
});
exports.topRecord = exports.top = void 0;

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Ordering = _interopRequireWildcard(require("../Data.Ordering/index.js"));

var Data_Symbol = _interopRequireWildcard(require("../Data.Symbol/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Record_Unsafe = _interopRequireWildcard(require("../Record.Unsafe/index.js"));

var Type_Proxy = _interopRequireWildcard(require("../Type.Proxy/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var ordRecord =
/* #__PURE__ */
Data_Ord.ordRecord();

var topRecord = function topRecord(dict) {
  return dict.topRecord;
};

exports.topRecord = topRecord;

var top = function top(dict) {
  return dict.top;
};

exports.top = top;
var boundedUnit = {
  top: Data_Unit.unit,
  bottom: Data_Unit.unit,
  Ord0: function Ord0() {
    return Data_Ord.ordUnit;
  }
};
exports.boundedUnit = boundedUnit;
var boundedRecordNil = {
  topRecord: function topRecord(v) {
    return function (v1) {
      return {};
    };
  },
  bottomRecord: function bottomRecord(v) {
    return function (v1) {
      return {};
    };
  },
  OrdRecord0: function OrdRecord0() {
    return Data_Ord.ordRecordNil;
  }
};
exports.boundedRecordNil = boundedRecordNil;

var boundedProxy =
/* #__PURE__ */
function () {
  return {
    bottom: Type_Proxy["Proxy"].value,
    top: Type_Proxy["Proxy"].value,
    Ord0: function Ord0() {
      return Data_Ord.ordProxy;
    }
  };
}();

exports.boundedProxy = boundedProxy;

var boundedOrdering =
/* #__PURE__ */
function () {
  return {
    top: Data_Ordering.GT.value,
    bottom: Data_Ordering.LT.value,
    Ord0: function Ord0() {
      return Data_Ord.ordOrdering;
    }
  };
}();

exports.boundedOrdering = boundedOrdering;
var boundedNumber = {
  top: $foreign.topNumber,
  bottom: $foreign.bottomNumber,
  Ord0: function Ord0() {
    return Data_Ord.ordNumber;
  }
};
exports.boundedNumber = boundedNumber;
var boundedInt = {
  top: $foreign.topInt,
  bottom: $foreign.bottomInt,
  Ord0: function Ord0() {
    return Data_Ord.ordInt;
  }
};
exports.boundedInt = boundedInt;
var boundedChar = {
  top: $foreign.topChar,
  bottom: $foreign.bottomChar,
  Ord0: function Ord0() {
    return Data_Ord.ordChar;
  }
};
exports.boundedChar = boundedChar;
var boundedBoolean = {
  top: true,
  bottom: false,
  Ord0: function Ord0() {
    return Data_Ord.ordBoolean;
  }
};
exports.boundedBoolean = boundedBoolean;

var bottomRecord = function bottomRecord(dict) {
  return dict.bottomRecord;
};

exports.bottomRecord = bottomRecord;

var boundedRecord = function boundedRecord() {
  return function (dictBoundedRecord) {
    var ordRecord1 = ordRecord(dictBoundedRecord.OrdRecord0());
    return {
      top: topRecord(dictBoundedRecord)(Type_Proxy["Proxy"].value)(Type_Proxy["Proxy"].value),
      bottom: bottomRecord(dictBoundedRecord)(Type_Proxy["Proxy"].value)(Type_Proxy["Proxy"].value),
      Ord0: function Ord0() {
        return ordRecord1;
      }
    };
  };
};

exports.boundedRecord = boundedRecord;

var bottom = function bottom(dict) {
  return dict.bottom;
};

exports.bottom = bottom;

var boundedRecordCons = function boundedRecordCons(dictIsSymbol) {
  var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
  return function (dictBounded) {
    var top1 = top(dictBounded);
    var bottom1 = bottom(dictBounded);
    var Ord0 = dictBounded.Ord0();
    return function () {
      return function () {
        return function (dictBoundedRecord) {
          var topRecord1 = topRecord(dictBoundedRecord);
          var bottomRecord1 = bottomRecord(dictBoundedRecord);
          var ordRecordCons = Data_Ord.ordRecordCons(dictBoundedRecord.OrdRecord0())()(dictIsSymbol)(Ord0);
          return {
            topRecord: function topRecord(v) {
              return function (rowProxy) {
                var tail = topRecord1(Type_Proxy["Proxy"].value)(rowProxy);
                var key = reflectSymbol(Type_Proxy["Proxy"].value);
                var insert = Record_Unsafe.unsafeSet(key);
                return insert(top1)(tail);
              };
            },
            bottomRecord: function bottomRecord(v) {
              return function (rowProxy) {
                var tail = bottomRecord1(Type_Proxy["Proxy"].value)(rowProxy);
                var key = reflectSymbol(Type_Proxy["Proxy"].value);
                var insert = Record_Unsafe.unsafeSet(key);
                return insert(bottom1)(tail);
              };
            },
            OrdRecord0: function OrdRecord0() {
              return ordRecordCons;
            }
          };
        };
      };
    };
  };
};

exports.boundedRecordCons = boundedRecordCons;
},{"./foreign.js":"../output/Data.Bounded/foreign.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Ordering/index.js":"../output/Data.Ordering/index.js","../Data.Symbol/index.js":"../output/Data.Symbol/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Record.Unsafe/index.js":"../output/Record.Unsafe/index.js","../Type.Proxy/index.js":"../output/Type.Proxy/index.js"}],"../output/Data.Functor.Invariant/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.invariantMultiplicative = exports.invariantFn = exports.invariantEndo = exports.invariantDual = exports.invariantDisj = exports.invariantConj = exports.invariantArray = exports.invariantAlternate = exports.invariantAdditive = exports.imapF = exports.imap = void 0;

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var invariantMultiplicative = {
  imap: function imap(f) {
    return function (v) {
      return function (v1) {
        return f(v1);
      };
    };
  }
};
exports.invariantMultiplicative = invariantMultiplicative;
var invariantEndo = {
  imap: function imap(ab) {
    return function (ba) {
      return function (v) {
        return function ($42) {
          return ab(v(ba($42)));
        };
      };
    };
  }
};
exports.invariantEndo = invariantEndo;
var invariantDual = {
  imap: function imap(f) {
    return function (v) {
      return function (v1) {
        return f(v1);
      };
    };
  }
};
exports.invariantDual = invariantDual;
var invariantDisj = {
  imap: function imap(f) {
    return function (v) {
      return function (v1) {
        return f(v1);
      };
    };
  }
};
exports.invariantDisj = invariantDisj;
var invariantConj = {
  imap: function imap(f) {
    return function (v) {
      return function (v1) {
        return f(v1);
      };
    };
  }
};
exports.invariantConj = invariantConj;
var invariantAdditive = {
  imap: function imap(f) {
    return function (v) {
      return function (v1) {
        return f(v1);
      };
    };
  }
};
exports.invariantAdditive = invariantAdditive;

var imapF = function imapF(dictFunctor) {
  var map = Data_Functor.map(dictFunctor);
  return function (f) {
    return function (v) {
      return map(f);
    };
  };
};

exports.imapF = imapF;
var invariantArray = {
  imap:
  /* #__PURE__ */
  imapF(Data_Functor.functorArray)
};
exports.invariantArray = invariantArray;
var invariantFn = {
  imap:
  /* #__PURE__ */
  imapF(Data_Functor.functorFn)
};
exports.invariantFn = invariantFn;

var imap = function imap(dict) {
  return dict.imap;
};

exports.imap = imap;

var invariantAlternate = function invariantAlternate(dictInvariant) {
  var imap1 = imap(dictInvariant);
  return {
    imap: function imap(f) {
      return function (g) {
        return function (v) {
          return imap1(f)(g)(v);
        };
      };
    }
  };
};

exports.invariantAlternate = invariantAlternate;
},{"../Data.Functor/index.js":"../output/Data.Functor/index.js"}],"../output/Data.Show/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showStringImpl = exports.showNumberImpl = exports.showIntImpl = exports.showCharImpl = exports.showArrayImpl = void 0;

var showIntImpl = function showIntImpl(n) {
  return n.toString();
};

exports.showIntImpl = showIntImpl;

var showNumberImpl = function showNumberImpl(n) {
  var str = n.toString();
  return isNaN(str + ".0") ? str : str + ".0";
};

exports.showNumberImpl = showNumberImpl;

var showCharImpl = function showCharImpl(c) {
  var code = c.charCodeAt(0);

  if (code < 0x20 || code === 0x7F) {
    switch (c) {
      case "\x07":
        return "'\\a'";

      case "\b":
        return "'\\b'";

      case "\f":
        return "'\\f'";

      case "\n":
        return "'\\n'";

      case "\r":
        return "'\\r'";

      case "\t":
        return "'\\t'";

      case "\v":
        return "'\\v'";
    }

    return "'\\" + code.toString(10) + "'";
  }

  return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
};

exports.showCharImpl = showCharImpl;

var showStringImpl = function showStringImpl(s) {
  var l = s.length;
  return "\"" + s.replace(/[\0-\x1F\x7F"\\]/g, // eslint-disable-line no-control-regex
  function (c, i) {
    switch (c) {
      case "\"":
      case "\\":
        return "\\" + c;

      case "\x07":
        return "\\a";

      case "\b":
        return "\\b";

      case "\f":
        return "\\f";

      case "\n":
        return "\\n";

      case "\r":
        return "\\r";

      case "\t":
        return "\\t";

      case "\v":
        return "\\v";
    }

    var k = i + 1;
    var empty = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
    return "\\" + c.charCodeAt(0).toString(10) + empty;
  }) + "\"";
};

exports.showStringImpl = showStringImpl;

var showArrayImpl = function showArrayImpl(f) {
  return function (xs) {
    var ss = [];

    for (var i = 0, l = xs.length; i < l; i++) {
      ss[i] = f(xs[i]);
    }

    return "[" + ss.join(",") + "]";
  };
};

exports.showArrayImpl = showArrayImpl;
},{}],"../output/Data.Show/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showVoid = exports.showUnit = exports.showString = exports.showRecordFieldsNil = exports.showRecordFieldsConsNil = exports.showRecordFieldsCons = exports.showRecordFields = exports.showRecord = exports.showProxy = exports.showNumber = exports.showInt = exports.showChar = exports.showBoolean = exports.showArray = exports.show = void 0;

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Data_Symbol = _interopRequireWildcard(require("../Data.Symbol/index.js"));

var Data_Void = _interopRequireWildcard(require("../Data.Void/index.js"));

var Record_Unsafe = _interopRequireWildcard(require("../Record.Unsafe/index.js"));

var Type_Proxy = _interopRequireWildcard(require("../Type.Proxy/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var showVoid = {
  show: Data_Void.absurd
};
exports.showVoid = showVoid;
var showUnit = {
  show: function show(v) {
    return "unit";
  }
};
exports.showUnit = showUnit;
var showString = {
  show: $foreign.showStringImpl
};
exports.showString = showString;
var showRecordFieldsNil = {
  showRecordFields: function showRecordFields(v) {
    return function (v1) {
      return "";
    };
  }
};
exports.showRecordFieldsNil = showRecordFieldsNil;

var showRecordFields = function showRecordFields(dict) {
  return dict.showRecordFields;
};

exports.showRecordFields = showRecordFields;

var showRecord = function showRecord() {
  return function () {
    return function (dictShowRecordFields) {
      var showRecordFields1 = showRecordFields(dictShowRecordFields);
      return {
        show: function show(record) {
          return "{" + (showRecordFields1(Type_Proxy["Proxy"].value)(record) + "}");
        }
      };
    };
  };
};

exports.showRecord = showRecord;
var showProxy = {
  show: function show(v) {
    return "Proxy";
  }
};
exports.showProxy = showProxy;
var showNumber = {
  show: $foreign.showNumberImpl
};
exports.showNumber = showNumber;
var showInt = {
  show: $foreign.showIntImpl
};
exports.showInt = showInt;
var showChar = {
  show: $foreign.showCharImpl
};
exports.showChar = showChar;
var showBoolean = {
  show: function show(v) {
    if (v) {
      return "true";
    }

    ;

    if (!v) {
      return "false";
    }

    ;
    throw new Error("Failed pattern match at Data.Show (line 29, column 1 - line 31, column 23): " + [v.constructor.name]);
  }
};
exports.showBoolean = showBoolean;

var show = function show(dict) {
  return dict.show;
};

exports.show = show;

var showArray = function showArray(dictShow) {
  return {
    show: $foreign.showArrayImpl(show(dictShow))
  };
};

exports.showArray = showArray;

var showRecordFieldsCons = function showRecordFieldsCons(dictIsSymbol) {
  var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
  return function (dictShowRecordFields) {
    var showRecordFields1 = showRecordFields(dictShowRecordFields);
    return function (dictShow) {
      var show1 = show(dictShow);
      return {
        showRecordFields: function showRecordFields(v) {
          return function (record) {
            var tail = showRecordFields1(Type_Proxy["Proxy"].value)(record);
            var key = reflectSymbol(Type_Proxy["Proxy"].value);
            var focus = Record_Unsafe.unsafeGet(key)(record);
            return " " + (key + (": " + (show1(focus) + ("," + tail))));
          };
        }
      };
    };
  };
};

exports.showRecordFieldsCons = showRecordFieldsCons;

var showRecordFieldsConsNil = function showRecordFieldsConsNil(dictIsSymbol) {
  var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
  return function (dictShow) {
    var show1 = show(dictShow);
    return {
      showRecordFields: function showRecordFields(v) {
        return function (record) {
          var key = reflectSymbol(Type_Proxy["Proxy"].value);
          var focus = Record_Unsafe.unsafeGet(key)(record);
          return " " + (key + (": " + (show1(focus) + " ")));
        };
      }
    };
  };
};

exports.showRecordFieldsConsNil = showRecordFieldsConsNil;
},{"./foreign.js":"../output/Data.Show/foreign.js","../Data.Symbol/index.js":"../output/Data.Symbol/index.js","../Data.Void/index.js":"../output/Data.Void/index.js","../Record.Unsafe/index.js":"../output/Record.Unsafe/index.js","../Type.Proxy/index.js":"../output/Type.Proxy/index.js"}],"../output/Data.Generic.Rep/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.to = exports.showSum = exports.showProduct = exports.showNoArguments = exports.showConstructor = exports.showArgument = exports.repOf = exports.from = exports.Product = exports.NoArguments = exports.Inr = exports.Inl = exports.Constructor = exports.Argument = void 0;

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

var Data_Symbol = _interopRequireWildcard(require("../Data.Symbol/index.js"));

var Type_Proxy = _interopRequireWildcard(require("../Type.Proxy/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var _show =
/* #__PURE__ */
Data_Show.show(Data_Show.showString);

var Inl =
/* #__PURE__ */
function () {
  function Inl(value0) {
    this.value0 = value0;
  }

  ;

  Inl.create = function (value0) {
    return new Inl(value0);
  };

  return Inl;
}();

exports.Inl = Inl;

var Inr =
/* #__PURE__ */
function () {
  function Inr(value0) {
    this.value0 = value0;
  }

  ;

  Inr.create = function (value0) {
    return new Inr(value0);
  };

  return Inr;
}();

exports.Inr = Inr;

var Product =
/* #__PURE__ */
function () {
  function Product(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }

  ;

  Product.create = function (value0) {
    return function (value1) {
      return new Product(value0, value1);
    };
  };

  return Product;
}();

exports.Product = Product;

var NoConstructors = function NoConstructors(x) {
  return x;
};

var NoArguments =
/* #__PURE__ */
function () {
  function NoArguments() {}

  ;
  NoArguments.value = new NoArguments();
  return NoArguments;
}();

exports.NoArguments = NoArguments;

var Constructor = function Constructor(x) {
  return x;
};

exports.Constructor = Constructor;

var Argument = function Argument(x) {
  return x;
};

exports.Argument = Argument;

var to = function to(dict) {
  return dict.to;
};

exports.to = to;

var showSum = function showSum(dictShow) {
  var show1 = Data_Show.show(dictShow);
  return function (dictShow1) {
    var show2 = Data_Show.show(dictShow1);
    return {
      show: function show(v) {
        if (v instanceof Inl) {
          return "(Inl " + (show1(v.value0) + ")");
        }

        ;

        if (v instanceof Inr) {
          return "(Inr " + (show2(v.value0) + ")");
        }

        ;
        throw new Error("Failed pattern match at Data.Generic.Rep (line 32, column 1 - line 34, column 42): " + [v.constructor.name]);
      }
    };
  };
};

exports.showSum = showSum;

var showProduct = function showProduct(dictShow) {
  var show1 = Data_Show.show(dictShow);
  return function (dictShow1) {
    var show2 = Data_Show.show(dictShow1);
    return {
      show: function show(v) {
        return "(Product " + (show1(v.value0) + (" " + (show2(v.value1) + ")")));
      }
    };
  };
};

exports.showProduct = showProduct;
var showNoArguments = {
  show: function show(v) {
    return "NoArguments";
  }
};
exports.showNoArguments = showNoArguments;

var showConstructor = function showConstructor(dictIsSymbol) {
  var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
  return function (dictShow) {
    var show1 = Data_Show.show(dictShow);
    return {
      show: function show(v) {
        return "(Constructor @" + (_show(reflectSymbol(Type_Proxy["Proxy"].value)) + (" " + (show1(v) + ")")));
      }
    };
  };
};

exports.showConstructor = showConstructor;

var showArgument = function showArgument(dictShow) {
  var show1 = Data_Show.show(dictShow);
  return {
    show: function show(v) {
      return "(Argument " + (show1(v) + ")");
    }
  };
};

exports.showArgument = showArgument;

var repOf = function repOf(dictGeneric) {
  return function (v) {
    return Type_Proxy["Proxy"].value;
  };
};

exports.repOf = repOf;

var from = function from(dict) {
  return dict.from;
};

exports.from = from;
},{"../Data.Show/index.js":"../output/Data.Show/index.js","../Data.Symbol/index.js":"../output/Data.Symbol/index.js","../Type.Proxy/index.js":"../output/Type.Proxy/index.js"}],"../output/Data.Maybe/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showMaybe = exports.semiringMaybe = exports.semigroupMaybe = exports.plusMaybe = exports.ordMaybe = exports.ord1Maybe = exports.optional = exports.monoidMaybe = exports.monadMaybe = exports.maybe$prime = exports.maybe = exports.isNothing = exports.isJust = exports.invariantMaybe = exports.genericMaybe = exports.functorMaybe = exports.fromMaybe$prime = exports.fromMaybe = exports.fromJust = exports.extendMaybe = exports.eqMaybe = exports.eq1Maybe = exports.boundedMaybe = exports.bindMaybe = exports.applyMaybe = exports.applicativeMaybe = exports.alternativeMaybe = exports.altMaybe = exports.Nothing = exports.Just = void 0;

var Control_Alt = _interopRequireWildcard(require("../Control.Alt/index.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Data_Bounded = _interopRequireWildcard(require("../Data.Bounded/index.js"));

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_Function = _interopRequireWildcard(require("../Data.Function/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Functor_Invariant = _interopRequireWildcard(require("../Data.Functor.Invariant/index.js"));

var Data_Generic_Rep = _interopRequireWildcard(require("../Data.Generic.Rep/index.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Ordering = _interopRequireWildcard(require("../Data.Ordering/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Semiring = _interopRequireWildcard(require("../Data.Semiring/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var identity =
/* #__PURE__ */
Control_Category.identity(Control_Category.categoryFn);

var Nothing =
/* #__PURE__ */
function () {
  function Nothing() {}

  ;
  Nothing.value = new Nothing();
  return Nothing;
}();

exports.Nothing = Nothing;

var Just =
/* #__PURE__ */
function () {
  function Just(value0) {
    this.value0 = value0;
  }

  ;

  Just.create = function (value0) {
    return new Just(value0);
  };

  return Just;
}();

exports.Just = Just;

var showMaybe = function showMaybe(dictShow) {
  var _show = Data_Show.show(dictShow);

  return {
    show: function show(v) {
      if (v instanceof Just) {
        return "(Just " + (_show(v.value0) + ")");
      }

      ;

      if (v instanceof Nothing) {
        return "Nothing";
      }

      ;
      throw new Error("Failed pattern match at Data.Maybe (line 223, column 1 - line 225, column 28): " + [v.constructor.name]);
    }
  };
};

exports.showMaybe = showMaybe;

var semigroupMaybe = function semigroupMaybe(dictSemigroup) {
  var append1 = Data_Semigroup.append(dictSemigroup);
  return {
    append: function append(v) {
      return function (v1) {
        if (v instanceof Nothing) {
          return v1;
        }

        ;

        if (v1 instanceof Nothing) {
          return v;
        }

        ;

        if (v instanceof Just && v1 instanceof Just) {
          return new Just(append1(v.value0)(v1.value0));
        }

        ;
        throw new Error("Failed pattern match at Data.Maybe (line 182, column 1 - line 185, column 43): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
};

exports.semigroupMaybe = semigroupMaybe;

var optional = function optional(dictAlt) {
  var alt = Control_Alt.alt(dictAlt);
  var map1 = Data_Functor.map(dictAlt.Functor0());
  return function (dictApplicative) {
    var pure = Control_Applicative.pure(dictApplicative);
    return function (a) {
      return alt(map1(Just.create)(a))(pure(Nothing.value));
    };
  };
};

exports.optional = optional;

var monoidMaybe = function monoidMaybe(dictSemigroup) {
  var semigroupMaybe1 = semigroupMaybe(dictSemigroup);
  return {
    mempty: Nothing.value,
    Semigroup0: function Semigroup0() {
      return semigroupMaybe1;
    }
  };
};

exports.monoidMaybe = monoidMaybe;

var maybe$prime = function maybe$prime(v) {
  return function (v1) {
    return function (v2) {
      if (v2 instanceof Nothing) {
        return v(Data_Unit.unit);
      }

      ;

      if (v2 instanceof Just) {
        return v1(v2.value0);
      }

      ;
      throw new Error("Failed pattern match at Data.Maybe (line 250, column 1 - line 250, column 62): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
    };
  };
};

exports.maybe$prime = maybe$prime;

var maybe = function maybe(v) {
  return function (v1) {
    return function (v2) {
      if (v2 instanceof Nothing) {
        return v;
      }

      ;

      if (v2 instanceof Just) {
        return v1(v2.value0);
      }

      ;
      throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
    };
  };
};

exports.maybe = maybe;
var isNothing =
/* #__PURE__ */
maybe(true)(
/* #__PURE__ */
Data_Function["const"](false));
exports.isNothing = isNothing;
var isJust =
/* #__PURE__ */
maybe(false)(
/* #__PURE__ */
Data_Function["const"](true));
exports.isJust = isJust;
var genericMaybe = {
  to: function to(x) {
    if (x instanceof Data_Generic_Rep.Inl) {
      return Nothing.value;
    }

    ;

    if (x instanceof Data_Generic_Rep.Inr) {
      return new Just(x.value0);
    }

    ;
    throw new Error("Failed pattern match at Data.Maybe (line 227, column 1 - line 227, column 52): " + [x.constructor.name]);
  },
  from: function from(x) {
    if (x instanceof Nothing) {
      return new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value);
    }

    ;

    if (x instanceof Just) {
      return new Data_Generic_Rep.Inr(x.value0);
    }

    ;
    throw new Error("Failed pattern match at Data.Maybe (line 227, column 1 - line 227, column 52): " + [x.constructor.name]);
  }
};
exports.genericMaybe = genericMaybe;
var functorMaybe = {
  map: function map(v) {
    return function (v1) {
      if (v1 instanceof Just) {
        return new Just(v(v1.value0));
      }

      ;
      return Nothing.value;
    };
  }
};
exports.functorMaybe = functorMaybe;
var map =
/* #__PURE__ */
Data_Functor.map(functorMaybe);
var invariantMaybe = {
  imap:
  /* #__PURE__ */
  Data_Functor_Invariant.imapF(functorMaybe)
};
exports.invariantMaybe = invariantMaybe;

var fromMaybe$prime = function fromMaybe$prime(a) {
  return maybe$prime(a)(identity);
};

exports.fromMaybe$prime = fromMaybe$prime;

var fromMaybe = function fromMaybe(a) {
  return maybe(a)(identity);
};

exports.fromMaybe = fromMaybe;

var fromJust = function fromJust() {
  return function (v) {
    if (v instanceof Just) {
      return v.value0;
    }

    ;
    throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
  };
};

exports.fromJust = fromJust;
var extendMaybe = {
  extend: function extend(v) {
    return function (v1) {
      if (v1 instanceof Nothing) {
        return Nothing.value;
      }

      ;
      return new Just(v(v1));
    };
  },
  Functor0: function Functor0() {
    return functorMaybe;
  }
};
exports.extendMaybe = extendMaybe;

var eqMaybe = function eqMaybe(dictEq) {
  var _eq = Data_Eq.eq(dictEq);

  return {
    eq: function eq(x) {
      return function (y) {
        if (x instanceof Nothing && y instanceof Nothing) {
          return true;
        }

        ;

        if (x instanceof Just && y instanceof Just) {
          return _eq(x.value0)(y.value0);
        }

        ;
        return false;
      };
    }
  };
};

exports.eqMaybe = eqMaybe;

var ordMaybe = function ordMaybe(dictOrd) {
  var _compare = Data_Ord.compare(dictOrd);

  var eqMaybe1 = eqMaybe(dictOrd.Eq0());
  return {
    compare: function compare(x) {
      return function (y) {
        if (x instanceof Nothing && y instanceof Nothing) {
          return Data_Ordering.EQ.value;
        }

        ;

        if (x instanceof Nothing) {
          return Data_Ordering.LT.value;
        }

        ;

        if (y instanceof Nothing) {
          return Data_Ordering.GT.value;
        }

        ;

        if (x instanceof Just && y instanceof Just) {
          return _compare(x.value0)(y.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.Maybe (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function Eq0() {
      return eqMaybe1;
    }
  };
};

exports.ordMaybe = ordMaybe;
var eq1Maybe = {
  eq1: function eq1(dictEq) {
    return Data_Eq.eq(eqMaybe(dictEq));
  }
};
exports.eq1Maybe = eq1Maybe;
var ord1Maybe = {
  compare1: function compare1(dictOrd) {
    return Data_Ord.compare(ordMaybe(dictOrd));
  },
  Eq10: function Eq10() {
    return eq1Maybe;
  }
};
exports.ord1Maybe = ord1Maybe;

var boundedMaybe = function boundedMaybe(dictBounded) {
  var ordMaybe1 = ordMaybe(dictBounded.Ord0());
  return {
    top: new Just(Data_Bounded.top(dictBounded)),
    bottom: Nothing.value,
    Ord0: function Ord0() {
      return ordMaybe1;
    }
  };
};

exports.boundedMaybe = boundedMaybe;
var applyMaybe = {
  apply: function apply(v) {
    return function (v1) {
      if (v instanceof Just) {
        return map(v.value0)(v1);
      }

      ;

      if (v instanceof Nothing) {
        return Nothing.value;
      }

      ;
      throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Functor0: function Functor0() {
    return functorMaybe;
  }
};
exports.applyMaybe = applyMaybe;
var apply =
/* #__PURE__ */
Control_Apply.apply(applyMaybe);
var bindMaybe = {
  bind: function bind(v) {
    return function (v1) {
      if (v instanceof Just) {
        return v1(v.value0);
      }

      ;

      if (v instanceof Nothing) {
        return Nothing.value;
      }

      ;
      throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Apply0: function Apply0() {
    return applyMaybe;
  }
};
exports.bindMaybe = bindMaybe;

var semiringMaybe = function semiringMaybe(dictSemiring) {
  var _add = Data_Semiring.add(dictSemiring);

  var _mul = Data_Semiring.mul(dictSemiring);

  return {
    zero: Nothing.value,
    one: new Just(Data_Semiring.one(dictSemiring)),
    add: function add(v) {
      return function (v1) {
        if (v instanceof Nothing) {
          return v1;
        }

        ;

        if (v1 instanceof Nothing) {
          return v;
        }

        ;

        if (v instanceof Just && v1 instanceof Just) {
          return new Just(_add(v.value0)(v1.value0));
        }

        ;
        throw new Error("Failed pattern match at Data.Maybe (line 190, column 1 - line 198, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    mul: function mul(x) {
      return function (y) {
        return apply(map(_mul)(x))(y);
      };
    }
  };
};

exports.semiringMaybe = semiringMaybe;

var applicativeMaybe =
/* #__PURE__ */
function () {
  return {
    pure: Just.create,
    Apply0: function Apply0() {
      return applyMaybe;
    }
  };
}();

exports.applicativeMaybe = applicativeMaybe;
var monadMaybe = {
  Applicative0: function Applicative0() {
    return applicativeMaybe;
  },
  Bind1: function Bind1() {
    return bindMaybe;
  }
};
exports.monadMaybe = monadMaybe;
var altMaybe = {
  alt: function alt(v) {
    return function (v1) {
      if (v instanceof Nothing) {
        return v1;
      }

      ;
      return v;
    };
  },
  Functor0: function Functor0() {
    return functorMaybe;
  }
};
exports.altMaybe = altMaybe;

var plusMaybe =
/* #__PURE__ */
function () {
  return {
    empty: Nothing.value,
    Alt0: function Alt0() {
      return altMaybe;
    }
  };
}();

exports.plusMaybe = plusMaybe;
var alternativeMaybe = {
  Applicative0: function Applicative0() {
    return applicativeMaybe;
  },
  Plus1: function Plus1() {
    return plusMaybe;
  }
};
exports.alternativeMaybe = alternativeMaybe;
},{"../Control.Alt/index.js":"../output/Control.Alt/index.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Category/index.js":"../output/Control.Category/index.js","../Data.Bounded/index.js":"../output/Data.Bounded/index.js","../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.Function/index.js":"../output/Data.Function/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Functor.Invariant/index.js":"../output/Data.Functor.Invariant/index.js","../Data.Generic.Rep/index.js":"../output/Data.Generic.Rep/index.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Ordering/index.js":"../output/Data.Ordering/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Semiring/index.js":"../output/Data.Semiring/index.js","../Data.Show/index.js":"../output/Data.Show/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js"}],"../output/Data.Either/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showEither = exports.semigroupEither = exports.ordEither = exports.ord1Either = exports.note$prime = exports.note = exports.monadEither = exports.isRight = exports.isLeft = exports.invariantEither = exports.hush = exports.genericEither = exports.functorEither = exports.fromRight$prime = exports.fromRight = exports.fromLeft$prime = exports.fromLeft = exports.extendEither = exports.eqEither = exports.eq1Either = exports.either = exports.choose = exports.boundedEither = exports.blush = exports.bindEither = exports.applyEither = exports.applicativeEither = exports.altEither = exports.Right = exports.Left = void 0;

var Control_Alt = _interopRequireWildcard(require("../Control.Alt/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Data_Bounded = _interopRequireWildcard(require("../Data.Bounded/index.js"));

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_Function = _interopRequireWildcard(require("../Data.Function/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Functor_Invariant = _interopRequireWildcard(require("../Data.Functor.Invariant/index.js"));

var Data_Generic_Rep = _interopRequireWildcard(require("../Data.Generic.Rep/index.js"));

var Data_Maybe = _interopRequireWildcard(require("../Data.Maybe/index.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Ordering = _interopRequireWildcard(require("../Data.Ordering/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var Left =
/* #__PURE__ */
function () {
  function Left(value0) {
    this.value0 = value0;
  }

  ;

  Left.create = function (value0) {
    return new Left(value0);
  };

  return Left;
}();

exports.Left = Left;

var Right =
/* #__PURE__ */
function () {
  function Right(value0) {
    this.value0 = value0;
  }

  ;

  Right.create = function (value0) {
    return new Right(value0);
  };

  return Right;
}();

exports.Right = Right;

var showEither = function showEither(dictShow) {
  var _show = Data_Show.show(dictShow);

  return function (dictShow1) {
    var show1 = Data_Show.show(dictShow1);
    return {
      show: function show(v) {
        if (v instanceof Left) {
          return "(Left " + (_show(v.value0) + ")");
        }

        ;

        if (v instanceof Right) {
          return "(Right " + (show1(v.value0) + ")");
        }

        ;
        throw new Error("Failed pattern match at Data.Either (line 173, column 1 - line 175, column 46): " + [v.constructor.name]);
      }
    };
  };
};

exports.showEither = showEither;

var note$prime = function note$prime(f) {
  return Data_Maybe["maybe$prime"](function ($138) {
    return Left.create(f($138));
  })(Right.create);
};

exports.note$prime = note$prime;

var note = function note(a) {
  return Data_Maybe.maybe(new Left(a))(Right.create);
};

exports.note = note;
var genericEither = {
  to: function to(x) {
    if (x instanceof Data_Generic_Rep.Inl) {
      return new Left(x.value0);
    }

    ;

    if (x instanceof Data_Generic_Rep.Inr) {
      return new Right(x.value0);
    }

    ;
    throw new Error("Failed pattern match at Data.Either (line 33, column 1 - line 33, column 56): " + [x.constructor.name]);
  },
  from: function from(x) {
    if (x instanceof Left) {
      return new Data_Generic_Rep.Inl(x.value0);
    }

    ;

    if (x instanceof Right) {
      return new Data_Generic_Rep.Inr(x.value0);
    }

    ;
    throw new Error("Failed pattern match at Data.Either (line 33, column 1 - line 33, column 56): " + [x.constructor.name]);
  }
};
exports.genericEither = genericEither;
var functorEither = {
  map: function map(f) {
    return function (m) {
      if (m instanceof Left) {
        return new Left(m.value0);
      }

      ;

      if (m instanceof Right) {
        return new Right(f(m.value0));
      }

      ;
      throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
    };
  }
};
exports.functorEither = functorEither;
var map =
/* #__PURE__ */
Data_Functor.map(functorEither);
var invariantEither = {
  imap:
  /* #__PURE__ */
  Data_Functor_Invariant.imapF(functorEither)
};
exports.invariantEither = invariantEither;

var fromRight$prime = function fromRight$prime(v) {
  return function (v1) {
    if (v1 instanceof Right) {
      return v1.value0;
    }

    ;
    return v(Data_Unit.unit);
  };
};

exports.fromRight$prime = fromRight$prime;

var fromRight = function fromRight(v) {
  return function (v1) {
    if (v1 instanceof Right) {
      return v1.value0;
    }

    ;
    return v;
  };
};

exports.fromRight = fromRight;

var fromLeft$prime = function fromLeft$prime(v) {
  return function (v1) {
    if (v1 instanceof Left) {
      return v1.value0;
    }

    ;
    return v(Data_Unit.unit);
  };
};

exports.fromLeft$prime = fromLeft$prime;

var fromLeft = function fromLeft(v) {
  return function (v1) {
    if (v1 instanceof Left) {
      return v1.value0;
    }

    ;
    return v;
  };
};

exports.fromLeft = fromLeft;
var extendEither = {
  extend: function extend(v) {
    return function (v1) {
      if (v1 instanceof Left) {
        return new Left(v1.value0);
      }

      ;
      return new Right(v(v1));
    };
  },
  Functor0: function Functor0() {
    return functorEither;
  }
};
exports.extendEither = extendEither;

var eqEither = function eqEither(dictEq) {
  var _eq = Data_Eq.eq(dictEq);

  return function (dictEq1) {
    var eq1 = Data_Eq.eq(dictEq1);
    return {
      eq: function eq(x) {
        return function (y) {
          if (x instanceof Left && y instanceof Left) {
            return _eq(x.value0)(y.value0);
          }

          ;

          if (x instanceof Right && y instanceof Right) {
            return eq1(x.value0)(y.value0);
          }

          ;
          return false;
        };
      }
    };
  };
};

exports.eqEither = eqEither;

var ordEither = function ordEither(dictOrd) {
  var _compare = Data_Ord.compare(dictOrd);

  var eqEither1 = eqEither(dictOrd.Eq0());
  return function (dictOrd1) {
    var compare1 = Data_Ord.compare(dictOrd1);
    var eqEither2 = eqEither1(dictOrd1.Eq0());
    return {
      compare: function compare(x) {
        return function (y) {
          if (x instanceof Left && y instanceof Left) {
            return _compare(x.value0)(y.value0);
          }

          ;

          if (x instanceof Left) {
            return Data_Ordering.LT.value;
          }

          ;

          if (y instanceof Left) {
            return Data_Ordering.GT.value;
          }

          ;

          if (x instanceof Right && y instanceof Right) {
            return compare1(x.value0)(y.value0);
          }

          ;
          throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
        };
      },
      Eq0: function Eq0() {
        return eqEither2;
      }
    };
  };
};

exports.ordEither = ordEither;

var eq1Either = function eq1Either(dictEq) {
  var eqEither1 = eqEither(dictEq);
  return {
    eq1: function eq1(dictEq1) {
      return Data_Eq.eq(eqEither1(dictEq1));
    }
  };
};

exports.eq1Either = eq1Either;

var ord1Either = function ord1Either(dictOrd) {
  var ordEither1 = ordEither(dictOrd);
  var eq1Either1 = eq1Either(dictOrd.Eq0());
  return {
    compare1: function compare1(dictOrd1) {
      return Data_Ord.compare(ordEither1(dictOrd1));
    },
    Eq10: function Eq10() {
      return eq1Either1;
    }
  };
};

exports.ord1Either = ord1Either;

var either = function either(v) {
  return function (v1) {
    return function (v2) {
      if (v2 instanceof Left) {
        return v(v2.value0);
      }

      ;

      if (v2 instanceof Right) {
        return v1(v2.value0);
      }

      ;
      throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
    };
  };
};

exports.either = either;

var hush =
/* #__PURE__ */
function () {
  return either(Data_Function["const"](Data_Maybe.Nothing.value))(Data_Maybe.Just.create);
}();

exports.hush = hush;
var isLeft =
/* #__PURE__ */
either(
/* #__PURE__ */
Data_Function["const"](true))(
/* #__PURE__ */
Data_Function["const"](false));
exports.isLeft = isLeft;
var isRight =
/* #__PURE__ */
either(
/* #__PURE__ */
Data_Function["const"](false))(
/* #__PURE__ */
Data_Function["const"](true));
exports.isRight = isRight;

var choose = function choose(dictAlt) {
  var alt = Control_Alt.alt(dictAlt);
  var map1 = Data_Functor.map(dictAlt.Functor0());
  return function (a) {
    return function (b) {
      return alt(map1(Left.create)(a))(map1(Right.create)(b));
    };
  };
};

exports.choose = choose;

var boundedEither = function boundedEither(dictBounded) {
  var bottom = Data_Bounded.bottom(dictBounded);
  var ordEither1 = ordEither(dictBounded.Ord0());
  return function (dictBounded1) {
    var ordEither2 = ordEither1(dictBounded1.Ord0());
    return {
      top: new Right(Data_Bounded.top(dictBounded1)),
      bottom: new Left(bottom),
      Ord0: function Ord0() {
        return ordEither2;
      }
    };
  };
};

exports.boundedEither = boundedEither;

var blush =
/* #__PURE__ */
function () {
  return either(Data_Maybe.Just.create)(Data_Function["const"](Data_Maybe.Nothing.value));
}();

exports.blush = blush;
var applyEither = {
  apply: function apply(v) {
    return function (v1) {
      if (v instanceof Left) {
        return new Left(v.value0);
      }

      ;

      if (v instanceof Right) {
        return map(v.value0)(v1);
      }

      ;
      throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Functor0: function Functor0() {
    return functorEither;
  }
};
exports.applyEither = applyEither;
var apply =
/* #__PURE__ */
Control_Apply.apply(applyEither);
var bindEither = {
  bind:
  /* #__PURE__ */
  either(function (e) {
    return function (v) {
      return new Left(e);
    };
  })(function (a) {
    return function (f) {
      return f(a);
    };
  }),
  Apply0: function Apply0() {
    return applyEither;
  }
};
exports.bindEither = bindEither;

var semigroupEither = function semigroupEither(dictSemigroup) {
  var append1 = Data_Semigroup.append(dictSemigroup);
  return {
    append: function append(x) {
      return function (y) {
        return apply(map(append1)(x))(y);
      };
    }
  };
};

exports.semigroupEither = semigroupEither;

var applicativeEither =
/* #__PURE__ */
function () {
  return {
    pure: Right.create,
    Apply0: function Apply0() {
      return applyEither;
    }
  };
}();

exports.applicativeEither = applicativeEither;
var monadEither = {
  Applicative0: function Applicative0() {
    return applicativeEither;
  },
  Bind1: function Bind1() {
    return bindEither;
  }
};
exports.monadEither = monadEither;
var altEither = {
  alt: function alt(v) {
    return function (v1) {
      if (v instanceof Left) {
        return v1;
      }

      ;
      return v;
    };
  },
  Functor0: function Functor0() {
    return functorEither;
  }
};
exports.altEither = altEither;
},{"../Control.Alt/index.js":"../output/Control.Alt/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Data.Bounded/index.js":"../output/Data.Bounded/index.js","../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.Function/index.js":"../output/Data.Function/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Functor.Invariant/index.js":"../output/Data.Functor.Invariant/index.js","../Data.Generic.Rep/index.js":"../output/Data.Generic.Rep/index.js","../Data.Maybe/index.js":"../output/Data.Maybe/index.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Ordering/index.js":"../output/Data.Ordering/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Show/index.js":"../output/Data.Show/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js"}],"../output/Effect/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.whileE = exports.untilE = exports.pureE = exports.foreachE = exports.forE = exports.bindE = void 0;

var pureE = function pureE(a) {
  return function () {
    return a;
  };
};

exports.pureE = pureE;

var bindE = function bindE(a) {
  return function (f) {
    return function () {
      return f(a())();
    };
  };
};

exports.bindE = bindE;

var untilE = function untilE(f) {
  return function () {
    while (!f());
  };
};

exports.untilE = untilE;

var whileE = function whileE(f) {
  return function (a) {
    return function () {
      while (f()) {
        a();
      }
    };
  };
};

exports.whileE = whileE;

var forE = function forE(lo) {
  return function (hi) {
    return function (f) {
      return function () {
        for (var i = lo; i < hi; i++) {
          f(i)();
        }
      };
    };
  };
};

exports.forE = forE;

var foreachE = function foreachE(as) {
  return function (f) {
    return function () {
      for (var i = 0, l = as.length; i < l; i++) {
        f(as[i])();
      }
    };
  };
};

exports.foreachE = foreachE;
},{}],"../output/Data.EuclideanRing/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.numDiv = exports.intMod = exports.intDiv = exports.intDegree = void 0;

var intDegree = function intDegree(x) {
  return Math.min(Math.abs(x), 2147483647);
}; // See the Euclidean definition in
// https://en.m.wikipedia.org/wiki/Modulo_operation.


exports.intDegree = intDegree;

var intDiv = function intDiv(x) {
  return function (y) {
    if (y === 0) return 0;
    return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
  };
};

exports.intDiv = intDiv;

var intMod = function intMod(x) {
  return function (y) {
    if (y === 0) return 0;
    var yy = Math.abs(y);
    return (x % yy + yy) % yy;
  };
};

exports.intMod = intMod;

var numDiv = function numDiv(n1) {
  return function (n2) {
    return n1 / n2;
  };
};

exports.numDiv = numDiv;
},{}],"../output/Data.CommutativeRing/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "add", {
  enumerable: true,
  get: function () {
    return Data_Semiring.add;
  }
});
exports.commutativeRingUnit = exports.commutativeRingRecordNil = exports.commutativeRingRecordCons = exports.commutativeRingRecord = exports.commutativeRingProxy = exports.commutativeRingNumber = exports.commutativeRingInt = exports.commutativeRingFn = void 0;
Object.defineProperty(exports, "mul", {
  enumerable: true,
  get: function () {
    return Data_Semiring.mul;
  }
});
Object.defineProperty(exports, "one", {
  enumerable: true,
  get: function () {
    return Data_Semiring.one;
  }
});
Object.defineProperty(exports, "zero", {
  enumerable: true,
  get: function () {
    return Data_Semiring.zero;
  }
});

var Data_Ring = _interopRequireWildcard(require("../Data.Ring/index.js"));

var Data_Semiring = _interopRequireWildcard(require("../Data.Semiring/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var ringRecord =
/* #__PURE__ */
Data_Ring.ringRecord();
var commutativeRingUnit = {
  Ring0: function Ring0() {
    return Data_Ring.ringUnit;
  }
};
exports.commutativeRingUnit = commutativeRingUnit;
var commutativeRingRecordNil = {
  RingRecord0: function RingRecord0() {
    return Data_Ring.ringRecordNil;
  }
};
exports.commutativeRingRecordNil = commutativeRingRecordNil;

var commutativeRingRecordCons = function commutativeRingRecordCons(dictIsSymbol) {
  var ringRecordCons = Data_Ring.ringRecordCons(dictIsSymbol)();
  return function () {
    return function (dictCommutativeRingRecord) {
      var ringRecordCons1 = ringRecordCons(dictCommutativeRingRecord.RingRecord0());
      return function (dictCommutativeRing) {
        var ringRecordCons2 = ringRecordCons1(dictCommutativeRing.Ring0());
        return {
          RingRecord0: function RingRecord0() {
            return ringRecordCons2;
          }
        };
      };
    };
  };
};

exports.commutativeRingRecordCons = commutativeRingRecordCons;

var commutativeRingRecord = function commutativeRingRecord() {
  return function (dictCommutativeRingRecord) {
    var ringRecord1 = ringRecord(dictCommutativeRingRecord.RingRecord0());
    return {
      Ring0: function Ring0() {
        return ringRecord1;
      }
    };
  };
};

exports.commutativeRingRecord = commutativeRingRecord;
var commutativeRingProxy = {
  Ring0: function Ring0() {
    return Data_Ring.ringProxy;
  }
};
exports.commutativeRingProxy = commutativeRingProxy;
var commutativeRingNumber = {
  Ring0: function Ring0() {
    return Data_Ring.ringNumber;
  }
};
exports.commutativeRingNumber = commutativeRingNumber;
var commutativeRingInt = {
  Ring0: function Ring0() {
    return Data_Ring.ringInt;
  }
};
exports.commutativeRingInt = commutativeRingInt;

var commutativeRingFn = function commutativeRingFn(dictCommutativeRing) {
  var ringFn = Data_Ring.ringFn(dictCommutativeRing.Ring0());
  return {
    Ring0: function Ring0() {
      return ringFn;
    }
  };
};

exports.commutativeRingFn = commutativeRingFn;
},{"../Data.Ring/index.js":"../output/Data.Ring/index.js","../Data.Semiring/index.js":"../output/Data.Semiring/index.js"}],"../output/Data.EuclideanRing/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "add", {
  enumerable: true,
  get: function () {
    return Data_Semiring.add;
  }
});
exports.mod = exports.lcm = exports.gcd = exports.euclideanRingNumber = exports.euclideanRingInt = exports.div = exports.degree = void 0;
Object.defineProperty(exports, "mul", {
  enumerable: true,
  get: function () {
    return Data_Semiring.mul;
  }
});
Object.defineProperty(exports, "one", {
  enumerable: true,
  get: function () {
    return Data_Semiring.one;
  }
});
Object.defineProperty(exports, "sub", {
  enumerable: true,
  get: function () {
    return Data_Ring.sub;
  }
});
Object.defineProperty(exports, "zero", {
  enumerable: true,
  get: function () {
    return Data_Semiring.zero;
  }
});

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Data_CommutativeRing = _interopRequireWildcard(require("../Data.CommutativeRing/index.js"));

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_Ring = _interopRequireWildcard(require("../Data.Ring/index.js"));

var Data_Semiring = _interopRequireWildcard(require("../Data.Semiring/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var mod = function mod(dict) {
  return dict.mod;
};

exports.mod = mod;

var gcd = function gcd(dictEq) {
  var eq = Data_Eq.eq(dictEq);
  return function (dictEuclideanRing) {
    var zero = Data_Semiring.zero(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0());
    var mod1 = mod(dictEuclideanRing);
    return function (a) {
      return function (b) {
        var $24 = eq(b)(zero);

        if ($24) {
          return a;
        }

        ;
        return gcd(dictEq)(dictEuclideanRing)(b)(mod1(a)(b));
      };
    };
  };
};

exports.gcd = gcd;
var euclideanRingNumber = {
  degree: function degree(v) {
    return 1;
  },
  div: $foreign.numDiv,
  mod: function mod(v) {
    return function (v1) {
      return 0.0;
    };
  },
  CommutativeRing0: function CommutativeRing0() {
    return Data_CommutativeRing.commutativeRingNumber;
  }
};
exports.euclideanRingNumber = euclideanRingNumber;
var euclideanRingInt = {
  degree: $foreign.intDegree,
  div: $foreign.intDiv,
  mod: $foreign.intMod,
  CommutativeRing0: function CommutativeRing0() {
    return Data_CommutativeRing.commutativeRingInt;
  }
};
exports.euclideanRingInt = euclideanRingInt;

var div = function div(dict) {
  return dict.div;
};

exports.div = div;

var lcm = function lcm(dictEq) {
  var eq = Data_Eq.eq(dictEq);
  var gcd1 = gcd(dictEq);
  return function (dictEuclideanRing) {
    var Semiring0 = dictEuclideanRing.CommutativeRing0().Ring0().Semiring0();
    var zero = Data_Semiring.zero(Semiring0);
    var div1 = div(dictEuclideanRing);
    var mul = Data_Semiring.mul(Semiring0);
    var gcd2 = gcd1(dictEuclideanRing);
    return function (a) {
      return function (b) {
        var $26 = eq(a)(zero) || eq(b)(zero);

        if ($26) {
          return zero;
        }

        ;
        return div1(mul(a)(b))(gcd2(a)(b));
      };
    };
  };
};

exports.lcm = lcm;

var degree = function degree(dict) {
  return dict.degree;
};

exports.degree = degree;
},{"./foreign.js":"../output/Data.EuclideanRing/foreign.js","../Data.CommutativeRing/index.js":"../output/Data.CommutativeRing/index.js","../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.Ring/index.js":"../output/Data.Ring/index.js","../Data.Semiring/index.js":"../output/Data.Semiring/index.js"}],"../output/Data.Monoid/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.power = exports.monoidUnit = exports.monoidString = exports.monoidRecordNil = exports.monoidRecordCons = exports.monoidRecord = exports.monoidOrdering = exports.monoidFn = exports.monoidArray = exports.memptyRecord = exports.mempty = exports.guard = void 0;

var Data_Boolean = _interopRequireWildcard(require("../Data.Boolean/index.js"));

var Data_EuclideanRing = _interopRequireWildcard(require("../Data.EuclideanRing/index.js"));

var Data_Ordering = _interopRequireWildcard(require("../Data.Ordering/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Symbol = _interopRequireWildcard(require("../Data.Symbol/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Record_Unsafe = _interopRequireWildcard(require("../Record.Unsafe/index.js"));

var Type_Proxy = _interopRequireWildcard(require("../Type.Proxy/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var semigroupRecord =
/* #__PURE__ */
Data_Semigroup.semigroupRecord();
var mod =
/* #__PURE__ */
Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt);
var div =
/* #__PURE__ */
Data_EuclideanRing.div(Data_EuclideanRing.euclideanRingInt);
var monoidUnit = {
  mempty: Data_Unit.unit,
  Semigroup0: function Semigroup0() {
    return Data_Semigroup.semigroupUnit;
  }
};
exports.monoidUnit = monoidUnit;
var monoidString = {
  mempty: "",
  Semigroup0: function Semigroup0() {
    return Data_Semigroup.semigroupString;
  }
};
exports.monoidString = monoidString;
var monoidRecordNil = {
  memptyRecord: function memptyRecord(v) {
    return {};
  },
  SemigroupRecord0: function SemigroupRecord0() {
    return Data_Semigroup.semigroupRecordNil;
  }
};
exports.monoidRecordNil = monoidRecordNil;

var monoidOrdering =
/* #__PURE__ */
function () {
  return {
    mempty: Data_Ordering.EQ.value,
    Semigroup0: function Semigroup0() {
      return Data_Ordering.semigroupOrdering;
    }
  };
}();

exports.monoidOrdering = monoidOrdering;
var monoidArray = {
  mempty: [],
  Semigroup0: function Semigroup0() {
    return Data_Semigroup.semigroupArray;
  }
};
exports.monoidArray = monoidArray;

var memptyRecord = function memptyRecord(dict) {
  return dict.memptyRecord;
};

exports.memptyRecord = memptyRecord;

var monoidRecord = function monoidRecord() {
  return function (dictMonoidRecord) {
    var semigroupRecord1 = semigroupRecord(dictMonoidRecord.SemigroupRecord0());
    return {
      mempty: memptyRecord(dictMonoidRecord)(Type_Proxy["Proxy"].value),
      Semigroup0: function Semigroup0() {
        return semigroupRecord1;
      }
    };
  };
};

exports.monoidRecord = monoidRecord;

var mempty = function mempty(dict) {
  return dict.mempty;
};

exports.mempty = mempty;

var monoidFn = function monoidFn(dictMonoid) {
  var mempty1 = mempty(dictMonoid);
  var semigroupFn = Data_Semigroup.semigroupFn(dictMonoid.Semigroup0());
  return {
    mempty: function mempty(v) {
      return mempty1;
    },
    Semigroup0: function Semigroup0() {
      return semigroupFn;
    }
  };
};

exports.monoidFn = monoidFn;

var monoidRecordCons = function monoidRecordCons(dictIsSymbol) {
  var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
  var semigroupRecordCons = Data_Semigroup.semigroupRecordCons(dictIsSymbol)();
  return function (dictMonoid) {
    var mempty1 = mempty(dictMonoid);
    var Semigroup0 = dictMonoid.Semigroup0();
    return function () {
      return function (dictMonoidRecord) {
        var memptyRecord1 = memptyRecord(dictMonoidRecord);
        var semigroupRecordCons1 = semigroupRecordCons(dictMonoidRecord.SemigroupRecord0())(Semigroup0);
        return {
          memptyRecord: function memptyRecord(v) {
            var tail = memptyRecord1(Type_Proxy["Proxy"].value);
            var key = reflectSymbol(Type_Proxy["Proxy"].value);
            var insert = Record_Unsafe.unsafeSet(key);
            return insert(mempty1)(tail);
          },
          SemigroupRecord0: function SemigroupRecord0() {
            return semigroupRecordCons1;
          }
        };
      };
    };
  };
};

exports.monoidRecordCons = monoidRecordCons;

var power = function power(dictMonoid) {
  var mempty1 = mempty(dictMonoid);
  var append = Data_Semigroup.append(dictMonoid.Semigroup0());
  return function (x) {
    var go = function go(p) {
      if (p <= 0) {
        return mempty1;
      }

      ;

      if (p === 1) {
        return x;
      }

      ;

      if (mod(p)(2) === 0) {
        var x$prime = go(div(p)(2));
        return append(x$prime)(x$prime);
      }

      ;

      if (Data_Boolean.otherwise) {
        var x$prime = go(div(p)(2));
        return append(x$prime)(append(x$prime)(x));
      }

      ;
      throw new Error("Failed pattern match at Data.Monoid (line 88, column 3 - line 88, column 17): " + [p.constructor.name]);
    };

    return go;
  };
};

exports.power = power;

var guard = function guard(dictMonoid) {
  var mempty1 = mempty(dictMonoid);
  return function (v) {
    return function (v1) {
      if (v) {
        return v1;
      }

      ;

      if (!v) {
        return mempty1;
      }

      ;
      throw new Error("Failed pattern match at Data.Monoid (line 96, column 1 - line 96, column 49): " + [v.constructor.name, v1.constructor.name]);
    };
  };
};

exports.guard = guard;
},{"../Data.Boolean/index.js":"../output/Data.Boolean/index.js","../Data.EuclideanRing/index.js":"../output/Data.EuclideanRing/index.js","../Data.Ordering/index.js":"../output/Data.Ordering/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Symbol/index.js":"../output/Data.Symbol/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Record.Unsafe/index.js":"../output/Record.Unsafe/index.js","../Type.Proxy/index.js":"../output/Type.Proxy/index.js"}],"../output/Effect/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.bindEffect = exports.applyEffect = exports.applicativeEffect = void 0;
Object.defineProperty(exports, "forE", {
  enumerable: true,
  get: function () {
    return $foreign.forE;
  }
});
Object.defineProperty(exports, "foreachE", {
  enumerable: true,
  get: function () {
    return $foreign.foreachE;
  }
});
exports.semigroupEffect = exports.monoidEffect = exports.monadEffect = exports.functorEffect = void 0;
Object.defineProperty(exports, "untilE", {
  enumerable: true,
  get: function () {
    return $foreign.untilE;
  }
});
Object.defineProperty(exports, "whileE", {
  enumerable: true,
  get: function () {
    return $foreign.whileE;
  }
});

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Monad = _interopRequireWildcard(require("../Control.Monad/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var $runtime_lazy = function $runtime_lazy(name, moduleName, init) {
  var state = 0;
  var val;
  return function (lineNumber) {
    if (state === 2) return val;
    if (state === 1) throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state = 1;
    val = init();
    state = 2;
    return val;
  };
};

var monadEffect = {
  Applicative0: function Applicative0() {
    return applicativeEffect;
  },
  Bind1: function Bind1() {
    return bindEffect;
  }
};
exports.monadEffect = monadEffect;
var bindEffect = {
  bind: $foreign.bindE,
  Apply0: function Apply0() {
    return $lazy_applyEffect(0);
  }
};
exports.bindEffect = bindEffect;
var applicativeEffect = {
  pure: $foreign.pureE,
  Apply0: function Apply0() {
    return $lazy_applyEffect(0);
  }
};
exports.applicativeEffect = applicativeEffect;
var $lazy_functorEffect =
/* #__PURE__ */
$runtime_lazy("functorEffect", "Effect", function () {
  return {
    map: Control_Applicative.liftA1(applicativeEffect)
  };
});
var $lazy_applyEffect =
/* #__PURE__ */
$runtime_lazy("applyEffect", "Effect", function () {
  return {
    apply: Control_Monad.ap(monadEffect),
    Functor0: function Functor0() {
      return $lazy_functorEffect(0);
    }
  };
});
var functorEffect =
/* #__PURE__ */
$lazy_functorEffect(20);
exports.functorEffect = functorEffect;
var applyEffect =
/* #__PURE__ */
$lazy_applyEffect(23);
exports.applyEffect = applyEffect;
var lift2 =
/* #__PURE__ */
Control_Apply.lift2(applyEffect);

var semigroupEffect = function semigroupEffect(dictSemigroup) {
  return {
    append: lift2(Data_Semigroup.append(dictSemigroup))
  };
};

exports.semigroupEffect = semigroupEffect;

var monoidEffect = function monoidEffect(dictMonoid) {
  var semigroupEffect1 = semigroupEffect(dictMonoid.Semigroup0());
  return {
    mempty: $foreign.pureE(Data_Monoid.mempty(dictMonoid)),
    Semigroup0: function Semigroup0() {
      return semigroupEffect1;
    }
  };
};

exports.monoidEffect = monoidEffect;
},{"./foreign.js":"../output/Effect/foreign.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Monad/index.js":"../output/Control.Monad/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js"}],"../output/Effect.Exception/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.catchException = catchException;
exports.error = error;
exports.message = message;
exports.name = name;
exports.showErrorImpl = showErrorImpl;
exports.stackImpl = stackImpl;
exports.throwException = throwException;

function showErrorImpl(err) {
  return err.stack || err.toString();
}

function error(msg) {
  return new Error(msg);
}

function message(e) {
  return e.message;
}

function name(e) {
  return e.name || "Error";
}

function stackImpl(just) {
  return function (nothing) {
    return function (e) {
      return e.stack ? just(e.stack) : nothing;
    };
  };
}

function throwException(e) {
  return function () {
    throw e;
  };
}

function catchException(c) {
  return function (t) {
    return function () {
      try {
        return t();
      } catch (e) {
        if (e instanceof Error || Object.prototype.toString.call(e) === "[object Error]") {
          return c(e)();
        } else {
          return c(new Error(e.toString()))();
        }
      }
    };
  };
}
},{}],"../output/Effect.Exception/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "catchException", {
  enumerable: true,
  get: function () {
    return $foreign.catchException;
  }
});
Object.defineProperty(exports, "error", {
  enumerable: true,
  get: function () {
    return $foreign.error;
  }
});
Object.defineProperty(exports, "message", {
  enumerable: true,
  get: function () {
    return $foreign.message;
  }
});
Object.defineProperty(exports, "name", {
  enumerable: true,
  get: function () {
    return $foreign.name;
  }
});
exports.throw = exports.stack = exports.showError = void 0;
Object.defineProperty(exports, "throwException", {
  enumerable: true,
  get: function () {
    return $foreign.throwException;
  }
});
exports.try = void 0;

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Data_Either = _interopRequireWildcard(require("../Data.Either/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Maybe = _interopRequireWildcard(require("../Data.Maybe/index.js"));

var Effect = _interopRequireWildcard(require("../Effect/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var pure =
/* #__PURE__ */
Control_Applicative.pure(Effect.applicativeEffect);
var map =
/* #__PURE__ */
Data_Functor.map(Effect.functorEffect);

var $$try = function $$try(action) {
  return $foreign.catchException(function ($3) {
    return pure(Data_Either.Left.create($3));
  })(map(Data_Either.Right.create)(action));
};

exports.try = $$try;

var $$throw = function $$throw($4) {
  return $foreign.throwException($foreign.error($4));
};

exports.throw = $$throw;

var stack =
/* #__PURE__ */
function () {
  return $foreign.stackImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
}();

exports.stack = stack;
var showError = {
  show: $foreign.showErrorImpl
};
exports.showError = showError;
},{"./foreign.js":"../output/Effect.Exception/foreign.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Data.Either/index.js":"../output/Data.Either/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Maybe/index.js":"../output/Data.Maybe/index.js","../Effect/index.js":"../output/Effect/index.js"}],"../output/Control.Monad.Error.Class/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.withResource = exports.try = exports.throwError = exports.monadThrowMaybe = exports.monadThrowEither = exports.monadThrowEffect = exports.monadErrorMaybe = exports.monadErrorEither = exports.monadErrorEffect = exports.liftMaybe = exports.liftEither = exports.catchJust = exports.catchError = void 0;

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Data_Either = _interopRequireWildcard(require("../Data.Either/index.js"));

var Data_Function = _interopRequireWildcard(require("../Data.Function/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Maybe = _interopRequireWildcard(require("../Data.Maybe/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Effect = _interopRequireWildcard(require("../Effect/index.js"));

var Effect_Exception = _interopRequireWildcard(require("../Effect.Exception/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var discard =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit);

var throwError = function throwError(dict) {
  return dict.throwError;
};

exports.throwError = throwError;

var monadThrowMaybe =
/* #__PURE__ */
function () {
  return {
    throwError: Data_Function["const"](Data_Maybe.Nothing.value),
    Monad0: function Monad0() {
      return Data_Maybe.monadMaybe;
    }
  };
}();

exports.monadThrowMaybe = monadThrowMaybe;

var monadThrowEither =
/* #__PURE__ */
function () {
  return {
    throwError: Data_Either.Left.create,
    Monad0: function Monad0() {
      return Data_Either.monadEither;
    }
  };
}();

exports.monadThrowEither = monadThrowEither;
var monadThrowEffect = {
  throwError: Effect_Exception.throwException,
  Monad0: function Monad0() {
    return Effect.monadEffect;
  }
};
exports.monadThrowEffect = monadThrowEffect;
var monadErrorMaybe = {
  catchError: function catchError(v) {
    return function (v1) {
      if (v instanceof Data_Maybe.Nothing) {
        return v1(Data_Unit.unit);
      }

      ;

      if (v instanceof Data_Maybe.Just) {
        return new Data_Maybe.Just(v.value0);
      }

      ;
      throw new Error("Failed pattern match at Control.Monad.Error.Class (line 79, column 1 - line 81, column 33): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  MonadThrow0: function MonadThrow0() {
    return monadThrowMaybe;
  }
};
exports.monadErrorMaybe = monadErrorMaybe;
var monadErrorEither = {
  catchError: function catchError(v) {
    return function (v1) {
      if (v instanceof Data_Either.Left) {
        return v1(v.value0);
      }

      ;

      if (v instanceof Data_Either.Right) {
        return new Data_Either.Right(v.value0);
      }

      ;
      throw new Error("Failed pattern match at Control.Monad.Error.Class (line 72, column 1 - line 74, column 35): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  MonadThrow0: function MonadThrow0() {
    return monadThrowEither;
  }
};
exports.monadErrorEither = monadErrorEither;
var monadErrorEffect = {
  catchError:
  /* #__PURE__ */
  Data_Function.flip(Effect_Exception.catchException),
  MonadThrow0: function MonadThrow0() {
    return monadThrowEffect;
  }
};
exports.monadErrorEffect = monadErrorEffect;

var liftMaybe = function liftMaybe(dictMonadThrow) {
  var throwError1 = throwError(dictMonadThrow);
  var pure = Control_Applicative.pure(dictMonadThrow.Monad0().Applicative0());
  return function (error) {
    return Data_Maybe.maybe(throwError1(error))(pure);
  };
};

exports.liftMaybe = liftMaybe;

var liftEither = function liftEither(dictMonadThrow) {
  return Data_Either.either(throwError(dictMonadThrow))(Control_Applicative.pure(dictMonadThrow.Monad0().Applicative0()));
};

exports.liftEither = liftEither;

var catchError = function catchError(dict) {
  return dict.catchError;
};

exports.catchError = catchError;

var catchJust = function catchJust(dictMonadError) {
  var throwError1 = throwError(dictMonadError.MonadThrow0());
  var catchError1 = catchError(dictMonadError);
  return function (p) {
    return function (act) {
      return function (handler) {
        var handle = function handle(e) {
          var v = p(e);

          if (v instanceof Data_Maybe.Nothing) {
            return throwError1(e);
          }

          ;

          if (v instanceof Data_Maybe.Just) {
            return handler(v.value0);
          }

          ;
          throw new Error("Failed pattern match at Control.Monad.Error.Class (line 57, column 5 - line 59, column 26): " + [v.constructor.name]);
        };

        return catchError1(act)(handle);
      };
    };
  };
};

exports.catchJust = catchJust;

var $$try = function $$try(dictMonadError) {
  var catchError1 = catchError(dictMonadError);
  var Monad0 = dictMonadError.MonadThrow0().Monad0();
  var map = Data_Functor.map(Monad0.Bind1().Apply0().Functor0());
  var pure = Control_Applicative.pure(Monad0.Applicative0());
  return function (a) {
    return catchError1(map(Data_Either.Right.create)(a))(function ($52) {
      return pure(Data_Either.Left.create($52));
    });
  };
};

exports.try = $$try;

var withResource = function withResource(dictMonadError) {
  var MonadThrow0 = dictMonadError.MonadThrow0();
  var Monad0 = MonadThrow0.Monad0();
  var Bind1 = Monad0.Bind1();
  var bind = Control_Bind.bind(Bind1);
  var try1 = $$try(dictMonadError);
  var discard1 = discard(Bind1);
  var throwError1 = throwError(MonadThrow0);
  var pure = Control_Applicative.pure(Monad0.Applicative0());
  return function (acquire) {
    return function (release) {
      return function (kleisli) {
        return bind(acquire)(function (resource) {
          return bind(try1(kleisli(resource)))(function (result) {
            return discard1(release(resource))(function () {
              return Data_Either.either(throwError1)(pure)(result);
            });
          });
        });
      };
    };
  };
};

exports.withResource = withResource;
},{"../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Data.Either/index.js":"../output/Data.Either/index.js","../Data.Function/index.js":"../output/Data.Function/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Maybe/index.js":"../output/Data.Maybe/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Effect/index.js":"../output/Effect/index.js","../Effect.Exception/index.js":"../output/Effect.Exception/index.js"}],"../output/Data.Identity/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showIdentity = exports.semiringIdentity = exports.semigroupIdentity = exports.ringIdentity = exports.ordIdentity = exports.ord1Identity = exports.newtypeIdentity = exports.monoidIdentity = exports.monadIdentity = exports.lazyIdentity = exports.invariantIdentity = exports.heytingAlgebraIdentity = exports.functorIdentity = exports.extendIdentity = exports.euclideanRingIdentity = exports.eqIdentity = exports.eq1Identity = exports.comonadIdentity = exports.commutativeRingIdentity = exports.boundedIdentity = exports.booleanAlgebraIdentity = exports.bindIdentity = exports.applyIdentity = exports.applicativeIdentity = exports.altIdentity = exports.Identity = void 0;

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_Functor_Invariant = _interopRequireWildcard(require("../Data.Functor.Invariant/index.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var Identity = function Identity(x) {
  return x;
};

exports.Identity = Identity;

var showIdentity = function showIdentity(dictShow) {
  var _show = Data_Show.show(dictShow);

  return {
    show: function show(v) {
      return "(Identity " + (_show(v) + ")");
    }
  };
};

exports.showIdentity = showIdentity;

var semiringIdentity = function semiringIdentity(dictSemiring) {
  return dictSemiring;
};

exports.semiringIdentity = semiringIdentity;

var semigroupIdentity = function semigroupIdentity(dictSemigroup) {
  return dictSemigroup;
};

exports.semigroupIdentity = semigroupIdentity;

var ringIdentity = function ringIdentity(dictRing) {
  return dictRing;
};

exports.ringIdentity = ringIdentity;

var ordIdentity = function ordIdentity(dictOrd) {
  return dictOrd;
};

exports.ordIdentity = ordIdentity;
var newtypeIdentity = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeIdentity = newtypeIdentity;

var monoidIdentity = function monoidIdentity(dictMonoid) {
  return dictMonoid;
};

exports.monoidIdentity = monoidIdentity;

var lazyIdentity = function lazyIdentity(dictLazy) {
  return dictLazy;
};

exports.lazyIdentity = lazyIdentity;

var heytingAlgebraIdentity = function heytingAlgebraIdentity(dictHeytingAlgebra) {
  return dictHeytingAlgebra;
};

exports.heytingAlgebraIdentity = heytingAlgebraIdentity;
var functorIdentity = {
  map: function map(f) {
    return function (m) {
      return f(m);
    };
  }
};
exports.functorIdentity = functorIdentity;
var invariantIdentity = {
  imap:
  /* #__PURE__ */
  Data_Functor_Invariant.imapF(functorIdentity)
};
exports.invariantIdentity = invariantIdentity;
var extendIdentity = {
  extend: function extend(f) {
    return function (m) {
      return f(m);
    };
  },
  Functor0: function Functor0() {
    return functorIdentity;
  }
};
exports.extendIdentity = extendIdentity;

var euclideanRingIdentity = function euclideanRingIdentity(dictEuclideanRing) {
  return dictEuclideanRing;
};

exports.euclideanRingIdentity = euclideanRingIdentity;

var eqIdentity = function eqIdentity(dictEq) {
  return dictEq;
};

exports.eqIdentity = eqIdentity;
var eq1Identity = {
  eq1: function eq1(dictEq) {
    return Data_Eq.eq(eqIdentity(dictEq));
  }
};
exports.eq1Identity = eq1Identity;
var ord1Identity = {
  compare1: function compare1(dictOrd) {
    return Data_Ord.compare(ordIdentity(dictOrd));
  },
  Eq10: function Eq10() {
    return eq1Identity;
  }
};
exports.ord1Identity = ord1Identity;
var comonadIdentity = {
  extract: function extract(v) {
    return v;
  },
  Extend0: function Extend0() {
    return extendIdentity;
  }
};
exports.comonadIdentity = comonadIdentity;

var commutativeRingIdentity = function commutativeRingIdentity(dictCommutativeRing) {
  return dictCommutativeRing;
};

exports.commutativeRingIdentity = commutativeRingIdentity;

var boundedIdentity = function boundedIdentity(dictBounded) {
  return dictBounded;
};

exports.boundedIdentity = boundedIdentity;

var booleanAlgebraIdentity = function booleanAlgebraIdentity(dictBooleanAlgebra) {
  return dictBooleanAlgebra;
};

exports.booleanAlgebraIdentity = booleanAlgebraIdentity;
var applyIdentity = {
  apply: function apply(v) {
    return function (v1) {
      return v(v1);
    };
  },
  Functor0: function Functor0() {
    return functorIdentity;
  }
};
exports.applyIdentity = applyIdentity;
var bindIdentity = {
  bind: function bind(v) {
    return function (f) {
      return f(v);
    };
  },
  Apply0: function Apply0() {
    return applyIdentity;
  }
};
exports.bindIdentity = bindIdentity;
var applicativeIdentity = {
  pure: Identity,
  Apply0: function Apply0() {
    return applyIdentity;
  }
};
exports.applicativeIdentity = applicativeIdentity;
var monadIdentity = {
  Applicative0: function Applicative0() {
    return applicativeIdentity;
  },
  Bind1: function Bind1() {
    return bindIdentity;
  }
};
exports.monadIdentity = monadIdentity;
var altIdentity = {
  alt: function alt(x) {
    return function (v) {
      return x;
    };
  },
  Functor0: function Functor0() {
    return functorIdentity;
  }
};
exports.altIdentity = altIdentity;
},{"../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.Functor.Invariant/index.js":"../output/Data.Functor.Invariant/index.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Show/index.js":"../output/Data.Show/index.js"}],"../output/Effect.Ref/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.write = exports.read = exports.newWithSelf = exports.modifyImpl = exports._new = void 0;

var _new = function _new(val) {
  return function () {
    return {
      value: val
    };
  };
};

exports._new = _new;

var newWithSelf = function newWithSelf(f) {
  return function () {
    var ref = {
      value: null
    };
    ref.value = f(ref);
    return ref;
  };
};

exports.newWithSelf = newWithSelf;

var read = function read(ref) {
  return function () {
    return ref.value;
  };
};

exports.read = read;

var modifyImpl = function modifyImpl(f) {
  return function (ref) {
    return function () {
      var t = f(ref.value);
      ref.value = t.state;
      return t.value;
    };
  };
};

exports.modifyImpl = modifyImpl;

var write = function write(val) {
  return function (ref) {
    return function () {
      ref.value = val;
    };
  };
};

exports.write = write;
},{}],"../output/Effect.Ref/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.new = exports.modify_ = exports.modify$prime = exports.modify = void 0;
Object.defineProperty(exports, "newWithSelf", {
  enumerable: true,
  get: function () {
    return $foreign.newWithSelf;
  }
});
Object.defineProperty(exports, "read", {
  enumerable: true,
  get: function () {
    return $foreign.read;
  }
});
Object.defineProperty(exports, "write", {
  enumerable: true,
  get: function () {
    return $foreign.write;
  }
});

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Effect = _interopRequireWildcard(require("../Effect/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var $$void =
/* #__PURE__ */
Data_Functor["void"](Effect.functorEffect);
var $$new = $foreign["_new"];
exports.new = $$new;
var modify$prime = $foreign.modifyImpl;
exports.modify$prime = modify$prime;

var modify = function modify(f) {
  return modify$prime(function (s) {
    var s$prime = f(s);
    return {
      state: s$prime,
      value: s$prime
    };
  });
};

exports.modify = modify;

var modify_ = function modify_(f) {
  return function (s) {
    return $$void(modify(f)(s));
  };
};

exports.modify_ = modify_;
},{"./foreign.js":"../output/Effect.Ref/foreign.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Effect/index.js":"../output/Effect/index.js"}],"../output/Control.Monad.Rec.Class/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.whileJust = exports.untilJust = exports.tailRecM3 = exports.tailRecM2 = exports.tailRecM = exports.tailRec3 = exports.tailRec2 = exports.tailRec = exports.monadRecMaybe = exports.monadRecIdentity = exports.monadRecFunction = exports.monadRecEither = exports.monadRecEffect = exports.loop3 = exports.loop2 = exports.functorStep = exports.forever = exports.bifunctorStep = exports.Loop = exports.Done = void 0;

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Monad = _interopRequireWildcard(require("../Control.Monad/index.js"));

var Data_Either = _interopRequireWildcard(require("../Data.Either/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Identity = _interopRequireWildcard(require("../Data.Identity/index.js"));

var Data_Maybe = _interopRequireWildcard(require("../Data.Maybe/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Effect = _interopRequireWildcard(require("../Effect/index.js"));

var Effect_Ref = _interopRequireWildcard(require("../Effect.Ref/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var bindFlipped =
/* #__PURE__ */
Control_Bind.bindFlipped(Effect.bindEffect);
var map =
/* #__PURE__ */
Data_Functor.map(Effect.functorEffect);

var Loop =
/* #__PURE__ */
function () {
  function Loop(value0) {
    this.value0 = value0;
  }

  ;

  Loop.create = function (value0) {
    return new Loop(value0);
  };

  return Loop;
}();

exports.Loop = Loop;

var Done =
/* #__PURE__ */
function () {
  function Done(value0) {
    this.value0 = value0;
  }

  ;

  Done.create = function (value0) {
    return new Done(value0);
  };

  return Done;
}();

exports.Done = Done;

var tailRecM = function tailRecM(dict) {
  return dict.tailRecM;
};

exports.tailRecM = tailRecM;

var tailRecM2 = function tailRecM2(dictMonadRec) {
  var tailRecM1 = tailRecM(dictMonadRec);
  return function (f) {
    return function (a) {
      return function (b) {
        return tailRecM1(function (o) {
          return f(o.a)(o.b);
        })({
          a: a,
          b: b
        });
      };
    };
  };
};

exports.tailRecM2 = tailRecM2;

var tailRecM3 = function tailRecM3(dictMonadRec) {
  var tailRecM1 = tailRecM(dictMonadRec);
  return function (f) {
    return function (a) {
      return function (b) {
        return function (c) {
          return tailRecM1(function (o) {
            return f(o.a)(o.b)(o.c);
          })({
            a: a,
            b: b,
            c: c
          });
        };
      };
    };
  };
};

exports.tailRecM3 = tailRecM3;

var untilJust = function untilJust(dictMonadRec) {
  var tailRecM1 = tailRecM(dictMonadRec);
  var mapFlipped = Data_Functor.mapFlipped(dictMonadRec.Monad0().Bind1().Apply0().Functor0());
  return function (m) {
    return tailRecM1(function (v) {
      return mapFlipped(m)(function (v1) {
        if (v1 instanceof Data_Maybe.Nothing) {
          return new Loop(Data_Unit.unit);
        }

        ;

        if (v1 instanceof Data_Maybe.Just) {
          return new Done(v1.value0);
        }

        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 179, column 43 - line 181, column 19): " + [v1.constructor.name]);
      });
    })(Data_Unit.unit);
  };
};

exports.untilJust = untilJust;

var whileJust = function whileJust(dictMonoid) {
  var append = Data_Semigroup.append(dictMonoid.Semigroup0());
  var mempty = Data_Monoid.mempty(dictMonoid);
  return function (dictMonadRec) {
    var tailRecM1 = tailRecM(dictMonadRec);
    var mapFlipped = Data_Functor.mapFlipped(dictMonadRec.Monad0().Bind1().Apply0().Functor0());
    return function (m) {
      return tailRecM1(function (v) {
        return mapFlipped(m)(function (v1) {
          if (v1 instanceof Data_Maybe.Nothing) {
            return new Done(v);
          }

          ;

          if (v1 instanceof Data_Maybe.Just) {
            return new Loop(append(v)(v1.value0));
          }

          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 172, column 45 - line 174, column 26): " + [v1.constructor.name]);
        });
      })(mempty);
    };
  };
};

exports.whileJust = whileJust;

var tailRec = function tailRec(f) {
  var go = function go($copy_v) {
    var $tco_done = false;
    var $tco_result;

    function $tco_loop(v) {
      if (v instanceof Loop) {
        $copy_v = f(v.value0);
        return;
      }

      ;

      if (v instanceof Done) {
        $tco_done = true;
        return v.value0;
      }

      ;
      throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 103, column 3 - line 103, column 25): " + [v.constructor.name]);
    }

    ;

    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }

    ;
    return $tco_result;
  };

  return function ($85) {
    return go(f($85));
  };
};

exports.tailRec = tailRec;

var tailRec2 = function tailRec2(f) {
  return function (a) {
    return function (b) {
      return tailRec(function (o) {
        return f(o.a)(o.b);
      })({
        a: a,
        b: b
      });
    };
  };
};

exports.tailRec2 = tailRec2;

var tailRec3 = function tailRec3(f) {
  return function (a) {
    return function (b) {
      return function (c) {
        return tailRec(function (o) {
          return f(o.a)(o.b)(o.c);
        })({
          a: a,
          b: b,
          c: c
        });
      };
    };
  };
};

exports.tailRec3 = tailRec3;
var monadRecMaybe = {
  tailRecM: function tailRecM(f) {
    return function (a0) {
      var g = function g(v) {
        if (v instanceof Data_Maybe.Nothing) {
          return new Done(Data_Maybe.Nothing.value);
        }

        ;

        if (v instanceof Data_Maybe.Just && v.value0 instanceof Loop) {
          return new Loop(f(v.value0.value0));
        }

        ;

        if (v instanceof Data_Maybe.Just && v.value0 instanceof Done) {
          return new Done(new Data_Maybe.Just(v.value0.value0));
        }

        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 153, column 7 - line 153, column 31): " + [v.constructor.name]);
      };

      return tailRec(g)(f(a0));
    };
  },
  Monad0: function Monad0() {
    return Data_Maybe.monadMaybe;
  }
};
exports.monadRecMaybe = monadRecMaybe;
var monadRecIdentity = {
  tailRecM: function tailRecM(f) {
    var runIdentity = function runIdentity(v) {
      return v;
    };

    var $86 = tailRec(function ($88) {
      return runIdentity(f($88));
    });
    return function ($87) {
      return Data_Identity.Identity($86($87));
    };
  },
  Monad0: function Monad0() {
    return Data_Identity.monadIdentity;
  }
};
exports.monadRecIdentity = monadRecIdentity;
var monadRecFunction = {
  tailRecM: function tailRecM(f) {
    return function (a0) {
      return function (e) {
        return tailRec(function (a) {
          return f(a)(e);
        })(a0);
      };
    };
  },
  Monad0: function Monad0() {
    return Control_Monad.monadFn;
  }
};
exports.monadRecFunction = monadRecFunction;
var monadRecEither = {
  tailRecM: function tailRecM(f) {
    return function (a0) {
      var g = function g(v) {
        if (v instanceof Data_Either.Left) {
          return new Done(new Data_Either.Left(v.value0));
        }

        ;

        if (v instanceof Data_Either.Right && v.value0 instanceof Loop) {
          return new Loop(f(v.value0.value0));
        }

        ;

        if (v instanceof Data_Either.Right && v.value0 instanceof Done) {
          return new Done(new Data_Either.Right(v.value0.value0));
        }

        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 145, column 7 - line 145, column 33): " + [v.constructor.name]);
      };

      return tailRec(g)(f(a0));
    };
  },
  Monad0: function Monad0() {
    return Data_Either.monadEither;
  }
};
exports.monadRecEither = monadRecEither;
var monadRecEffect = {
  tailRecM: function tailRecM(f) {
    return function (a) {
      var fromDone = function fromDone(v) {
        if (v instanceof Done) {
          return v.value0;
        }

        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 137, column 30 - line 137, column 44): " + [v.constructor.name]);
      };

      return function __do() {
        var r = bindFlipped(Effect_Ref["new"])(f(a))();

        (function () {
          while (!function __do() {
            var v = Effect_Ref.read(r)();

            if (v instanceof Loop) {
              var e = f(v.value0)();
              Effect_Ref.write(e)(r)();
              return false;
            }

            ;

            if (v instanceof Done) {
              return true;
            }

            ;
            throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 128, column 22 - line 133, column 28): " + [v.constructor.name]);
          }()) {}

          ;
          return {};
        })();

        return map(fromDone)(Effect_Ref.read(r))();
      };
    };
  },
  Monad0: function Monad0() {
    return Effect.monadEffect;
  }
};
exports.monadRecEffect = monadRecEffect;

var loop3 = function loop3(a) {
  return function (b) {
    return function (c) {
      return new Loop({
        a: a,
        b: b,
        c: c
      });
    };
  };
};

exports.loop3 = loop3;

var loop2 = function loop2(a) {
  return function (b) {
    return new Loop({
      a: a,
      b: b
    });
  };
};

exports.loop2 = loop2;
var functorStep = {
  map: function map(f) {
    return function (m) {
      if (m instanceof Loop) {
        return new Loop(m.value0);
      }

      ;

      if (m instanceof Done) {
        return new Done(f(m.value0));
      }

      ;
      throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
    };
  }
};
exports.functorStep = functorStep;

var forever = function forever(dictMonadRec) {
  var tailRecM1 = tailRecM(dictMonadRec);
  var voidRight = Data_Functor.voidRight(dictMonadRec.Monad0().Bind1().Apply0().Functor0());
  return function (ma) {
    return tailRecM1(function (u) {
      return voidRight(new Loop(u))(ma);
    })(Data_Unit.unit);
  };
};

exports.forever = forever;
var bifunctorStep = {
  bimap: function bimap(v) {
    return function (v1) {
      return function (v2) {
        if (v2 instanceof Loop) {
          return new Loop(v(v2.value0));
        }

        ;

        if (v2 instanceof Done) {
          return new Done(v1(v2.value0));
        }

        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 33, column 1 - line 35, column 34): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  }
};
exports.bifunctorStep = bifunctorStep;
},{"../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Monad/index.js":"../output/Control.Monad/index.js","../Data.Either/index.js":"../output/Data.Either/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Identity/index.js":"../output/Data.Identity/index.js","../Data.Maybe/index.js":"../output/Data.Maybe/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Effect/index.js":"../output/Effect/index.js","../Effect.Ref/index.js":"../output/Effect.Ref/index.js"}],"../output/Unsafe.Coerce/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.unsafeCoerce = void 0;

// module Unsafe.Coerce
var unsafeCoerce = function unsafeCoerce(x) {
  return x;
};

exports.unsafeCoerce = unsafeCoerce;
},{}],"../output/Unsafe.Coerce/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "unsafeCoerce", {
  enumerable: true,
  get: function () {
    return $foreign.unsafeCoerce;
  }
});

var $foreign = _interopRequireWildcard(require("./foreign.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }
},{"./foreign.js":"../output/Unsafe.Coerce/foreign.js"}],"../output/Control.Monad.ST.Global/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.toEffect = void 0;

var Unsafe_Coerce = _interopRequireWildcard(require("../Unsafe.Coerce/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var toEffect = Unsafe_Coerce.unsafeCoerce;
exports.toEffect = toEffect;
},{"../Unsafe.Coerce/index.js":"../output/Unsafe.Coerce/index.js"}],"../output/Control.Monad.ST.Internal/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.bind_ = void 0;
exports.for = forST;
exports.modifyImpl = exports.map_ = exports.foreach = void 0;
exports.new = newSTRef;
exports.run = exports.read = exports.pure_ = void 0;
exports.while = whileST;
exports.write = void 0;

var map_ = function map_(f) {
  return function (a) {
    return function () {
      return f(a());
    };
  };
};

exports.map_ = map_;

var pure_ = function pure_(a) {
  return function () {
    return a;
  };
};

exports.pure_ = pure_;

var bind_ = function bind_(a) {
  return function (f) {
    return function () {
      return f(a())();
    };
  };
};

exports.bind_ = bind_;

var run = function run(f) {
  return f();
};

exports.run = run;

function whileST(f) {
  return function (a) {
    return function () {
      while (f()) {
        a();
      }
    };
  };
}

function forST(lo) {
  return function (hi) {
    return function (f) {
      return function () {
        for (var i = lo; i < hi; i++) {
          f(i)();
        }
      };
    };
  };
}

var foreach = function foreach(as) {
  return function (f) {
    return function () {
      for (var i = 0, l = as.length; i < l; i++) {
        f(as[i])();
      }
    };
  };
};

exports.foreach = foreach;

function newSTRef(val) {
  return function () {
    return {
      value: val
    };
  };
}

var read = function read(ref) {
  return function () {
    return ref.value;
  };
};

exports.read = read;

var modifyImpl = function modifyImpl(f) {
  return function (ref) {
    return function () {
      var t = f(ref.value);
      ref.value = t.state;
      return t.value;
    };
  };
};

exports.modifyImpl = modifyImpl;

var write = function write(a) {
  return function (ref) {
    return function () {
      return ref.value = a; // eslint-disable-line no-return-assign
    };
  };
};

exports.write = write;
},{}],"../output/Control.Monad.ST.Internal/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.bindST = exports.applyST = exports.applicativeST = void 0;
Object.defineProperty(exports, "for", {
  enumerable: true,
  get: function () {
    return $foreign.for;
  }
});
Object.defineProperty(exports, "foreach", {
  enumerable: true,
  get: function () {
    return $foreign.foreach;
  }
});
exports.monoidST = exports.monadST = exports.monadRecST = exports.modify$prime = exports.modify = exports.functorST = void 0;
Object.defineProperty(exports, "new", {
  enumerable: true,
  get: function () {
    return $foreign.new;
  }
});
Object.defineProperty(exports, "read", {
  enumerable: true,
  get: function () {
    return $foreign.read;
  }
});
Object.defineProperty(exports, "run", {
  enumerable: true,
  get: function () {
    return $foreign.run;
  }
});
exports.semigroupST = void 0;
Object.defineProperty(exports, "while", {
  enumerable: true,
  get: function () {
    return $foreign.while;
  }
});
Object.defineProperty(exports, "write", {
  enumerable: true,
  get: function () {
    return $foreign.write;
  }
});

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Monad = _interopRequireWildcard(require("../Control.Monad/index.js"));

var Control_Monad_Rec_Class = _interopRequireWildcard(require("../Control.Monad.Rec.Class/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var $runtime_lazy = function $runtime_lazy(name, moduleName, init) {
  var state = 0;
  var val;
  return function (lineNumber) {
    if (state === 2) return val;
    if (state === 1) throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state = 1;
    val = init();
    state = 2;
    return val;
  };
};

var modify$prime = $foreign.modifyImpl;
exports.modify$prime = modify$prime;

var modify = function modify(f) {
  return modify$prime(function (s) {
    var s$prime = f(s);
    return {
      state: s$prime,
      value: s$prime
    };
  });
};

exports.modify = modify;
var functorST = {
  map: $foreign.map_
};
exports.functorST = functorST;
var map =
/* #__PURE__ */
Data_Functor.map(functorST);
var $$void =
/* #__PURE__ */
Data_Functor["void"](functorST);
var monadST = {
  Applicative0: function Applicative0() {
    return applicativeST;
  },
  Bind1: function Bind1() {
    return bindST;
  }
};
exports.monadST = monadST;
var bindST = {
  bind: $foreign.bind_,
  Apply0: function Apply0() {
    return $lazy_applyST(0);
  }
};
exports.bindST = bindST;
var applicativeST = {
  pure: $foreign.pure_,
  Apply0: function Apply0() {
    return $lazy_applyST(0);
  }
};
exports.applicativeST = applicativeST;
var $lazy_applyST =
/* #__PURE__ */
$runtime_lazy("applyST", "Control.Monad.ST.Internal", function () {
  return {
    apply: Control_Monad.ap(monadST),
    Functor0: function Functor0() {
      return functorST;
    }
  };
});
var applyST =
/* #__PURE__ */
$lazy_applyST(47);
exports.applyST = applyST;
var lift2 =
/* #__PURE__ */
Control_Apply.lift2(applyST);
var bind =
/* #__PURE__ */
Control_Bind.bind(bindST);
var bindFlipped =
/* #__PURE__ */
Control_Bind.bindFlipped(bindST);
var discard =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit)(bindST);
var pure =
/* #__PURE__ */
Control_Applicative.pure(applicativeST);

var semigroupST = function semigroupST(dictSemigroup) {
  return {
    append: lift2(Data_Semigroup.append(dictSemigroup))
  };
};

exports.semigroupST = semigroupST;
var monadRecST = {
  tailRecM: function tailRecM(f) {
    return function (a) {
      var isLooping = function isLooping(v) {
        if (v instanceof Control_Monad_Rec_Class.Loop) {
          return true;
        }

        ;
        return false;
      };

      var fromDone = function fromDone(v) {
        if (v instanceof Control_Monad_Rec_Class.Done) {
          return v.value0;
        }

        ;
        throw new Error("Failed pattern match at Control.Monad.ST.Internal (line 70, column 32 - line 70, column 46): " + [v.constructor.name]);
      };

      return bind(bindFlipped($foreign["new"])(f(a)))(function (r) {
        return discard($foreign["while"](map(isLooping)($foreign.read(r)))(bind($foreign.read(r))(function (v) {
          if (v instanceof Control_Monad_Rec_Class.Loop) {
            return bind(f(v.value0))(function (e) {
              return $$void($foreign.write(e)(r));
            });
          }

          ;

          if (v instanceof Control_Monad_Rec_Class.Done) {
            return pure(Data_Unit.unit);
          }

          ;
          throw new Error("Failed pattern match at Control.Monad.ST.Internal (line 62, column 18 - line 66, column 28): " + [v.constructor.name]);
        })))(function () {
          return map(fromDone)($foreign.read(r));
        });
      });
    };
  },
  Monad0: function Monad0() {
    return monadST;
  }
};
exports.monadRecST = monadRecST;

var monoidST = function monoidST(dictMonoid) {
  var semigroupST1 = semigroupST(dictMonoid.Semigroup0());
  return {
    mempty: pure(Data_Monoid.mempty(dictMonoid)),
    Semigroup0: function Semigroup0() {
      return semigroupST1;
    }
  };
};

exports.monoidST = monoidST;
},{"./foreign.js":"../output/Control.Monad.ST.Internal/foreign.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Monad/index.js":"../output/Control.Monad/index.js","../Control.Monad.Rec.Class/index.js":"../output/Control.Monad.Rec.Class/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js"}],"../output/Control.Monad.ST.Class/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.monadSTST = exports.monadSTEffect = exports.liftST = void 0;

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Control_Monad_ST_Global = _interopRequireWildcard(require("../Control.Monad.ST.Global/index.js"));

var Control_Monad_ST_Internal = _interopRequireWildcard(require("../Control.Monad.ST.Internal/index.js"));

var Effect = _interopRequireWildcard(require("../Effect/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var monadSTST = {
  liftST:
  /* #__PURE__ */
  Control_Category.identity(Control_Category.categoryFn),
  Monad0: function Monad0() {
    return Control_Monad_ST_Internal.monadST;
  }
};
exports.monadSTST = monadSTST;
var monadSTEffect = {
  liftST: Control_Monad_ST_Global.toEffect,
  Monad0: function Monad0() {
    return Effect.monadEffect;
  }
};
exports.monadSTEffect = monadSTEffect;

var liftST = function liftST(dict) {
  return dict.liftST;
};

exports.liftST = liftST;
},{"../Control.Category/index.js":"../output/Control.Category/index.js","../Control.Monad.ST.Global/index.js":"../output/Control.Monad.ST.Global/index.js","../Control.Monad.ST.Internal/index.js":"../output/Control.Monad.ST.Internal/index.js","../Effect/index.js":"../output/Effect/index.js"}],"../output/Control.Monad.Cont.Class/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.callCC = void 0;

// Generated by purs version 0.15.10
var callCC = function callCC(dict) {
  return dict.callCC;
};

exports.callCC = callCC;
},{}],"../output/Control.Monad.Reader.Class/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.monadReaderFun = exports.monadAskFun = exports.local = exports.asks = exports.ask = void 0;

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Control_Monad = _interopRequireWildcard(require("../Control.Monad/index.js"));

var Control_Semigroupoid = _interopRequireWildcard(require("../Control.Semigroupoid/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var monadAskFun = {
  ask:
  /* #__PURE__ */
  Control_Category.identity(Control_Category.categoryFn),
  Monad0: function Monad0() {
    return Control_Monad.monadFn;
  }
};
exports.monadAskFun = monadAskFun;
var monadReaderFun = {
  local:
  /* #__PURE__ */
  Control_Semigroupoid.composeFlipped(Control_Semigroupoid.semigroupoidFn),
  MonadAsk0: function MonadAsk0() {
    return monadAskFun;
  }
};
exports.monadReaderFun = monadReaderFun;

var local = function local(dict) {
  return dict.local;
};

exports.local = local;

var ask = function ask(dict) {
  return dict.ask;
};

exports.ask = ask;

var asks = function asks(dictMonadAsk) {
  var map = Data_Functor.map(dictMonadAsk.Monad0().Bind1().Apply0().Functor0());
  var ask1 = ask(dictMonadAsk);
  return function (f) {
    return map(f)(ask1);
  };
};

exports.asks = asks;
},{"../Control.Category/index.js":"../output/Control.Category/index.js","../Control.Monad/index.js":"../output/Control.Monad/index.js","../Control.Semigroupoid/index.js":"../output/Control.Semigroupoid/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js"}],"../output/Control.Lazy/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.lazyUnit = exports.lazyFn = exports.fix = exports.defer = void 0;

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var $runtime_lazy = function $runtime_lazy(name, moduleName, init) {
  var state = 0;
  var val;
  return function (lineNumber) {
    if (state === 2) return val;
    if (state === 1) throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state = 1;
    val = init();
    state = 2;
    return val;
  };
};

var lazyUnit = {
  defer: function defer(v) {
    return Data_Unit.unit;
  }
};
exports.lazyUnit = lazyUnit;
var lazyFn = {
  defer: function defer(f) {
    return function (x) {
      return f(Data_Unit.unit)(x);
    };
  }
};
exports.lazyFn = lazyFn;

var defer = function defer(dict) {
  return dict.defer;
};

exports.defer = defer;

var fix = function fix(dictLazy) {
  var defer1 = defer(dictLazy);
  return function (f) {
    var $lazy_go = $runtime_lazy("go", "Control.Lazy", function () {
      return defer1(function (v) {
        return f($lazy_go(25));
      });
    });
    var go = $lazy_go(25);
    return go;
  };
};

exports.fix = fix;
},{"../Data.Unit/index.js":"../output/Data.Unit/index.js"}],"../output/Data.HeytingAlgebra/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.boolNot = exports.boolDisj = exports.boolConj = void 0;

var boolConj = function boolConj(b1) {
  return function (b2) {
    return b1 && b2;
  };
};

exports.boolConj = boolConj;

var boolDisj = function boolDisj(b1) {
  return function (b2) {
    return b1 || b2;
  };
};

exports.boolDisj = boolDisj;

var boolNot = function boolNot(b) {
  return !b;
};

exports.boolNot = boolNot;
},{}],"../output/Data.HeytingAlgebra/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ttRecord = exports.tt = exports.notRecord = exports.not = exports.impliesRecord = exports.implies = exports.heytingAlgebraUnit = exports.heytingAlgebraRecordNil = exports.heytingAlgebraRecordCons = exports.heytingAlgebraRecord = exports.heytingAlgebraProxy = exports.heytingAlgebraFunction = exports.heytingAlgebraBoolean = exports.ffRecord = exports.ff = exports.disjRecord = exports.disj = exports.conjRecord = exports.conj = void 0;

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Data_Symbol = _interopRequireWildcard(require("../Data.Symbol/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Record_Unsafe = _interopRequireWildcard(require("../Record.Unsafe/index.js"));

var Type_Proxy = _interopRequireWildcard(require("../Type.Proxy/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var ttRecord = function ttRecord(dict) {
  return dict.ttRecord;
};

exports.ttRecord = ttRecord;

var tt = function tt(dict) {
  return dict.tt;
};

exports.tt = tt;

var notRecord = function notRecord(dict) {
  return dict.notRecord;
};

exports.notRecord = notRecord;

var not = function not(dict) {
  return dict.not;
};

exports.not = not;

var impliesRecord = function impliesRecord(dict) {
  return dict.impliesRecord;
};

exports.impliesRecord = impliesRecord;

var implies = function implies(dict) {
  return dict.implies;
};

exports.implies = implies;
var heytingAlgebraUnit = {
  ff: Data_Unit.unit,
  tt: Data_Unit.unit,
  implies: function implies(v) {
    return function (v1) {
      return Data_Unit.unit;
    };
  },
  conj: function conj(v) {
    return function (v1) {
      return Data_Unit.unit;
    };
  },
  disj: function disj(v) {
    return function (v1) {
      return Data_Unit.unit;
    };
  },
  not: function not(v) {
    return Data_Unit.unit;
  }
};
exports.heytingAlgebraUnit = heytingAlgebraUnit;
var heytingAlgebraRecordNil = {
  conjRecord: function conjRecord(v) {
    return function (v1) {
      return function (v2) {
        return {};
      };
    };
  },
  disjRecord: function disjRecord(v) {
    return function (v1) {
      return function (v2) {
        return {};
      };
    };
  },
  ffRecord: function ffRecord(v) {
    return function (v1) {
      return {};
    };
  },
  impliesRecord: function impliesRecord(v) {
    return function (v1) {
      return function (v2) {
        return {};
      };
    };
  },
  notRecord: function notRecord(v) {
    return function (v1) {
      return {};
    };
  },
  ttRecord: function ttRecord(v) {
    return function (v1) {
      return {};
    };
  }
};
exports.heytingAlgebraRecordNil = heytingAlgebraRecordNil;

var heytingAlgebraProxy =
/* #__PURE__ */
function () {
  return {
    conj: function conj(v) {
      return function (v1) {
        return Type_Proxy["Proxy"].value;
      };
    },
    disj: function disj(v) {
      return function (v1) {
        return Type_Proxy["Proxy"].value;
      };
    },
    implies: function implies(v) {
      return function (v1) {
        return Type_Proxy["Proxy"].value;
      };
    },
    ff: Type_Proxy["Proxy"].value,
    not: function not(v) {
      return Type_Proxy["Proxy"].value;
    },
    tt: Type_Proxy["Proxy"].value
  };
}();

exports.heytingAlgebraProxy = heytingAlgebraProxy;

var ffRecord = function ffRecord(dict) {
  return dict.ffRecord;
};

exports.ffRecord = ffRecord;

var ff = function ff(dict) {
  return dict.ff;
};

exports.ff = ff;

var disjRecord = function disjRecord(dict) {
  return dict.disjRecord;
};

exports.disjRecord = disjRecord;

var disj = function disj(dict) {
  return dict.disj;
};

exports.disj = disj;
var heytingAlgebraBoolean = {
  ff: false,
  tt: true,
  implies: function implies(a) {
    return function (b) {
      return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
    };
  },
  conj: $foreign.boolConj,
  disj: $foreign.boolDisj,
  not: $foreign.boolNot
};
exports.heytingAlgebraBoolean = heytingAlgebraBoolean;

var conjRecord = function conjRecord(dict) {
  return dict.conjRecord;
};

exports.conjRecord = conjRecord;

var heytingAlgebraRecord = function heytingAlgebraRecord() {
  return function (dictHeytingAlgebraRecord) {
    return {
      ff: ffRecord(dictHeytingAlgebraRecord)(Type_Proxy["Proxy"].value)(Type_Proxy["Proxy"].value),
      tt: ttRecord(dictHeytingAlgebraRecord)(Type_Proxy["Proxy"].value)(Type_Proxy["Proxy"].value),
      conj: conjRecord(dictHeytingAlgebraRecord)(Type_Proxy["Proxy"].value),
      disj: disjRecord(dictHeytingAlgebraRecord)(Type_Proxy["Proxy"].value),
      implies: impliesRecord(dictHeytingAlgebraRecord)(Type_Proxy["Proxy"].value),
      not: notRecord(dictHeytingAlgebraRecord)(Type_Proxy["Proxy"].value)
    };
  };
};

exports.heytingAlgebraRecord = heytingAlgebraRecord;

var conj = function conj(dict) {
  return dict.conj;
};

exports.conj = conj;

var heytingAlgebraFunction = function heytingAlgebraFunction(dictHeytingAlgebra) {
  var ff1 = ff(dictHeytingAlgebra);
  var tt1 = tt(dictHeytingAlgebra);
  var implies1 = implies(dictHeytingAlgebra);
  var conj1 = conj(dictHeytingAlgebra);
  var disj1 = disj(dictHeytingAlgebra);
  var not1 = not(dictHeytingAlgebra);
  return {
    ff: function ff(v) {
      return ff1;
    },
    tt: function tt(v) {
      return tt1;
    },
    implies: function implies(f) {
      return function (g) {
        return function (a) {
          return implies1(f(a))(g(a));
        };
      };
    },
    conj: function conj(f) {
      return function (g) {
        return function (a) {
          return conj1(f(a))(g(a));
        };
      };
    },
    disj: function disj(f) {
      return function (g) {
        return function (a) {
          return disj1(f(a))(g(a));
        };
      };
    },
    not: function not(f) {
      return function (a) {
        return not1(f(a));
      };
    }
  };
};

exports.heytingAlgebraFunction = heytingAlgebraFunction;

var heytingAlgebraRecordCons = function heytingAlgebraRecordCons(dictIsSymbol) {
  var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
  return function () {
    return function (dictHeytingAlgebraRecord) {
      var conjRecord1 = conjRecord(dictHeytingAlgebraRecord);
      var disjRecord1 = disjRecord(dictHeytingAlgebraRecord);
      var impliesRecord1 = impliesRecord(dictHeytingAlgebraRecord);
      var ffRecord1 = ffRecord(dictHeytingAlgebraRecord);
      var notRecord1 = notRecord(dictHeytingAlgebraRecord);
      var ttRecord1 = ttRecord(dictHeytingAlgebraRecord);
      return function (dictHeytingAlgebra) {
        var conj1 = conj(dictHeytingAlgebra);
        var disj1 = disj(dictHeytingAlgebra);
        var implies1 = implies(dictHeytingAlgebra);
        var ff1 = ff(dictHeytingAlgebra);
        var not1 = not(dictHeytingAlgebra);
        var tt1 = tt(dictHeytingAlgebra);
        return {
          conjRecord: function conjRecord(v) {
            return function (ra) {
              return function (rb) {
                var tail = conjRecord1(Type_Proxy["Proxy"].value)(ra)(rb);
                var key = reflectSymbol(Type_Proxy["Proxy"].value);
                var insert = Record_Unsafe.unsafeSet(key);
                var get = Record_Unsafe.unsafeGet(key);
                return insert(conj1(get(ra))(get(rb)))(tail);
              };
            };
          },
          disjRecord: function disjRecord(v) {
            return function (ra) {
              return function (rb) {
                var tail = disjRecord1(Type_Proxy["Proxy"].value)(ra)(rb);
                var key = reflectSymbol(Type_Proxy["Proxy"].value);
                var insert = Record_Unsafe.unsafeSet(key);
                var get = Record_Unsafe.unsafeGet(key);
                return insert(disj1(get(ra))(get(rb)))(tail);
              };
            };
          },
          impliesRecord: function impliesRecord(v) {
            return function (ra) {
              return function (rb) {
                var tail = impliesRecord1(Type_Proxy["Proxy"].value)(ra)(rb);
                var key = reflectSymbol(Type_Proxy["Proxy"].value);
                var insert = Record_Unsafe.unsafeSet(key);
                var get = Record_Unsafe.unsafeGet(key);
                return insert(implies1(get(ra))(get(rb)))(tail);
              };
            };
          },
          ffRecord: function ffRecord(v) {
            return function (row) {
              var tail = ffRecord1(Type_Proxy["Proxy"].value)(row);
              var key = reflectSymbol(Type_Proxy["Proxy"].value);
              var insert = Record_Unsafe.unsafeSet(key);
              return insert(ff1)(tail);
            };
          },
          notRecord: function notRecord(v) {
            return function (row) {
              var tail = notRecord1(Type_Proxy["Proxy"].value)(row);
              var key = reflectSymbol(Type_Proxy["Proxy"].value);
              var insert = Record_Unsafe.unsafeSet(key);
              var get = Record_Unsafe.unsafeGet(key);
              return insert(not1(get(row)))(tail);
            };
          },
          ttRecord: function ttRecord(v) {
            return function (row) {
              var tail = ttRecord1(Type_Proxy["Proxy"].value)(row);
              var key = reflectSymbol(Type_Proxy["Proxy"].value);
              var insert = Record_Unsafe.unsafeSet(key);
              return insert(tt1)(tail);
            };
          }
        };
      };
    };
  };
};

exports.heytingAlgebraRecordCons = heytingAlgebraRecordCons;
},{"./foreign.js":"../output/Data.HeytingAlgebra/foreign.js","../Data.Symbol/index.js":"../output/Data.Symbol/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Record.Unsafe/index.js":"../output/Record.Unsafe/index.js","../Type.Proxy/index.js":"../output/Type.Proxy/index.js"}],"../output/Data.Tuple/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.uncurry = exports.swap = exports.snd = exports.showTuple = exports.semiringTuple = exports.semigroupoidTuple = exports.semigroupTuple = exports.ringTuple = exports.ordTuple = exports.ord1Tuple = exports.monoidTuple = exports.monadTuple = exports.lazyTuple = exports.invariantTuple = exports.heytingAlgebraTuple = exports.genericTuple = exports.functorTuple = exports.fst = exports.extendTuple = exports.eqTuple = exports.eq1Tuple = exports.curry = exports.comonadTuple = exports.commutativeRingTuple = exports.boundedTuple = exports.booleanAlgebraTuple = exports.bindTuple = exports.applyTuple = exports.applicativeTuple = exports.Tuple = void 0;

var Control_Lazy = _interopRequireWildcard(require("../Control.Lazy/index.js"));

var Data_Bounded = _interopRequireWildcard(require("../Data.Bounded/index.js"));

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_Functor_Invariant = _interopRequireWildcard(require("../Data.Functor.Invariant/index.js"));

var Data_Generic_Rep = _interopRequireWildcard(require("../Data.Generic.Rep/index.js"));

var Data_HeytingAlgebra = _interopRequireWildcard(require("../Data.HeytingAlgebra/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Ordering = _interopRequireWildcard(require("../Data.Ordering/index.js"));

var Data_Ring = _interopRequireWildcard(require("../Data.Ring/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Semiring = _interopRequireWildcard(require("../Data.Semiring/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var Tuple =
/* #__PURE__ */
function () {
  function Tuple(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }

  ;

  Tuple.create = function (value0) {
    return function (value1) {
      return new Tuple(value0, value1);
    };
  };

  return Tuple;
}();

exports.Tuple = Tuple;

var uncurry = function uncurry(f) {
  return function (v) {
    return f(v.value0)(v.value1);
  };
};

exports.uncurry = uncurry;

var swap = function swap(v) {
  return new Tuple(v.value1, v.value0);
};

exports.swap = swap;

var snd = function snd(v) {
  return v.value1;
};

exports.snd = snd;

var showTuple = function showTuple(dictShow) {
  var _show = Data_Show.show(dictShow);

  return function (dictShow1) {
    var show1 = Data_Show.show(dictShow1);
    return {
      show: function show(v) {
        return "(Tuple " + (_show(v.value0) + (" " + (show1(v.value1) + ")")));
      }
    };
  };
};

exports.showTuple = showTuple;

var semiringTuple = function semiringTuple(dictSemiring) {
  var _add = Data_Semiring.add(dictSemiring);

  var one = Data_Semiring.one(dictSemiring);

  var _mul = Data_Semiring.mul(dictSemiring);

  var zero = Data_Semiring.zero(dictSemiring);
  return function (dictSemiring1) {
    var add1 = Data_Semiring.add(dictSemiring1);
    var mul1 = Data_Semiring.mul(dictSemiring1);
    return {
      add: function add(v) {
        return function (v1) {
          return new Tuple(_add(v.value0)(v1.value0), add1(v.value1)(v1.value1));
        };
      },
      one: new Tuple(one, Data_Semiring.one(dictSemiring1)),
      mul: function mul(v) {
        return function (v1) {
          return new Tuple(_mul(v.value0)(v1.value0), mul1(v.value1)(v1.value1));
        };
      },
      zero: new Tuple(zero, Data_Semiring.zero(dictSemiring1))
    };
  };
};

exports.semiringTuple = semiringTuple;
var semigroupoidTuple = {
  compose: function compose(v) {
    return function (v1) {
      return new Tuple(v1.value0, v.value1);
    };
  }
};
exports.semigroupoidTuple = semigroupoidTuple;

var semigroupTuple = function semigroupTuple(dictSemigroup) {
  var append1 = Data_Semigroup.append(dictSemigroup);
  return function (dictSemigroup1) {
    var append2 = Data_Semigroup.append(dictSemigroup1);
    return {
      append: function append(v) {
        return function (v1) {
          return new Tuple(append1(v.value0)(v1.value0), append2(v.value1)(v1.value1));
        };
      }
    };
  };
};

exports.semigroupTuple = semigroupTuple;

var ringTuple = function ringTuple(dictRing) {
  var _sub = Data_Ring.sub(dictRing);

  var semiringTuple1 = semiringTuple(dictRing.Semiring0());
  return function (dictRing1) {
    var sub1 = Data_Ring.sub(dictRing1);
    var semiringTuple2 = semiringTuple1(dictRing1.Semiring0());
    return {
      sub: function sub(v) {
        return function (v1) {
          return new Tuple(_sub(v.value0)(v1.value0), sub1(v.value1)(v1.value1));
        };
      },
      Semiring0: function Semiring0() {
        return semiringTuple2;
      }
    };
  };
};

exports.ringTuple = ringTuple;

var monoidTuple = function monoidTuple(dictMonoid) {
  var mempty = Data_Monoid.mempty(dictMonoid);
  var semigroupTuple1 = semigroupTuple(dictMonoid.Semigroup0());
  return function (dictMonoid1) {
    var semigroupTuple2 = semigroupTuple1(dictMonoid1.Semigroup0());
    return {
      mempty: new Tuple(mempty, Data_Monoid.mempty(dictMonoid1)),
      Semigroup0: function Semigroup0() {
        return semigroupTuple2;
      }
    };
  };
};

exports.monoidTuple = monoidTuple;

var heytingAlgebraTuple = function heytingAlgebraTuple(dictHeytingAlgebra) {
  var tt = Data_HeytingAlgebra.tt(dictHeytingAlgebra);
  var ff = Data_HeytingAlgebra.ff(dictHeytingAlgebra);

  var _implies = Data_HeytingAlgebra.implies(dictHeytingAlgebra);

  var conj1 = Data_HeytingAlgebra.conj(dictHeytingAlgebra);

  var _disj = Data_HeytingAlgebra.disj(dictHeytingAlgebra);

  var _not = Data_HeytingAlgebra.not(dictHeytingAlgebra);

  return function (dictHeytingAlgebra1) {
    var implies1 = Data_HeytingAlgebra.implies(dictHeytingAlgebra1);
    var conj2 = Data_HeytingAlgebra.conj(dictHeytingAlgebra1);
    var disj1 = Data_HeytingAlgebra.disj(dictHeytingAlgebra1);
    var not1 = Data_HeytingAlgebra.not(dictHeytingAlgebra1);
    return {
      tt: new Tuple(tt, Data_HeytingAlgebra.tt(dictHeytingAlgebra1)),
      ff: new Tuple(ff, Data_HeytingAlgebra.ff(dictHeytingAlgebra1)),
      implies: function implies(v) {
        return function (v1) {
          return new Tuple(_implies(v.value0)(v1.value0), implies1(v.value1)(v1.value1));
        };
      },
      conj: function conj(v) {
        return function (v1) {
          return new Tuple(conj1(v.value0)(v1.value0), conj2(v.value1)(v1.value1));
        };
      },
      disj: function disj(v) {
        return function (v1) {
          return new Tuple(_disj(v.value0)(v1.value0), disj1(v.value1)(v1.value1));
        };
      },
      not: function not(v) {
        return new Tuple(_not(v.value0), not1(v.value1));
      }
    };
  };
};

exports.heytingAlgebraTuple = heytingAlgebraTuple;
var genericTuple = {
  to: function to(x) {
    return new Tuple(x.value0, x.value1);
  },
  from: function from(x) {
    return new Data_Generic_Rep.Product(x.value0, x.value1);
  }
};
exports.genericTuple = genericTuple;
var functorTuple = {
  map: function map(f) {
    return function (m) {
      return new Tuple(m.value0, f(m.value1));
    };
  }
};
exports.functorTuple = functorTuple;
var invariantTuple = {
  imap:
  /* #__PURE__ */
  Data_Functor_Invariant.imapF(functorTuple)
};
exports.invariantTuple = invariantTuple;

var fst = function fst(v) {
  return v.value0;
};

exports.fst = fst;

var lazyTuple = function lazyTuple(dictLazy) {
  var _defer = Control_Lazy.defer(dictLazy);

  return function (dictLazy1) {
    var defer1 = Control_Lazy.defer(dictLazy1);
    return {
      defer: function defer(f) {
        return new Tuple(_defer(function (v) {
          return fst(f(Data_Unit.unit));
        }), defer1(function (v) {
          return snd(f(Data_Unit.unit));
        }));
      }
    };
  };
};

exports.lazyTuple = lazyTuple;
var extendTuple = {
  extend: function extend(f) {
    return function (v) {
      return new Tuple(v.value0, f(v));
    };
  },
  Functor0: function Functor0() {
    return functorTuple;
  }
};
exports.extendTuple = extendTuple;

var eqTuple = function eqTuple(dictEq) {
  var _eq = Data_Eq.eq(dictEq);

  return function (dictEq1) {
    var eq1 = Data_Eq.eq(dictEq1);
    return {
      eq: function eq(x) {
        return function (y) {
          return _eq(x.value0)(y.value0) && eq1(x.value1)(y.value1);
        };
      }
    };
  };
};

exports.eqTuple = eqTuple;

var ordTuple = function ordTuple(dictOrd) {
  var _compare = Data_Ord.compare(dictOrd);

  var eqTuple1 = eqTuple(dictOrd.Eq0());
  return function (dictOrd1) {
    var compare1 = Data_Ord.compare(dictOrd1);
    var eqTuple2 = eqTuple1(dictOrd1.Eq0());
    return {
      compare: function compare(x) {
        return function (y) {
          var v = _compare(x.value0)(y.value0);

          if (v instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
          }

          ;

          if (v instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
          }

          ;
          return compare1(x.value1)(y.value1);
        };
      },
      Eq0: function Eq0() {
        return eqTuple2;
      }
    };
  };
};

exports.ordTuple = ordTuple;

var eq1Tuple = function eq1Tuple(dictEq) {
  var eqTuple1 = eqTuple(dictEq);
  return {
    eq1: function eq1(dictEq1) {
      return Data_Eq.eq(eqTuple1(dictEq1));
    }
  };
};

exports.eq1Tuple = eq1Tuple;

var ord1Tuple = function ord1Tuple(dictOrd) {
  var ordTuple1 = ordTuple(dictOrd);
  var eq1Tuple1 = eq1Tuple(dictOrd.Eq0());
  return {
    compare1: function compare1(dictOrd1) {
      return Data_Ord.compare(ordTuple1(dictOrd1));
    },
    Eq10: function Eq10() {
      return eq1Tuple1;
    }
  };
};

exports.ord1Tuple = ord1Tuple;

var curry = function curry(f) {
  return function (a) {
    return function (b) {
      return f(new Tuple(a, b));
    };
  };
};

exports.curry = curry;
var comonadTuple = {
  extract: snd,
  Extend0: function Extend0() {
    return extendTuple;
  }
};
exports.comonadTuple = comonadTuple;

var commutativeRingTuple = function commutativeRingTuple(dictCommutativeRing) {
  var ringTuple1 = ringTuple(dictCommutativeRing.Ring0());
  return function (dictCommutativeRing1) {
    var ringTuple2 = ringTuple1(dictCommutativeRing1.Ring0());
    return {
      Ring0: function Ring0() {
        return ringTuple2;
      }
    };
  };
};

exports.commutativeRingTuple = commutativeRingTuple;

var boundedTuple = function boundedTuple(dictBounded) {
  var top = Data_Bounded.top(dictBounded);
  var bottom = Data_Bounded.bottom(dictBounded);
  var ordTuple1 = ordTuple(dictBounded.Ord0());
  return function (dictBounded1) {
    var ordTuple2 = ordTuple1(dictBounded1.Ord0());
    return {
      top: new Tuple(top, Data_Bounded.top(dictBounded1)),
      bottom: new Tuple(bottom, Data_Bounded.bottom(dictBounded1)),
      Ord0: function Ord0() {
        return ordTuple2;
      }
    };
  };
};

exports.boundedTuple = boundedTuple;

var booleanAlgebraTuple = function booleanAlgebraTuple(dictBooleanAlgebra) {
  var heytingAlgebraTuple1 = heytingAlgebraTuple(dictBooleanAlgebra.HeytingAlgebra0());
  return function (dictBooleanAlgebra1) {
    var heytingAlgebraTuple2 = heytingAlgebraTuple1(dictBooleanAlgebra1.HeytingAlgebra0());
    return {
      HeytingAlgebra0: function HeytingAlgebra0() {
        return heytingAlgebraTuple2;
      }
    };
  };
};

exports.booleanAlgebraTuple = booleanAlgebraTuple;

var applyTuple = function applyTuple(dictSemigroup) {
  var append1 = Data_Semigroup.append(dictSemigroup);
  return {
    apply: function apply(v) {
      return function (v1) {
        return new Tuple(append1(v.value0)(v1.value0), v.value1(v1.value1));
      };
    },
    Functor0: function Functor0() {
      return functorTuple;
    }
  };
};

exports.applyTuple = applyTuple;

var bindTuple = function bindTuple(dictSemigroup) {
  var append1 = Data_Semigroup.append(dictSemigroup);
  var applyTuple1 = applyTuple(dictSemigroup);
  return {
    bind: function bind(v) {
      return function (f) {
        var v1 = f(v.value1);
        return new Tuple(append1(v.value0)(v1.value0), v1.value1);
      };
    },
    Apply0: function Apply0() {
      return applyTuple1;
    }
  };
};

exports.bindTuple = bindTuple;

var applicativeTuple = function applicativeTuple(dictMonoid) {
  var applyTuple1 = applyTuple(dictMonoid.Semigroup0());
  return {
    pure: Tuple.create(Data_Monoid.mempty(dictMonoid)),
    Apply0: function Apply0() {
      return applyTuple1;
    }
  };
};

exports.applicativeTuple = applicativeTuple;

var monadTuple = function monadTuple(dictMonoid) {
  var applicativeTuple1 = applicativeTuple(dictMonoid);
  var bindTuple1 = bindTuple(dictMonoid.Semigroup0());
  return {
    Applicative0: function Applicative0() {
      return applicativeTuple1;
    },
    Bind1: function Bind1() {
      return bindTuple1;
    }
  };
};

exports.monadTuple = monadTuple;
},{"../Control.Lazy/index.js":"../output/Control.Lazy/index.js","../Data.Bounded/index.js":"../output/Data.Bounded/index.js","../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.Functor.Invariant/index.js":"../output/Data.Functor.Invariant/index.js","../Data.Generic.Rep/index.js":"../output/Data.Generic.Rep/index.js","../Data.HeytingAlgebra/index.js":"../output/Data.HeytingAlgebra/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Ordering/index.js":"../output/Data.Ordering/index.js","../Data.Ring/index.js":"../output/Data.Ring/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Semiring/index.js":"../output/Data.Semiring/index.js","../Data.Show/index.js":"../output/Data.Show/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js"}],"../output/Control.Monad.State.Class/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.state = exports.put = exports.modify_ = exports.modify = exports.gets = exports.get = void 0;

var Data_Tuple = _interopRequireWildcard(require("../Data.Tuple/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var state = function state(dict) {
  return dict.state;
};

exports.state = state;

var put = function put(dictMonadState) {
  var state1 = state(dictMonadState);
  return function (s) {
    return state1(function (v) {
      return new Data_Tuple.Tuple(Data_Unit.unit, s);
    });
  };
};

exports.put = put;

var modify_ = function modify_(dictMonadState) {
  var state1 = state(dictMonadState);
  return function (f) {
    return state1(function (s) {
      return new Data_Tuple.Tuple(Data_Unit.unit, f(s));
    });
  };
};

exports.modify_ = modify_;

var modify = function modify(dictMonadState) {
  var state1 = state(dictMonadState);
  return function (f) {
    return state1(function (s) {
      var s$prime = f(s);
      return new Data_Tuple.Tuple(s$prime, s$prime);
    });
  };
};

exports.modify = modify;

var gets = function gets(dictMonadState) {
  var state1 = state(dictMonadState);
  return function (f) {
    return state1(function (s) {
      return new Data_Tuple.Tuple(f(s), s);
    });
  };
};

exports.gets = gets;

var get = function get(dictMonadState) {
  return state(dictMonadState)(function (s) {
    return new Data_Tuple.Tuple(s, s);
  });
};

exports.get = get;
},{"../Data.Tuple/index.js":"../output/Data.Tuple/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js"}],"../output/Control.Monad.Trans.Class/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.lift = void 0;

// Generated by purs version 0.15.10
var lift = function lift(dict) {
  return dict.lift;
};

exports.lift = lift;
},{}],"../output/Effect.Class/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.monadEffectEffect = exports.liftEffect = void 0;

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Effect = _interopRequireWildcard(require("../Effect/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var monadEffectEffect = {
  liftEffect:
  /* #__PURE__ */
  Control_Category.identity(Control_Category.categoryFn),
  Monad0: function Monad0() {
    return Effect.monadEffect;
  }
};
exports.monadEffectEffect = monadEffectEffect;

var liftEffect = function liftEffect(dict) {
  return dict.liftEffect;
};

exports.liftEffect = liftEffect;
},{"../Control.Category/index.js":"../output/Control.Category/index.js","../Effect/index.js":"../output/Effect/index.js"}],"../output/Control.Monad.Cont.Trans/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.bindContT = exports.applyContT = exports.applicativeContT = exports.ContT = void 0;
Object.defineProperty(exports, "callCC", {
  enumerable: true,
  get: function () {
    return Control_Monad_Cont_Class.callCC;
  }
});
exports.functorContT = void 0;
Object.defineProperty(exports, "lift", {
  enumerable: true,
  get: function () {
    return Control_Monad_Trans_Class.lift;
  }
});
exports.withContT = exports.semigroupContT = exports.runContT = exports.newtypeContT = exports.monoidContT = exports.monadTransContT = exports.monadStateContT = exports.monadReaderContT = exports.monadEffectContT = exports.monadContT = exports.monadContContT = exports.monadAskContT = exports.mapContT = void 0;

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Monad_Cont_Class = _interopRequireWildcard(require("../Control.Monad.Cont.Class/index.js"));

var Control_Monad_Reader_Class = _interopRequireWildcard(require("../Control.Monad.Reader.Class/index.js"));

var Control_Monad_State_Class = _interopRequireWildcard(require("../Control.Monad.State.Class/index.js"));

var Control_Monad_Trans_Class = _interopRequireWildcard(require("../Control.Monad.Trans.Class/index.js"));

var Data_Function = _interopRequireWildcard(require("../Data.Function/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Effect_Class = _interopRequireWildcard(require("../Effect.Class/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var ContT = function ContT(x) {
  return x;
};

exports.ContT = ContT;

var withContT = function withContT(f) {
  return function (v) {
    return function (k) {
      return v(f(k));
    };
  };
};

exports.withContT = withContT;

var runContT = function runContT(v) {
  return function (k) {
    return v(k);
  };
};

exports.runContT = runContT;
var newtypeContT = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeContT = newtypeContT;
var monadTransContT = {
  lift: function lift(dictMonad) {
    var bind = Control_Bind.bind(dictMonad.Bind1());
    return function (m) {
      return function (k) {
        return bind(m)(k);
      };
    };
  }
};
exports.monadTransContT = monadTransContT;
var lift =
/* #__PURE__ */
Control_Monad_Trans_Class.lift(monadTransContT);

var mapContT = function mapContT(f) {
  return function (v) {
    return function (k) {
      return f(v(k));
    };
  };
};

exports.mapContT = mapContT;

var functorContT = function functorContT(dictFunctor) {
  return {
    map: function map(f) {
      return function (v) {
        return function (k) {
          return v(function (a) {
            return k(f(a));
          });
        };
      };
    }
  };
};

exports.functorContT = functorContT;

var applyContT = function applyContT(dictApply) {
  var functorContT1 = functorContT(dictApply.Functor0());
  return {
    apply: function apply(v) {
      return function (v1) {
        return function (k) {
          return v(function (g) {
            return v1(function (a) {
              return k(g(a));
            });
          });
        };
      };
    },
    Functor0: function Functor0() {
      return functorContT1;
    }
  };
};

exports.applyContT = applyContT;

var bindContT = function bindContT(dictBind) {
  var applyContT1 = applyContT(dictBind.Apply0());
  return {
    bind: function bind(v) {
      return function (k) {
        return function (k$prime) {
          return v(function (a) {
            var v1 = k(a);
            return v1(k$prime);
          });
        };
      };
    },
    Apply0: function Apply0() {
      return applyContT1;
    }
  };
};

exports.bindContT = bindContT;

var semigroupContT = function semigroupContT(dictApply) {
  var lift2 = Control_Apply.lift2(applyContT(dictApply));
  return function (dictSemigroup) {
    return {
      append: lift2(Data_Semigroup.append(dictSemigroup))
    };
  };
};

exports.semigroupContT = semigroupContT;

var applicativeContT = function applicativeContT(dictApplicative) {
  var applyContT1 = applyContT(dictApplicative.Apply0());
  return {
    pure: function pure(a) {
      return function (k) {
        return k(a);
      };
    },
    Apply0: function Apply0() {
      return applyContT1;
    }
  };
};

exports.applicativeContT = applicativeContT;

var monadContT = function monadContT(dictMonad) {
  var applicativeContT1 = applicativeContT(dictMonad.Applicative0());
  var bindContT1 = bindContT(dictMonad.Bind1());
  return {
    Applicative0: function Applicative0() {
      return applicativeContT1;
    },
    Bind1: function Bind1() {
      return bindContT1;
    }
  };
};

exports.monadContT = monadContT;

var monadAskContT = function monadAskContT(dictMonadAsk) {
  var Monad0 = dictMonadAsk.Monad0();
  var monadContT1 = monadContT(Monad0);
  return {
    ask: lift(Monad0)(Control_Monad_Reader_Class.ask(dictMonadAsk)),
    Monad0: function Monad0() {
      return monadContT1;
    }
  };
};

exports.monadAskContT = monadAskContT;

var monadReaderContT = function monadReaderContT(dictMonadReader) {
  var MonadAsk0 = dictMonadReader.MonadAsk0();
  var bind = Control_Bind.bind(MonadAsk0.Monad0().Bind1());
  var ask = Control_Monad_Reader_Class.ask(MonadAsk0);

  var _local = Control_Monad_Reader_Class.local(dictMonadReader);

  var monadAskContT1 = monadAskContT(MonadAsk0);
  return {
    local: function local(f) {
      return function (v) {
        return function (k) {
          return bind(ask)(function (r) {
            return _local(f)(v(function () {
              var $88 = _local(Data_Function["const"](r));

              return function ($89) {
                return $88(k($89));
              };
            }()));
          });
        };
      };
    },
    MonadAsk0: function MonadAsk0() {
      return monadAskContT1;
    }
  };
};

exports.monadReaderContT = monadReaderContT;

var monadContContT = function monadContContT(dictMonad) {
  var monadContT1 = monadContT(dictMonad);
  return {
    callCC: function callCC(f) {
      return function (k) {
        var v = f(function (a) {
          return function (v1) {
            return k(a);
          };
        });
        return v(k);
      };
    },
    Monad0: function Monad0() {
      return monadContT1;
    }
  };
};

exports.monadContContT = monadContContT;

var monadEffectContT = function monadEffectContT(dictMonadEffect) {
  var Monad0 = dictMonadEffect.Monad0();
  var monadContT1 = monadContT(Monad0);
  return {
    liftEffect: function () {
      var $90 = lift(Monad0);
      var $91 = Effect_Class.liftEffect(dictMonadEffect);
      return function ($92) {
        return $90($91($92));
      };
    }(),
    Monad0: function Monad0() {
      return monadContT1;
    }
  };
};

exports.monadEffectContT = monadEffectContT;

var monadStateContT = function monadStateContT(dictMonadState) {
  var Monad0 = dictMonadState.Monad0();
  var monadContT1 = monadContT(Monad0);
  return {
    state: function () {
      var $93 = lift(Monad0);
      var $94 = Control_Monad_State_Class.state(dictMonadState);
      return function ($95) {
        return $93($94($95));
      };
    }(),
    Monad0: function Monad0() {
      return monadContT1;
    }
  };
};

exports.monadStateContT = monadStateContT;

var monoidContT = function monoidContT(dictApplicative) {
  var pure = Control_Applicative.pure(applicativeContT(dictApplicative));
  var semigroupContT1 = semigroupContT(dictApplicative.Apply0());
  return function (dictMonoid) {
    var semigroupContT2 = semigroupContT1(dictMonoid.Semigroup0());
    return {
      mempty: pure(Data_Monoid.mempty(dictMonoid)),
      Semigroup0: function Semigroup0() {
        return semigroupContT2;
      }
    };
  };
};

exports.monoidContT = monoidContT;
},{"../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Monad.Cont.Class/index.js":"../output/Control.Monad.Cont.Class/index.js","../Control.Monad.Reader.Class/index.js":"../output/Control.Monad.Reader.Class/index.js","../Control.Monad.State.Class/index.js":"../output/Control.Monad.State.Class/index.js","../Control.Monad.Trans.Class/index.js":"../output/Control.Monad.Trans.Class/index.js","../Data.Function/index.js":"../output/Data.Function/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Effect.Class/index.js":"../output/Effect.Class/index.js"}],"../output/Control.Monad.Writer.Class/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.tell = exports.pass = exports.listens = exports.listen = exports.censor = void 0;

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Data_Tuple = _interopRequireWildcard(require("../Data.Tuple/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var tell = function tell(dict) {
  return dict.tell;
};

exports.tell = tell;

var pass = function pass(dict) {
  return dict.pass;
};

exports.pass = pass;

var listen = function listen(dict) {
  return dict.listen;
};

exports.listen = listen;

var listens = function listens(dictMonadWriter) {
  var Monad1 = dictMonadWriter.MonadTell1().Monad1();
  var bind = Control_Bind.bind(Monad1.Bind1());
  var listen1 = listen(dictMonadWriter);
  var pure = Control_Applicative.pure(Monad1.Applicative0());
  return function (f) {
    return function (m) {
      return bind(listen1(m))(function (v) {
        return pure(new Data_Tuple.Tuple(v.value0, f(v.value1)));
      });
    };
  };
};

exports.listens = listens;

var censor = function censor(dictMonadWriter) {
  var pass1 = pass(dictMonadWriter);
  var Monad1 = dictMonadWriter.MonadTell1().Monad1();
  var bind = Control_Bind.bind(Monad1.Bind1());
  var pure = Control_Applicative.pure(Monad1.Applicative0());
  return function (f) {
    return function (m) {
      return pass1(bind(m)(function (a) {
        return pure(new Data_Tuple.Tuple(a, f));
      }));
    };
  };
};

exports.censor = censor;
},{"../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Data.Tuple/index.js":"../output/Data.Tuple/index.js"}],"../output/Control.Monad.Except.Trans/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.bindExceptT = exports.applyExceptT = exports.applicativeExceptT = exports.alternativeExceptT = exports.altExceptT = exports.ExceptT = void 0;
Object.defineProperty(exports, "catchError", {
  enumerable: true,
  get: function () {
    return Control_Monad_Error_Class.catchError;
  }
});
exports.functorExceptT = exports.except = void 0;
Object.defineProperty(exports, "lift", {
  enumerable: true,
  get: function () {
    return Control_Monad_Trans_Class.lift;
  }
});
exports.semigroupExceptT = exports.runExceptT = exports.plusExceptT = exports.newtypeExceptT = exports.monoidExceptT = exports.monadWriterExceptT = exports.monadTransExceptT = exports.monadThrowExceptT = exports.monadTellExceptT = exports.monadStateExceptT = exports.monadRecExceptT = exports.monadReaderExceptT = exports.monadPlusExceptT = exports.monadExceptT = exports.monadErrorExceptT = exports.monadEffectExceptT = exports.monadContExceptT = exports.monadAskExceptT = exports.mapExceptT = void 0;
Object.defineProperty(exports, "throwError", {
  enumerable: true,
  get: function () {
    return Control_Monad_Error_Class.throwError;
  }
});
exports.withExceptT = void 0;

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Control_Monad = _interopRequireWildcard(require("../Control.Monad/index.js"));

var Control_Monad_Cont_Class = _interopRequireWildcard(require("../Control.Monad.Cont.Class/index.js"));

var Control_Monad_Error_Class = _interopRequireWildcard(require("../Control.Monad.Error.Class/index.js"));

var Control_Monad_Reader_Class = _interopRequireWildcard(require("../Control.Monad.Reader.Class/index.js"));

var Control_Monad_Rec_Class = _interopRequireWildcard(require("../Control.Monad.Rec.Class/index.js"));

var Control_Monad_State_Class = _interopRequireWildcard(require("../Control.Monad.State.Class/index.js"));

var Control_Monad_Trans_Class = _interopRequireWildcard(require("../Control.Monad.Trans.Class/index.js"));

var Control_Monad_Writer_Class = _interopRequireWildcard(require("../Control.Monad.Writer.Class/index.js"));

var Data_Either = _interopRequireWildcard(require("../Data.Either/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Tuple = _interopRequireWildcard(require("../Data.Tuple/index.js"));

var Effect_Class = _interopRequireWildcard(require("../Effect.Class/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var _map =
/* #__PURE__ */
Data_Functor.map(Data_Either.functorEither);

var identity =
/* #__PURE__ */
Control_Category.identity(Control_Category.categoryFn);

var ExceptT = function ExceptT(x) {
  return x;
};

exports.ExceptT = ExceptT;

var withExceptT = function withExceptT(dictFunctor) {
  var map1 = Data_Functor.map(dictFunctor);
  return function (f) {
    return function (v) {
      var mapLeft = function mapLeft(v1) {
        return function (v2) {
          if (v2 instanceof Data_Either.Right) {
            return new Data_Either.Right(v2.value0);
          }

          ;

          if (v2 instanceof Data_Either.Left) {
            return new Data_Either.Left(v1(v2.value0));
          }

          ;
          throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 42, column 3 - line 42, column 32): " + [v1.constructor.name, v2.constructor.name]);
        };
      };

      return map1(mapLeft(f))(v);
    };
  };
};

exports.withExceptT = withExceptT;

var runExceptT = function runExceptT(v) {
  return v;
};

exports.runExceptT = runExceptT;
var newtypeExceptT = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeExceptT = newtypeExceptT;
var monadTransExceptT = {
  lift: function lift(dictMonad) {
    var bind = Control_Bind.bind(dictMonad.Bind1());
    var pure = Control_Applicative.pure(dictMonad.Applicative0());
    return function (m) {
      return bind(m)(function (a) {
        return pure(new Data_Either.Right(a));
      });
    };
  }
};
exports.monadTransExceptT = monadTransExceptT;
var lift =
/* #__PURE__ */
Control_Monad_Trans_Class.lift(monadTransExceptT);

var mapExceptT = function mapExceptT(f) {
  return function (v) {
    return f(v);
  };
};

exports.mapExceptT = mapExceptT;

var functorExceptT = function functorExceptT(dictFunctor) {
  var map1 = Data_Functor.map(dictFunctor);
  return {
    map: function map(f) {
      return mapExceptT(map1(_map(f)));
    }
  };
};

exports.functorExceptT = functorExceptT;

var except = function except(dictApplicative) {
  var $185 = Control_Applicative.pure(dictApplicative);
  return function ($186) {
    return ExceptT($185($186));
  };
};

exports.except = except;

var monadExceptT = function monadExceptT(dictMonad) {
  return {
    Applicative0: function Applicative0() {
      return applicativeExceptT(dictMonad);
    },
    Bind1: function Bind1() {
      return bindExceptT(dictMonad);
    }
  };
};

exports.monadExceptT = monadExceptT;

var bindExceptT = function bindExceptT(dictMonad) {
  var _bind = Control_Bind.bind(dictMonad.Bind1());

  var pure = Control_Applicative.pure(dictMonad.Applicative0());
  return {
    bind: function bind(v) {
      return function (k) {
        return _bind(v)(Data_Either.either(function ($187) {
          return pure(Data_Either.Left.create($187));
        })(function (a) {
          var v1 = k(a);
          return v1;
        }));
      };
    },
    Apply0: function Apply0() {
      return applyExceptT(dictMonad);
    }
  };
};

exports.bindExceptT = bindExceptT;

var applyExceptT = function applyExceptT(dictMonad) {
  var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
  return {
    apply: Control_Monad.ap(monadExceptT(dictMonad)),
    Functor0: function Functor0() {
      return functorExceptT1;
    }
  };
};

exports.applyExceptT = applyExceptT;

var applicativeExceptT = function applicativeExceptT(dictMonad) {
  return {
    pure: function () {
      var $188 = Control_Applicative.pure(dictMonad.Applicative0());
      return function ($189) {
        return ExceptT($188(Data_Either.Right.create($189)));
      };
    }(),
    Apply0: function Apply0() {
      return applyExceptT(dictMonad);
    }
  };
};

exports.applicativeExceptT = applicativeExceptT;

var semigroupExceptT = function semigroupExceptT(dictMonad) {
  var lift2 = Control_Apply.lift2(applyExceptT(dictMonad));
  return function (dictSemigroup) {
    return {
      append: lift2(Data_Semigroup.append(dictSemigroup))
    };
  };
};

exports.semigroupExceptT = semigroupExceptT;

var monadAskExceptT = function monadAskExceptT(dictMonadAsk) {
  var Monad0 = dictMonadAsk.Monad0();
  var monadExceptT1 = monadExceptT(Monad0);
  return {
    ask: lift(Monad0)(Control_Monad_Reader_Class.ask(dictMonadAsk)),
    Monad0: function Monad0() {
      return monadExceptT1;
    }
  };
};

exports.monadAskExceptT = monadAskExceptT;

var monadReaderExceptT = function monadReaderExceptT(dictMonadReader) {
  var _local = Control_Monad_Reader_Class.local(dictMonadReader);

  var monadAskExceptT1 = monadAskExceptT(dictMonadReader.MonadAsk0());
  return {
    local: function local(f) {
      return mapExceptT(_local(f));
    },
    MonadAsk0: function MonadAsk0() {
      return monadAskExceptT1;
    }
  };
};

exports.monadReaderExceptT = monadReaderExceptT;

var monadContExceptT = function monadContExceptT(dictMonadCont) {
  var _callCC = Control_Monad_Cont_Class.callCC(dictMonadCont);

  var monadExceptT1 = monadExceptT(dictMonadCont.Monad0());
  return {
    callCC: function callCC(f) {
      return _callCC(function (c) {
        var v = f(function (a) {
          return c(new Data_Either.Right(a));
        });
        return v;
      });
    },
    Monad0: function Monad0() {
      return monadExceptT1;
    }
  };
};

exports.monadContExceptT = monadContExceptT;

var monadEffectExceptT = function monadEffectExceptT(dictMonadEffect) {
  var Monad0 = dictMonadEffect.Monad0();
  var monadExceptT1 = monadExceptT(Monad0);
  return {
    liftEffect: function () {
      var $190 = lift(Monad0);
      var $191 = Effect_Class.liftEffect(dictMonadEffect);
      return function ($192) {
        return $190($191($192));
      };
    }(),
    Monad0: function Monad0() {
      return monadExceptT1;
    }
  };
};

exports.monadEffectExceptT = monadEffectExceptT;

var monadRecExceptT = function monadRecExceptT(dictMonadRec) {
  var _tailRecM = Control_Monad_Rec_Class.tailRecM(dictMonadRec);

  var Monad0 = dictMonadRec.Monad0();
  var bind = Control_Bind.bind(Monad0.Bind1());
  var pure = Control_Applicative.pure(Monad0.Applicative0());
  var monadExceptT1 = monadExceptT(Monad0);
  return {
    tailRecM: function tailRecM(f) {
      var $193 = _tailRecM(function (a) {
        var v = f(a);
        return bind(v)(function (m$prime) {
          return pure(function () {
            if (m$prime instanceof Data_Either.Left) {
              return new Control_Monad_Rec_Class.Done(new Data_Either.Left(m$prime.value0));
            }

            ;

            if (m$prime instanceof Data_Either.Right && m$prime.value0 instanceof Control_Monad_Rec_Class.Loop) {
              return new Control_Monad_Rec_Class.Loop(m$prime.value0.value0);
            }

            ;

            if (m$prime instanceof Data_Either.Right && m$prime.value0 instanceof Control_Monad_Rec_Class.Done) {
              return new Control_Monad_Rec_Class.Done(new Data_Either.Right(m$prime.value0.value0));
            }

            ;
            throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 74, column 14 - line 77, column 43): " + [m$prime.constructor.name]);
          }());
        });
      });

      return function ($194) {
        return ExceptT($193($194));
      };
    },
    Monad0: function Monad0() {
      return monadExceptT1;
    }
  };
};

exports.monadRecExceptT = monadRecExceptT;

var monadStateExceptT = function monadStateExceptT(dictMonadState) {
  var Monad0 = dictMonadState.Monad0();
  var lift1 = lift(Monad0);

  var _state = Control_Monad_State_Class.state(dictMonadState);

  var monadExceptT1 = monadExceptT(Monad0);
  return {
    state: function state(f) {
      return lift1(_state(f));
    },
    Monad0: function Monad0() {
      return monadExceptT1;
    }
  };
};

exports.monadStateExceptT = monadStateExceptT;

var monadTellExceptT = function monadTellExceptT(dictMonadTell) {
  var Monad1 = dictMonadTell.Monad1();

  var _Semigroup = dictMonadTell.Semigroup0();

  var monadExceptT1 = monadExceptT(Monad1);
  return {
    tell: function () {
      var $195 = lift(Monad1);
      var $196 = Control_Monad_Writer_Class.tell(dictMonadTell);
      return function ($197) {
        return $195($196($197));
      };
    }(),
    Semigroup0: function Semigroup0() {
      return _Semigroup;
    },
    Monad1: function Monad1() {
      return monadExceptT1;
    }
  };
};

exports.monadTellExceptT = monadTellExceptT;

var monadWriterExceptT = function monadWriterExceptT(dictMonadWriter) {
  var MonadTell1 = dictMonadWriter.MonadTell1();
  var Monad1 = MonadTell1.Monad1();
  var bind = Control_Bind.bind(Monad1.Bind1());
  var listen = Control_Monad_Writer_Class.listen(dictMonadWriter);
  var pure = Control_Applicative.pure(Monad1.Applicative0());
  var pass = Control_Monad_Writer_Class.pass(dictMonadWriter);

  var _Monoid = dictMonadWriter.Monoid0();

  var monadTellExceptT1 = monadTellExceptT(MonadTell1);
  return {
    listen: mapExceptT(function (m) {
      return bind(listen(m))(function (v) {
        return pure(_map(function (r) {
          return new Data_Tuple.Tuple(r, v.value1);
        })(v.value0));
      });
    }),
    pass: mapExceptT(function (m) {
      return pass(bind(m)(function (a) {
        return pure(function () {
          if (a instanceof Data_Either.Left) {
            return new Data_Tuple.Tuple(new Data_Either.Left(a.value0), identity);
          }

          ;

          if (a instanceof Data_Either.Right) {
            return new Data_Tuple.Tuple(new Data_Either.Right(a.value0.value0), a.value0.value1);
          }

          ;
          throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 134, column 10 - line 136, column 45): " + [a.constructor.name]);
        }());
      }));
    }),
    Monoid0: function Monoid0() {
      return _Monoid;
    },
    MonadTell1: function MonadTell1() {
      return monadTellExceptT1;
    }
  };
};

exports.monadWriterExceptT = monadWriterExceptT;

var monadThrowExceptT = function monadThrowExceptT(dictMonad) {
  var monadExceptT1 = monadExceptT(dictMonad);
  return {
    throwError: function () {
      var $198 = Control_Applicative.pure(dictMonad.Applicative0());
      return function ($199) {
        return ExceptT($198(Data_Either.Left.create($199)));
      };
    }(),
    Monad0: function Monad0() {
      return monadExceptT1;
    }
  };
};

exports.monadThrowExceptT = monadThrowExceptT;

var monadErrorExceptT = function monadErrorExceptT(dictMonad) {
  var bind = Control_Bind.bind(dictMonad.Bind1());
  var pure = Control_Applicative.pure(dictMonad.Applicative0());
  var monadThrowExceptT1 = monadThrowExceptT(dictMonad);
  return {
    catchError: function catchError(v) {
      return function (k) {
        return bind(v)(Data_Either.either(function (a) {
          var v1 = k(a);
          return v1;
        })(function ($200) {
          return pure(Data_Either.Right.create($200));
        }));
      };
    },
    MonadThrow0: function MonadThrow0() {
      return monadThrowExceptT1;
    }
  };
};

exports.monadErrorExceptT = monadErrorExceptT;

var monoidExceptT = function monoidExceptT(dictMonad) {
  var pure = Control_Applicative.pure(applicativeExceptT(dictMonad));
  var semigroupExceptT1 = semigroupExceptT(dictMonad);
  return function (dictMonoid) {
    var semigroupExceptT2 = semigroupExceptT1(dictMonoid.Semigroup0());
    return {
      mempty: pure(Data_Monoid.mempty(dictMonoid)),
      Semigroup0: function Semigroup0() {
        return semigroupExceptT2;
      }
    };
  };
};

exports.monoidExceptT = monoidExceptT;

var altExceptT = function altExceptT(dictSemigroup) {
  var append = Data_Semigroup.append(dictSemigroup);
  return function (dictMonad) {
    var Bind1 = dictMonad.Bind1();
    var bind = Control_Bind.bind(Bind1);
    var pure = Control_Applicative.pure(dictMonad.Applicative0());
    var functorExceptT1 = functorExceptT(Bind1.Apply0().Functor0());
    return {
      alt: function alt(v) {
        return function (v1) {
          return bind(v)(function (rm) {
            if (rm instanceof Data_Either.Right) {
              return pure(new Data_Either.Right(rm.value0));
            }

            ;

            if (rm instanceof Data_Either.Left) {
              return bind(v1)(function (rn) {
                if (rn instanceof Data_Either.Right) {
                  return pure(new Data_Either.Right(rn.value0));
                }

                ;

                if (rn instanceof Data_Either.Left) {
                  return pure(new Data_Either.Left(append(rm.value0)(rn.value0)));
                }

                ;
                throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): " + [rn.constructor.name]);
              });
            }

            ;
            throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): " + [rm.constructor.name]);
          });
        };
      },
      Functor0: function Functor0() {
        return functorExceptT1;
      }
    };
  };
};

exports.altExceptT = altExceptT;

var plusExceptT = function plusExceptT(dictMonoid) {
  var mempty = Data_Monoid.mempty(dictMonoid);
  var altExceptT1 = altExceptT(dictMonoid.Semigroup0());
  return function (dictMonad) {
    var altExceptT2 = altExceptT1(dictMonad);
    return {
      empty: Control_Monad_Error_Class.throwError(monadThrowExceptT(dictMonad))(mempty),
      Alt0: function Alt0() {
        return altExceptT2;
      }
    };
  };
};

exports.plusExceptT = plusExceptT;

var alternativeExceptT = function alternativeExceptT(dictMonoid) {
  var plusExceptT1 = plusExceptT(dictMonoid);
  return function (dictMonad) {
    var applicativeExceptT1 = applicativeExceptT(dictMonad);
    var plusExceptT2 = plusExceptT1(dictMonad);
    return {
      Applicative0: function Applicative0() {
        return applicativeExceptT1;
      },
      Plus1: function Plus1() {
        return plusExceptT2;
      }
    };
  };
};

exports.alternativeExceptT = alternativeExceptT;

var monadPlusExceptT = function monadPlusExceptT(dictMonoid) {
  var alternativeExceptT1 = alternativeExceptT(dictMonoid);
  return function (dictMonad) {
    var monadExceptT1 = monadExceptT(dictMonad);
    var alternativeExceptT2 = alternativeExceptT1(dictMonad);
    return {
      Monad0: function Monad0() {
        return monadExceptT1;
      },
      Alternative1: function Alternative1() {
        return alternativeExceptT2;
      }
    };
  };
};

exports.monadPlusExceptT = monadPlusExceptT;
},{"../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Category/index.js":"../output/Control.Category/index.js","../Control.Monad/index.js":"../output/Control.Monad/index.js","../Control.Monad.Cont.Class/index.js":"../output/Control.Monad.Cont.Class/index.js","../Control.Monad.Error.Class/index.js":"../output/Control.Monad.Error.Class/index.js","../Control.Monad.Reader.Class/index.js":"../output/Control.Monad.Reader.Class/index.js","../Control.Monad.Rec.Class/index.js":"../output/Control.Monad.Rec.Class/index.js","../Control.Monad.State.Class/index.js":"../output/Control.Monad.State.Class/index.js","../Control.Monad.Trans.Class/index.js":"../output/Control.Monad.Trans.Class/index.js","../Control.Monad.Writer.Class/index.js":"../output/Control.Monad.Writer.Class/index.js","../Data.Either/index.js":"../output/Data.Either/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Tuple/index.js":"../output/Data.Tuple/index.js","../Effect.Class/index.js":"../output/Effect.Class/index.js"}],"../output/Control.Monad.Maybe.Trans/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.functorMaybeT = exports.bindMaybeT = exports.applyMaybeT = exports.applicativeMaybeT = exports.alternativeMaybeT = exports.altMaybeT = exports.MaybeT = void 0;
Object.defineProperty(exports, "lift", {
  enumerable: true,
  get: function () {
    return Control_Monad_Trans_Class.lift;
  }
});
exports.semigroupMaybeT = exports.runMaybeT = exports.plusMaybeT = exports.newtypeMaybeT = exports.monoidMaybeT = exports.monadWriterMaybeT = exports.monadTransMaybeT = exports.monadThrowMaybeT = exports.monadTellMaybeT = exports.monadStateMaybeT = exports.monadRecMaybeT = exports.monadReaderMaybeT = exports.monadPlusMaybeT = exports.monadMaybeT = exports.monadErrorMaybeT = exports.monadEffectMaybe = exports.monadContMaybeT = exports.monadAskMaybeT = exports.mapMaybeT = void 0;

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Control_Monad = _interopRequireWildcard(require("../Control.Monad/index.js"));

var Control_Monad_Cont_Class = _interopRequireWildcard(require("../Control.Monad.Cont.Class/index.js"));

var Control_Monad_Error_Class = _interopRequireWildcard(require("../Control.Monad.Error.Class/index.js"));

var Control_Monad_Reader_Class = _interopRequireWildcard(require("../Control.Monad.Reader.Class/index.js"));

var Control_Monad_Rec_Class = _interopRequireWildcard(require("../Control.Monad.Rec.Class/index.js"));

var Control_Monad_State_Class = _interopRequireWildcard(require("../Control.Monad.State.Class/index.js"));

var Control_Monad_Trans_Class = _interopRequireWildcard(require("../Control.Monad.Trans.Class/index.js"));

var Control_Monad_Writer_Class = _interopRequireWildcard(require("../Control.Monad.Writer.Class/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Maybe = _interopRequireWildcard(require("../Data.Maybe/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Tuple = _interopRequireWildcard(require("../Data.Tuple/index.js"));

var Effect_Class = _interopRequireWildcard(require("../Effect.Class/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var _map =
/* #__PURE__ */
Data_Functor.map(Data_Maybe.functorMaybe);

var identity =
/* #__PURE__ */
Control_Category.identity(Control_Category.categoryFn);

var MaybeT = function MaybeT(x) {
  return x;
};

exports.MaybeT = MaybeT;

var runMaybeT = function runMaybeT(v) {
  return v;
};

exports.runMaybeT = runMaybeT;
var newtypeMaybeT = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeMaybeT = newtypeMaybeT;
var monadTransMaybeT = {
  lift: function lift(dictMonad) {
    var $157 = Control_Monad.liftM1(dictMonad)(Data_Maybe.Just.create);
    return function ($158) {
      return MaybeT($157($158));
    };
  }
};
exports.monadTransMaybeT = monadTransMaybeT;
var lift =
/* #__PURE__ */
Control_Monad_Trans_Class.lift(monadTransMaybeT);

var mapMaybeT = function mapMaybeT(f) {
  return function (v) {
    return f(v);
  };
};

exports.mapMaybeT = mapMaybeT;

var functorMaybeT = function functorMaybeT(dictFunctor) {
  var map1 = Data_Functor.map(dictFunctor);
  return {
    map: function map(f) {
      return function (v) {
        return map1(_map(f))(v);
      };
    }
  };
};

exports.functorMaybeT = functorMaybeT;

var monadMaybeT = function monadMaybeT(dictMonad) {
  return {
    Applicative0: function Applicative0() {
      return applicativeMaybeT(dictMonad);
    },
    Bind1: function Bind1() {
      return bindMaybeT(dictMonad);
    }
  };
};

exports.monadMaybeT = monadMaybeT;

var bindMaybeT = function bindMaybeT(dictMonad) {
  var _bind = Control_Bind.bind(dictMonad.Bind1());

  var pure = Control_Applicative.pure(dictMonad.Applicative0());
  return {
    bind: function bind(v) {
      return function (f) {
        return _bind(v)(function (v1) {
          if (v1 instanceof Data_Maybe.Nothing) {
            return pure(Data_Maybe.Nothing.value);
          }

          ;

          if (v1 instanceof Data_Maybe.Just) {
            var v2 = f(v1.value0);
            return v2;
          }

          ;
          throw new Error("Failed pattern match at Control.Monad.Maybe.Trans (line 54, column 11 - line 56, column 42): " + [v1.constructor.name]);
        });
      };
    },
    Apply0: function Apply0() {
      return applyMaybeT(dictMonad);
    }
  };
};

exports.bindMaybeT = bindMaybeT;

var applyMaybeT = function applyMaybeT(dictMonad) {
  var functorMaybeT1 = functorMaybeT(dictMonad.Bind1().Apply0().Functor0());
  return {
    apply: Control_Monad.ap(monadMaybeT(dictMonad)),
    Functor0: function Functor0() {
      return functorMaybeT1;
    }
  };
};

exports.applyMaybeT = applyMaybeT;

var applicativeMaybeT = function applicativeMaybeT(dictMonad) {
  return {
    pure: function () {
      var $159 = Control_Applicative.pure(dictMonad.Applicative0());
      return function ($160) {
        return MaybeT($159(Data_Maybe.Just.create($160)));
      };
    }(),
    Apply0: function Apply0() {
      return applyMaybeT(dictMonad);
    }
  };
};

exports.applicativeMaybeT = applicativeMaybeT;

var semigroupMaybeT = function semigroupMaybeT(dictMonad) {
  var lift2 = Control_Apply.lift2(applyMaybeT(dictMonad));
  return function (dictSemigroup) {
    return {
      append: lift2(Data_Semigroup.append(dictSemigroup))
    };
  };
};

exports.semigroupMaybeT = semigroupMaybeT;

var monadAskMaybeT = function monadAskMaybeT(dictMonadAsk) {
  var Monad0 = dictMonadAsk.Monad0();
  var monadMaybeT1 = monadMaybeT(Monad0);
  return {
    ask: lift(Monad0)(Control_Monad_Reader_Class.ask(dictMonadAsk)),
    Monad0: function Monad0() {
      return monadMaybeT1;
    }
  };
};

exports.monadAskMaybeT = monadAskMaybeT;

var monadReaderMaybeT = function monadReaderMaybeT(dictMonadReader) {
  var _local = Control_Monad_Reader_Class.local(dictMonadReader);

  var monadAskMaybeT1 = monadAskMaybeT(dictMonadReader.MonadAsk0());
  return {
    local: function local(f) {
      return mapMaybeT(_local(f));
    },
    MonadAsk0: function MonadAsk0() {
      return monadAskMaybeT1;
    }
  };
};

exports.monadReaderMaybeT = monadReaderMaybeT;

var monadContMaybeT = function monadContMaybeT(dictMonadCont) {
  var _callCC = Control_Monad_Cont_Class.callCC(dictMonadCont);

  var monadMaybeT1 = monadMaybeT(dictMonadCont.Monad0());
  return {
    callCC: function callCC(f) {
      return _callCC(function (c) {
        var v = f(function (a) {
          return c(new Data_Maybe.Just(a));
        });
        return v;
      });
    },
    Monad0: function Monad0() {
      return monadMaybeT1;
    }
  };
};

exports.monadContMaybeT = monadContMaybeT;

var monadEffectMaybe = function monadEffectMaybe(dictMonadEffect) {
  var Monad0 = dictMonadEffect.Monad0();
  var monadMaybeT1 = monadMaybeT(Monad0);
  return {
    liftEffect: function () {
      var $161 = lift(Monad0);
      var $162 = Effect_Class.liftEffect(dictMonadEffect);
      return function ($163) {
        return $161($162($163));
      };
    }(),
    Monad0: function Monad0() {
      return monadMaybeT1;
    }
  };
};

exports.monadEffectMaybe = monadEffectMaybe;

var monadRecMaybeT = function monadRecMaybeT(dictMonadRec) {
  var _tailRecM = Control_Monad_Rec_Class.tailRecM(dictMonadRec);

  var Monad0 = dictMonadRec.Monad0();
  var bind = Control_Bind.bind(Monad0.Bind1());
  var pure = Control_Applicative.pure(Monad0.Applicative0());
  var monadMaybeT1 = monadMaybeT(Monad0);
  return {
    tailRecM: function tailRecM(f) {
      var $164 = _tailRecM(function (a) {
        var v = f(a);
        return bind(v)(function (m$prime) {
          return pure(function () {
            if (m$prime instanceof Data_Maybe.Nothing) {
              return new Control_Monad_Rec_Class.Done(Data_Maybe.Nothing.value);
            }

            ;

            if (m$prime instanceof Data_Maybe.Just && m$prime.value0 instanceof Control_Monad_Rec_Class.Loop) {
              return new Control_Monad_Rec_Class.Loop(m$prime.value0.value0);
            }

            ;

            if (m$prime instanceof Data_Maybe.Just && m$prime.value0 instanceof Control_Monad_Rec_Class.Done) {
              return new Control_Monad_Rec_Class.Done(new Data_Maybe.Just(m$prime.value0.value0));
            }

            ;
            throw new Error("Failed pattern match at Control.Monad.Maybe.Trans (line 82, column 16 - line 85, column 43): " + [m$prime.constructor.name]);
          }());
        });
      });

      return function ($165) {
        return MaybeT($164($165));
      };
    },
    Monad0: function Monad0() {
      return monadMaybeT1;
    }
  };
};

exports.monadRecMaybeT = monadRecMaybeT;

var monadStateMaybeT = function monadStateMaybeT(dictMonadState) {
  var Monad0 = dictMonadState.Monad0();
  var lift1 = lift(Monad0);

  var _state = Control_Monad_State_Class.state(dictMonadState);

  var monadMaybeT1 = monadMaybeT(Monad0);
  return {
    state: function state(f) {
      return lift1(_state(f));
    },
    Monad0: function Monad0() {
      return monadMaybeT1;
    }
  };
};

exports.monadStateMaybeT = monadStateMaybeT;

var monadTellMaybeT = function monadTellMaybeT(dictMonadTell) {
  var Monad1 = dictMonadTell.Monad1();

  var _Semigroup = dictMonadTell.Semigroup0();

  var monadMaybeT1 = monadMaybeT(Monad1);
  return {
    tell: function () {
      var $166 = lift(Monad1);
      var $167 = Control_Monad_Writer_Class.tell(dictMonadTell);
      return function ($168) {
        return $166($167($168));
      };
    }(),
    Semigroup0: function Semigroup0() {
      return _Semigroup;
    },
    Monad1: function Monad1() {
      return monadMaybeT1;
    }
  };
};

exports.monadTellMaybeT = monadTellMaybeT;

var monadWriterMaybeT = function monadWriterMaybeT(dictMonadWriter) {
  var MonadTell1 = dictMonadWriter.MonadTell1();
  var Monad1 = MonadTell1.Monad1();
  var bind = Control_Bind.bind(Monad1.Bind1());
  var listen = Control_Monad_Writer_Class.listen(dictMonadWriter);
  var pure = Control_Applicative.pure(Monad1.Applicative0());
  var pass = Control_Monad_Writer_Class.pass(dictMonadWriter);

  var _Monoid = dictMonadWriter.Monoid0();

  var monadTellMaybeT1 = monadTellMaybeT(MonadTell1);
  return {
    listen: mapMaybeT(function (m) {
      return bind(listen(m))(function (v) {
        return pure(_map(function (r) {
          return new Data_Tuple.Tuple(r, v.value1);
        })(v.value0));
      });
    }),
    pass: mapMaybeT(function (m) {
      return pass(bind(m)(function (a) {
        return pure(function () {
          if (a instanceof Data_Maybe.Nothing) {
            return new Data_Tuple.Tuple(Data_Maybe.Nothing.value, identity);
          }

          ;

          if (a instanceof Data_Maybe.Just) {
            return new Data_Tuple.Tuple(new Data_Maybe.Just(a.value0.value0), a.value0.value1);
          }

          ;
          throw new Error("Failed pattern match at Control.Monad.Maybe.Trans (line 119, column 10 - line 121, column 43): " + [a.constructor.name]);
        }());
      }));
    }),
    Monoid0: function Monoid0() {
      return _Monoid;
    },
    MonadTell1: function MonadTell1() {
      return monadTellMaybeT1;
    }
  };
};

exports.monadWriterMaybeT = monadWriterMaybeT;

var monadThrowMaybeT = function monadThrowMaybeT(dictMonadThrow) {
  var Monad0 = dictMonadThrow.Monad0();
  var lift1 = lift(Monad0);

  var _throwError = Control_Monad_Error_Class.throwError(dictMonadThrow);

  var monadMaybeT1 = monadMaybeT(Monad0);
  return {
    throwError: function throwError(e) {
      return lift1(_throwError(e));
    },
    Monad0: function Monad0() {
      return monadMaybeT1;
    }
  };
};

exports.monadThrowMaybeT = monadThrowMaybeT;

var monadErrorMaybeT = function monadErrorMaybeT(dictMonadError) {
  var _catchError = Control_Monad_Error_Class.catchError(dictMonadError);

  var monadThrowMaybeT1 = monadThrowMaybeT(dictMonadError.MonadThrow0());
  return {
    catchError: function catchError(v) {
      return function (h) {
        return _catchError(v)(function (a) {
          var v1 = h(a);
          return v1;
        });
      };
    },
    MonadThrow0: function MonadThrow0() {
      return monadThrowMaybeT1;
    }
  };
};

exports.monadErrorMaybeT = monadErrorMaybeT;

var monoidMaybeT = function monoidMaybeT(dictMonad) {
  var pure = Control_Applicative.pure(applicativeMaybeT(dictMonad));
  var semigroupMaybeT1 = semigroupMaybeT(dictMonad);
  return function (dictMonoid) {
    var semigroupMaybeT2 = semigroupMaybeT1(dictMonoid.Semigroup0());
    return {
      mempty: pure(Data_Monoid.mempty(dictMonoid)),
      Semigroup0: function Semigroup0() {
        return semigroupMaybeT2;
      }
    };
  };
};

exports.monoidMaybeT = monoidMaybeT;

var altMaybeT = function altMaybeT(dictMonad) {
  var Bind1 = dictMonad.Bind1();
  var bind = Control_Bind.bind(Bind1);
  var pure = Control_Applicative.pure(dictMonad.Applicative0());
  var functorMaybeT1 = functorMaybeT(Bind1.Apply0().Functor0());
  return {
    alt: function alt(v) {
      return function (v1) {
        return bind(v)(function (m) {
          if (m instanceof Data_Maybe.Nothing) {
            return v1;
          }

          ;
          return pure(m);
        });
      };
    },
    Functor0: function Functor0() {
      return functorMaybeT1;
    }
  };
};

exports.altMaybeT = altMaybeT;

var plusMaybeT = function plusMaybeT(dictMonad) {
  var altMaybeT1 = altMaybeT(dictMonad);
  return {
    empty: Control_Applicative.pure(dictMonad.Applicative0())(Data_Maybe.Nothing.value),
    Alt0: function Alt0() {
      return altMaybeT1;
    }
  };
};

exports.plusMaybeT = plusMaybeT;

var alternativeMaybeT = function alternativeMaybeT(dictMonad) {
  var applicativeMaybeT1 = applicativeMaybeT(dictMonad);
  var plusMaybeT1 = plusMaybeT(dictMonad);
  return {
    Applicative0: function Applicative0() {
      return applicativeMaybeT1;
    },
    Plus1: function Plus1() {
      return plusMaybeT1;
    }
  };
};

exports.alternativeMaybeT = alternativeMaybeT;

var monadPlusMaybeT = function monadPlusMaybeT(dictMonad) {
  var monadMaybeT1 = monadMaybeT(dictMonad);
  var alternativeMaybeT1 = alternativeMaybeT(dictMonad);
  return {
    Monad0: function Monad0() {
      return monadMaybeT1;
    },
    Alternative1: function Alternative1() {
      return alternativeMaybeT1;
    }
  };
};

exports.monadPlusMaybeT = monadPlusMaybeT;
},{"../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Category/index.js":"../output/Control.Category/index.js","../Control.Monad/index.js":"../output/Control.Monad/index.js","../Control.Monad.Cont.Class/index.js":"../output/Control.Monad.Cont.Class/index.js","../Control.Monad.Error.Class/index.js":"../output/Control.Monad.Error.Class/index.js","../Control.Monad.Reader.Class/index.js":"../output/Control.Monad.Reader.Class/index.js","../Control.Monad.Rec.Class/index.js":"../output/Control.Monad.Rec.Class/index.js","../Control.Monad.State.Class/index.js":"../output/Control.Monad.State.Class/index.js","../Control.Monad.Trans.Class/index.js":"../output/Control.Monad.Trans.Class/index.js","../Control.Monad.Writer.Class/index.js":"../output/Control.Monad.Writer.Class/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Maybe/index.js":"../output/Data.Maybe/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Tuple/index.js":"../output/Data.Tuple/index.js","../Effect.Class/index.js":"../output/Effect.Class/index.js"}],"../output/Control.Plus/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "alt", {
  enumerable: true,
  get: function () {
    return Control_Alt.alt;
  }
});
exports.empty = void 0;
Object.defineProperty(exports, "map", {
  enumerable: true,
  get: function () {
    return Data_Functor.map;
  }
});
exports.plusArray = void 0;
Object.defineProperty(exports, "void", {
  enumerable: true,
  get: function () {
    return Data_Functor.void;
  }
});

var Control_Alt = _interopRequireWildcard(require("../Control.Alt/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var plusArray = {
  empty: [],
  Alt0: function Alt0() {
    return Control_Alt.altArray;
  }
};
exports.plusArray = plusArray;

var empty = function empty(dict) {
  return dict.empty;
};

exports.empty = empty;
},{"../Control.Alt/index.js":"../output/Control.Alt/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js"}],"../output/Safe.Coerce/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.coerce = void 0;

var Unsafe_Coerce = _interopRequireWildcard(require("../Unsafe.Coerce/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var coerce = function coerce() {
  return Unsafe_Coerce.unsafeCoerce;
};

exports.coerce = coerce;
},{"../Unsafe.Coerce/index.js":"../output/Unsafe.Coerce/index.js"}],"../output/Data.Newtype/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.wrap = exports.unwrap = exports.underF2 = exports.underF = exports.under2 = exports.under = exports.un = exports.traverse = exports.overF2 = exports.overF = exports.over2 = exports.over = exports.newtypeMultiplicative = exports.newtypeLast = exports.newtypeFirst = exports.newtypeEndo = exports.newtypeDual = exports.newtypeDisj = exports.newtypeConj = exports.newtypeAdditive = exports.modify = exports.collect = exports.alaF = exports.ala = void 0;

var Safe_Coerce = _interopRequireWildcard(require("../Safe.Coerce/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var coerce =
/* #__PURE__ */
Safe_Coerce.coerce();

var wrap = function wrap() {
  return coerce;
};

exports.wrap = wrap;
var wrap1 =
/* #__PURE__ */
wrap();

var unwrap = function unwrap() {
  return coerce;
};

exports.unwrap = unwrap;
var unwrap1 =
/* #__PURE__ */
unwrap();

var underF2 = function underF2() {
  return function () {
    return function () {
      return function () {
        return function (v) {
          return coerce;
        };
      };
    };
  };
};

exports.underF2 = underF2;

var underF = function underF() {
  return function () {
    return function () {
      return function () {
        return function (v) {
          return coerce;
        };
      };
    };
  };
};

exports.underF = underF;

var under2 = function under2() {
  return function () {
    return function (v) {
      return coerce;
    };
  };
};

exports.under2 = under2;

var under = function under() {
  return function () {
    return function (v) {
      return coerce;
    };
  };
};

exports.under = under;

var un = function un() {
  return function (v) {
    return unwrap1;
  };
};

exports.un = un;

var traverse = function traverse() {
  return function () {
    return function (v) {
      return coerce;
    };
  };
};

exports.traverse = traverse;

var overF2 = function overF2() {
  return function () {
    return function () {
      return function () {
        return function (v) {
          return coerce;
        };
      };
    };
  };
};

exports.overF2 = overF2;

var overF = function overF() {
  return function () {
    return function () {
      return function () {
        return function (v) {
          return coerce;
        };
      };
    };
  };
};

exports.overF = overF;

var over2 = function over2() {
  return function () {
    return function (v) {
      return coerce;
    };
  };
};

exports.over2 = over2;

var over = function over() {
  return function () {
    return function (v) {
      return coerce;
    };
  };
};

exports.over = over;
var newtypeMultiplicative = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeMultiplicative = newtypeMultiplicative;
var newtypeLast = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeLast = newtypeLast;
var newtypeFirst = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeFirst = newtypeFirst;
var newtypeEndo = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeEndo = newtypeEndo;
var newtypeDual = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeDual = newtypeDual;
var newtypeDisj = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeDisj = newtypeDisj;
var newtypeConj = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeConj = newtypeConj;
var newtypeAdditive = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeAdditive = newtypeAdditive;

var modify = function modify() {
  return function (fn) {
    return function (t) {
      return wrap1(fn(unwrap1(t)));
    };
  };
};

exports.modify = modify;

var collect = function collect() {
  return function () {
    return function (v) {
      return coerce;
    };
  };
};

exports.collect = collect;

var alaF = function alaF() {
  return function () {
    return function () {
      return function () {
        return function (v) {
          return coerce;
        };
      };
    };
  };
};

exports.alaF = alaF;

var ala = function ala() {
  return function () {
    return function () {
      return function (v) {
        return function (f) {
          return coerce(f(wrap1));
        };
      };
    };
  };
};

exports.ala = ala;
},{"../Safe.Coerce/index.js":"../output/Safe.Coerce/index.js"}],"../output/Type.Equality/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.to = exports.refl = exports.proof = exports.from = void 0;

// Generated by purs version 0.15.10
var To = function To(x) {
  return x;
};

var From = function From(x) {
  return x;
};

var refl = {
  proof: function proof(a) {
    return a;
  },
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.refl = refl;

var proof = function proof(dict) {
  return dict.proof;
};

exports.proof = proof;

var to = function to(dictTypeEquals) {
  var v = proof(dictTypeEquals)(function (a) {
    return a;
  });
  return v;
};

exports.to = to;

var from = function from(dictTypeEquals) {
  var v = proof(dictTypeEquals)(function (a) {
    return a;
  });
  return v;
};

exports.from = from;
},{}],"../output/Data.Distributive/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.distributiveTuple = exports.distributiveIdentity = exports.distributiveFunction = exports.distributeDefault = exports.distribute = exports.cotraverse = exports.collectDefault = exports.collect = void 0;

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Identity = _interopRequireWildcard(require("../Data.Identity/index.js"));

var Data_Newtype = _interopRequireWildcard(require("../Data.Newtype/index.js"));

var Data_Tuple = _interopRequireWildcard(require("../Data.Tuple/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Type_Equality = _interopRequireWildcard(require("../Type.Equality/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var unwrap =
/* #__PURE__ */
Data_Newtype.unwrap();
var identity =
/* #__PURE__ */
Control_Category.identity(Control_Category.categoryFn);
var distributiveIdentity = {
  distribute: function distribute(dictFunctor) {
    var $34 = Data_Functor.map(dictFunctor)(unwrap);
    return function ($35) {
      return Data_Identity.Identity($34($35));
    };
  },
  collect: function collect(dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (f) {
      var $36 = map(function ($38) {
        return unwrap(f($38));
      });
      return function ($37) {
        return Data_Identity.Identity($36($37));
      };
    };
  },
  Functor0: function Functor0() {
    return Data_Identity.functorIdentity;
  }
};
exports.distributiveIdentity = distributiveIdentity;

var distribute = function distribute(dict) {
  return dict.distribute;
};

exports.distribute = distribute;
var distributiveFunction = {
  distribute: function distribute(dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (a) {
      return function (e) {
        return map(function (v) {
          return v(e);
        })(a);
      };
    };
  },
  collect: function collect(dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (f) {
      var $39 = distribute(distributiveFunction)(dictFunctor);
      var $40 = map(f);
      return function ($41) {
        return $39($40($41));
      };
    };
  },
  Functor0: function Functor0() {
    return Data_Functor.functorFn;
  }
};
exports.distributiveFunction = distributiveFunction;

var cotraverse = function cotraverse(dictDistributive) {
  var map = Data_Functor.map(dictDistributive.Functor0());
  var distribute1 = distribute(dictDistributive);
  return function (dictFunctor) {
    var distribute2 = distribute1(dictFunctor);
    return function (f) {
      var $42 = map(f);
      return function ($43) {
        return $42(distribute2($43));
      };
    };
  };
};

exports.cotraverse = cotraverse;

var collectDefault = function collectDefault(dictDistributive) {
  var distribute1 = distribute(dictDistributive);
  return function (dictFunctor) {
    var distribute2 = distribute1(dictFunctor);
    var map = Data_Functor.map(dictFunctor);
    return function (f) {
      var $44 = map(f);
      return function ($45) {
        return distribute2($44($45));
      };
    };
  };
};

exports.collectDefault = collectDefault;

var distributiveTuple = function distributiveTuple(dictTypeEquals) {
  var from = Type_Equality.from(dictTypeEquals);
  return {
    collect: function collect(dictFunctor) {
      return collectDefault(distributiveTuple(dictTypeEquals))(dictFunctor);
    },
    distribute: function distribute(dictFunctor) {
      var $46 = Data_Tuple.Tuple.create(from(Data_Unit.unit));
      var $47 = Data_Functor.map(dictFunctor)(Data_Tuple.snd);
      return function ($48) {
        return $46($47($48));
      };
    },
    Functor0: function Functor0() {
      return Data_Tuple.functorTuple;
    }
  };
};

exports.distributiveTuple = distributiveTuple;

var collect = function collect(dict) {
  return dict.collect;
};

exports.collect = collect;

var distributeDefault = function distributeDefault(dictDistributive) {
  var collect1 = collect(dictDistributive);
  return function (dictFunctor) {
    return collect1(dictFunctor)(identity);
  };
};

exports.distributeDefault = distributeDefault;
},{"../Control.Category/index.js":"../output/Control.Category/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Identity/index.js":"../output/Data.Identity/index.js","../Data.Newtype/index.js":"../output/Data.Newtype/index.js","../Data.Tuple/index.js":"../output/Data.Tuple/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Type.Equality/index.js":"../output/Type.Equality/index.js"}],"../output/Control.Monad.Reader.Trans/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.applyReaderT = exports.applicativeReaderT = exports.alternativeReaderT = exports.altReaderT = exports.ReaderT = void 0;
Object.defineProperty(exports, "ask", {
  enumerable: true,
  get: function () {
    return Control_Monad_Reader_Class.ask;
  }
});
Object.defineProperty(exports, "asks", {
  enumerable: true,
  get: function () {
    return Control_Monad_Reader_Class.asks;
  }
});
exports.functorReaderT = exports.distributiveReaderT = exports.bindReaderT = void 0;
Object.defineProperty(exports, "lift", {
  enumerable: true,
  get: function () {
    return Control_Monad_Trans_Class.lift;
  }
});
Object.defineProperty(exports, "local", {
  enumerable: true,
  get: function () {
    return Control_Monad_Reader_Class.local;
  }
});
exports.withReaderT = exports.semigroupReaderT = exports.runReaderT = exports.plusReaderT = exports.newtypeReaderT = exports.monoidReaderT = exports.monadWriterReaderT = exports.monadTransReaderT = exports.monadThrowReaderT = exports.monadTellReaderT = exports.monadStateReaderT = exports.monadRecReaderT = exports.monadReaderT = exports.monadReaderReaderT = exports.monadPlusReaderT = exports.monadErrorReaderT = exports.monadEffectReader = exports.monadContReaderT = exports.monadAskReaderT = exports.mapReaderT = void 0;

var Control_Alt = _interopRequireWildcard(require("../Control.Alt/index.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Monad_Cont_Class = _interopRequireWildcard(require("../Control.Monad.Cont.Class/index.js"));

var Control_Monad_Error_Class = _interopRequireWildcard(require("../Control.Monad.Error.Class/index.js"));

var Control_Monad_Reader_Class = _interopRequireWildcard(require("../Control.Monad.Reader.Class/index.js"));

var Control_Monad_Rec_Class = _interopRequireWildcard(require("../Control.Monad.Rec.Class/index.js"));

var Control_Monad_State_Class = _interopRequireWildcard(require("../Control.Monad.State.Class/index.js"));

var Control_Monad_Trans_Class = _interopRequireWildcard(require("../Control.Monad.Trans.Class/index.js"));

var Control_Monad_Writer_Class = _interopRequireWildcard(require("../Control.Monad.Writer.Class/index.js"));

var Control_Plus = _interopRequireWildcard(require("../Control.Plus/index.js"));

var Data_Distributive = _interopRequireWildcard(require("../Data.Distributive/index.js"));

var Data_Function = _interopRequireWildcard(require("../Data.Function/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Effect_Class = _interopRequireWildcard(require("../Effect.Class/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var ReaderT = function ReaderT(x) {
  return x;
};

exports.ReaderT = ReaderT;

var withReaderT = function withReaderT(f) {
  return function (v) {
    return function ($146) {
      return v(f($146));
    };
  };
};

exports.withReaderT = withReaderT;

var runReaderT = function runReaderT(v) {
  return v;
};

exports.runReaderT = runReaderT;
var newtypeReaderT = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeReaderT = newtypeReaderT;
var monadTransReaderT = {
  lift: function lift(dictMonad) {
    return function ($147) {
      return ReaderT(Data_Function["const"]($147));
    };
  }
};
exports.monadTransReaderT = monadTransReaderT;
var lift =
/* #__PURE__ */
Control_Monad_Trans_Class.lift(monadTransReaderT);

var mapReaderT = function mapReaderT(f) {
  return function (v) {
    return function ($148) {
      return f(v($148));
    };
  };
};

exports.mapReaderT = mapReaderT;

var functorReaderT = function functorReaderT(dictFunctor) {
  return {
    map: function () {
      var $149 = Data_Functor.map(dictFunctor);
      return function ($150) {
        return mapReaderT($149($150));
      };
    }()
  };
};

exports.functorReaderT = functorReaderT;

var distributiveReaderT = function distributiveReaderT(dictDistributive) {
  var collect = Data_Distributive.collect(dictDistributive);
  var functorReaderT1 = functorReaderT(dictDistributive.Functor0());
  return {
    distribute: function distribute(dictFunctor) {
      var collect1 = collect(dictFunctor);
      return function (a) {
        return function (e) {
          return collect1(function (r) {
            return r(e);
          })(a);
        };
      };
    },
    collect: function collect(dictFunctor) {
      var map = Data_Functor.map(dictFunctor);
      return function (f) {
        var $151 = Data_Distributive.distribute(distributiveReaderT(dictDistributive))(dictFunctor);
        var $152 = map(f);
        return function ($153) {
          return $151($152($153));
        };
      };
    },
    Functor0: function Functor0() {
      return functorReaderT1;
    }
  };
};

exports.distributiveReaderT = distributiveReaderT;

var applyReaderT = function applyReaderT(dictApply) {
  var _apply = Control_Apply.apply(dictApply);

  var functorReaderT1 = functorReaderT(dictApply.Functor0());
  return {
    apply: function apply(v) {
      return function (v1) {
        return function (r) {
          return _apply(v(r))(v1(r));
        };
      };
    },
    Functor0: function Functor0() {
      return functorReaderT1;
    }
  };
};

exports.applyReaderT = applyReaderT;

var bindReaderT = function bindReaderT(dictBind) {
  var _bind = Control_Bind.bind(dictBind);

  var applyReaderT1 = applyReaderT(dictBind.Apply0());
  return {
    bind: function bind(v) {
      return function (k) {
        return function (r) {
          return _bind(v(r))(function (a) {
            var v1 = k(a);
            return v1(r);
          });
        };
      };
    },
    Apply0: function Apply0() {
      return applyReaderT1;
    }
  };
};

exports.bindReaderT = bindReaderT;

var semigroupReaderT = function semigroupReaderT(dictApply) {
  var lift2 = Control_Apply.lift2(applyReaderT(dictApply));
  return function (dictSemigroup) {
    return {
      append: lift2(Data_Semigroup.append(dictSemigroup))
    };
  };
};

exports.semigroupReaderT = semigroupReaderT;

var applicativeReaderT = function applicativeReaderT(dictApplicative) {
  var applyReaderT1 = applyReaderT(dictApplicative.Apply0());
  return {
    pure: function () {
      var $154 = Control_Applicative.pure(dictApplicative);
      return function ($155) {
        return ReaderT(Data_Function["const"]($154($155)));
      };
    }(),
    Apply0: function Apply0() {
      return applyReaderT1;
    }
  };
};

exports.applicativeReaderT = applicativeReaderT;

var monadReaderT = function monadReaderT(dictMonad) {
  var applicativeReaderT1 = applicativeReaderT(dictMonad.Applicative0());
  var bindReaderT1 = bindReaderT(dictMonad.Bind1());
  return {
    Applicative0: function Applicative0() {
      return applicativeReaderT1;
    },
    Bind1: function Bind1() {
      return bindReaderT1;
    }
  };
};

exports.monadReaderT = monadReaderT;

var monadAskReaderT = function monadAskReaderT(dictMonad) {
  var monadReaderT1 = monadReaderT(dictMonad);
  return {
    ask: Control_Applicative.pure(dictMonad.Applicative0()),
    Monad0: function Monad0() {
      return monadReaderT1;
    }
  };
};

exports.monadAskReaderT = monadAskReaderT;

var monadReaderReaderT = function monadReaderReaderT(dictMonad) {
  var monadAskReaderT1 = monadAskReaderT(dictMonad);
  return {
    local: withReaderT,
    MonadAsk0: function MonadAsk0() {
      return monadAskReaderT1;
    }
  };
};

exports.monadReaderReaderT = monadReaderReaderT;

var monadContReaderT = function monadContReaderT(dictMonadCont) {
  var _callCC = Control_Monad_Cont_Class.callCC(dictMonadCont);

  var monadReaderT1 = monadReaderT(dictMonadCont.Monad0());
  return {
    callCC: function callCC(f) {
      return function (r) {
        return _callCC(function (c) {
          var v = f(function ($156) {
            return ReaderT(Data_Function["const"](c($156)));
          });
          return v(r);
        });
      };
    },
    Monad0: function Monad0() {
      return monadReaderT1;
    }
  };
};

exports.monadContReaderT = monadContReaderT;

var monadEffectReader = function monadEffectReader(dictMonadEffect) {
  var Monad0 = dictMonadEffect.Monad0();
  var monadReaderT1 = monadReaderT(Monad0);
  return {
    liftEffect: function () {
      var $157 = lift(Monad0);
      var $158 = Effect_Class.liftEffect(dictMonadEffect);
      return function ($159) {
        return $157($158($159));
      };
    }(),
    Monad0: function Monad0() {
      return monadReaderT1;
    }
  };
};

exports.monadEffectReader = monadEffectReader;

var monadRecReaderT = function monadRecReaderT(dictMonadRec) {
  var Monad0 = dictMonadRec.Monad0();
  var bindFlipped = Control_Bind.bindFlipped(Monad0.Bind1());
  var pure = Control_Applicative.pure(Monad0.Applicative0());

  var _tailRecM = Control_Monad_Rec_Class.tailRecM(dictMonadRec);

  var monadReaderT1 = monadReaderT(Monad0);
  return {
    tailRecM: function tailRecM(k) {
      return function (a) {
        var k$prime = function k$prime(r) {
          return function (a$prime) {
            var v = k(a$prime);
            return bindFlipped(pure)(v(r));
          };
        };

        return function (r) {
          return _tailRecM(k$prime(r))(a);
        };
      };
    },
    Monad0: function Monad0() {
      return monadReaderT1;
    }
  };
};

exports.monadRecReaderT = monadRecReaderT;

var monadStateReaderT = function monadStateReaderT(dictMonadState) {
  var Monad0 = dictMonadState.Monad0();
  var monadReaderT1 = monadReaderT(Monad0);
  return {
    state: function () {
      var $160 = lift(Monad0);
      var $161 = Control_Monad_State_Class.state(dictMonadState);
      return function ($162) {
        return $160($161($162));
      };
    }(),
    Monad0: function Monad0() {
      return monadReaderT1;
    }
  };
};

exports.monadStateReaderT = monadStateReaderT;

var monadTellReaderT = function monadTellReaderT(dictMonadTell) {
  var Monad1 = dictMonadTell.Monad1();

  var _Semigroup = dictMonadTell.Semigroup0();

  var monadReaderT1 = monadReaderT(Monad1);
  return {
    tell: function () {
      var $163 = lift(Monad1);
      var $164 = Control_Monad_Writer_Class.tell(dictMonadTell);
      return function ($165) {
        return $163($164($165));
      };
    }(),
    Semigroup0: function Semigroup0() {
      return _Semigroup;
    },
    Monad1: function Monad1() {
      return monadReaderT1;
    }
  };
};

exports.monadTellReaderT = monadTellReaderT;

var monadWriterReaderT = function monadWriterReaderT(dictMonadWriter) {
  var _Monoid = dictMonadWriter.Monoid0();

  var monadTellReaderT1 = monadTellReaderT(dictMonadWriter.MonadTell1());
  return {
    listen: mapReaderT(Control_Monad_Writer_Class.listen(dictMonadWriter)),
    pass: mapReaderT(Control_Monad_Writer_Class.pass(dictMonadWriter)),
    Monoid0: function Monoid0() {
      return _Monoid;
    },
    MonadTell1: function MonadTell1() {
      return monadTellReaderT1;
    }
  };
};

exports.monadWriterReaderT = monadWriterReaderT;

var monadThrowReaderT = function monadThrowReaderT(dictMonadThrow) {
  var Monad0 = dictMonadThrow.Monad0();
  var monadReaderT1 = monadReaderT(Monad0);
  return {
    throwError: function () {
      var $166 = lift(Monad0);
      var $167 = Control_Monad_Error_Class.throwError(dictMonadThrow);
      return function ($168) {
        return $166($167($168));
      };
    }(),
    Monad0: function Monad0() {
      return monadReaderT1;
    }
  };
};

exports.monadThrowReaderT = monadThrowReaderT;

var monadErrorReaderT = function monadErrorReaderT(dictMonadError) {
  var _catchError = Control_Monad_Error_Class.catchError(dictMonadError);

  var monadThrowReaderT1 = monadThrowReaderT(dictMonadError.MonadThrow0());
  return {
    catchError: function catchError(v) {
      return function (h) {
        return function (r) {
          return _catchError(v(r))(function (e) {
            var v1 = h(e);
            return v1(r);
          });
        };
      };
    },
    MonadThrow0: function MonadThrow0() {
      return monadThrowReaderT1;
    }
  };
};

exports.monadErrorReaderT = monadErrorReaderT;

var monoidReaderT = function monoidReaderT(dictApplicative) {
  var pure = Control_Applicative.pure(applicativeReaderT(dictApplicative));
  var semigroupReaderT1 = semigroupReaderT(dictApplicative.Apply0());
  return function (dictMonoid) {
    var semigroupReaderT2 = semigroupReaderT1(dictMonoid.Semigroup0());
    return {
      mempty: pure(Data_Monoid.mempty(dictMonoid)),
      Semigroup0: function Semigroup0() {
        return semigroupReaderT2;
      }
    };
  };
};

exports.monoidReaderT = monoidReaderT;

var altReaderT = function altReaderT(dictAlt) {
  var _alt = Control_Alt.alt(dictAlt);

  var functorReaderT1 = functorReaderT(dictAlt.Functor0());
  return {
    alt: function alt(v) {
      return function (v1) {
        return function (r) {
          return _alt(v(r))(v1(r));
        };
      };
    },
    Functor0: function Functor0() {
      return functorReaderT1;
    }
  };
};

exports.altReaderT = altReaderT;

var plusReaderT = function plusReaderT(dictPlus) {
  var altReaderT1 = altReaderT(dictPlus.Alt0());
  return {
    empty: Data_Function["const"](Control_Plus.empty(dictPlus)),
    Alt0: function Alt0() {
      return altReaderT1;
    }
  };
};

exports.plusReaderT = plusReaderT;

var alternativeReaderT = function alternativeReaderT(dictAlternative) {
  var applicativeReaderT1 = applicativeReaderT(dictAlternative.Applicative0());
  var plusReaderT1 = plusReaderT(dictAlternative.Plus1());
  return {
    Applicative0: function Applicative0() {
      return applicativeReaderT1;
    },
    Plus1: function Plus1() {
      return plusReaderT1;
    }
  };
};

exports.alternativeReaderT = alternativeReaderT;

var monadPlusReaderT = function monadPlusReaderT(dictMonadPlus) {
  var monadReaderT1 = monadReaderT(dictMonadPlus.Monad0());
  var alternativeReaderT1 = alternativeReaderT(dictMonadPlus.Alternative1());
  return {
    Monad0: function Monad0() {
      return monadReaderT1;
    },
    Alternative1: function Alternative1() {
      return alternativeReaderT1;
    }
  };
};

exports.monadPlusReaderT = monadPlusReaderT;
},{"../Control.Alt/index.js":"../output/Control.Alt/index.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Monad.Cont.Class/index.js":"../output/Control.Monad.Cont.Class/index.js","../Control.Monad.Error.Class/index.js":"../output/Control.Monad.Error.Class/index.js","../Control.Monad.Reader.Class/index.js":"../output/Control.Monad.Reader.Class/index.js","../Control.Monad.Rec.Class/index.js":"../output/Control.Monad.Rec.Class/index.js","../Control.Monad.State.Class/index.js":"../output/Control.Monad.State.Class/index.js","../Control.Monad.Trans.Class/index.js":"../output/Control.Monad.Trans.Class/index.js","../Control.Monad.Writer.Class/index.js":"../output/Control.Monad.Writer.Class/index.js","../Control.Plus/index.js":"../output/Control.Plus/index.js","../Data.Distributive/index.js":"../output/Data.Distributive/index.js","../Data.Function/index.js":"../output/Data.Function/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Effect.Class/index.js":"../output/Effect.Class/index.js"}],"../output/Control.Monad.Writer.Trans/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.bindWriterT = exports.applyWriterT = exports.applicativeWriterT = exports.alternativeWriterT = exports.altWriterT = exports.WriterT = void 0;
Object.defineProperty(exports, "censor", {
  enumerable: true,
  get: function () {
    return Control_Monad_Writer_Class.censor;
  }
});
exports.functorWriterT = exports.execWriterT = void 0;
Object.defineProperty(exports, "lift", {
  enumerable: true,
  get: function () {
    return Control_Monad_Trans_Class.lift;
  }
});
Object.defineProperty(exports, "listen", {
  enumerable: true,
  get: function () {
    return Control_Monad_Writer_Class.listen;
  }
});
Object.defineProperty(exports, "listens", {
  enumerable: true,
  get: function () {
    return Control_Monad_Writer_Class.listens;
  }
});
exports.newtypeWriterT = exports.monoidWriterT = exports.monadWriterWriterT = exports.monadWriterT = exports.monadTransWriterT = exports.monadThrowWriterT = exports.monadTellWriterT = exports.monadStateWriterT = exports.monadRecWriterT = exports.monadReaderWriterT = exports.monadPlusWriterT = exports.monadErrorWriterT = exports.monadEffectWriter = exports.monadContWriterT = exports.monadAskWriterT = exports.mapWriterT = void 0;
Object.defineProperty(exports, "pass", {
  enumerable: true,
  get: function () {
    return Control_Monad_Writer_Class.pass;
  }
});
exports.semigroupWriterT = exports.runWriterT = exports.plusWriterT = void 0;
Object.defineProperty(exports, "tell", {
  enumerable: true,
  get: function () {
    return Control_Monad_Writer_Class.tell;
  }
});

var Control_Alt = _interopRequireWildcard(require("../Control.Alt/index.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Monad_Cont_Class = _interopRequireWildcard(require("../Control.Monad.Cont.Class/index.js"));

var Control_Monad_Error_Class = _interopRequireWildcard(require("../Control.Monad.Error.Class/index.js"));

var Control_Monad_Reader_Class = _interopRequireWildcard(require("../Control.Monad.Reader.Class/index.js"));

var Control_Monad_Rec_Class = _interopRequireWildcard(require("../Control.Monad.Rec.Class/index.js"));

var Control_Monad_State_Class = _interopRequireWildcard(require("../Control.Monad.State.Class/index.js"));

var Control_Monad_Trans_Class = _interopRequireWildcard(require("../Control.Monad.Trans.Class/index.js"));

var Control_Monad_Writer_Class = _interopRequireWildcard(require("../Control.Monad.Writer.Class/index.js"));

var Control_Plus = _interopRequireWildcard(require("../Control.Plus/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Tuple = _interopRequireWildcard(require("../Data.Tuple/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Effect_Class = _interopRequireWildcard(require("../Effect.Class/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var WriterT = function WriterT(x) {
  return x;
};

exports.WriterT = WriterT;

var runWriterT = function runWriterT(v) {
  return v;
};

exports.runWriterT = runWriterT;
var newtypeWriterT = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeWriterT = newtypeWriterT;

var monadTransWriterT = function monadTransWriterT(dictMonoid) {
  var mempty = Data_Monoid.mempty(dictMonoid);
  return {
    lift: function lift(dictMonad) {
      var bind = Control_Bind.bind(dictMonad.Bind1());
      var pure = Control_Applicative.pure(dictMonad.Applicative0());
      return function (m) {
        return bind(m)(function (a) {
          return pure(new Data_Tuple.Tuple(a, mempty));
        });
      };
    }
  };
};

exports.monadTransWriterT = monadTransWriterT;

var mapWriterT = function mapWriterT(f) {
  return function (v) {
    return f(v);
  };
};

exports.mapWriterT = mapWriterT;

var functorWriterT = function functorWriterT(dictFunctor) {
  var _map = Data_Functor.map(dictFunctor);

  return {
    map: function map(f) {
      return mapWriterT(_map(function (v) {
        return new Data_Tuple.Tuple(f(v.value0), v.value1);
      }));
    }
  };
};

exports.functorWriterT = functorWriterT;

var execWriterT = function execWriterT(dictFunctor) {
  var map = Data_Functor.map(dictFunctor);
  return function (v) {
    return map(Data_Tuple.snd)(v);
  };
};

exports.execWriterT = execWriterT;

var applyWriterT = function applyWriterT(dictSemigroup) {
  var append = Data_Semigroup.append(dictSemigroup);
  return function (dictApply) {
    var _apply = Control_Apply.apply(dictApply);

    var Functor0 = dictApply.Functor0();
    var map = Data_Functor.map(Functor0);
    var functorWriterT1 = functorWriterT(Functor0);
    return {
      apply: function apply(v) {
        return function (v1) {
          var k = function k(v3) {
            return function (v4) {
              return new Data_Tuple.Tuple(v3.value0(v4.value0), append(v3.value1)(v4.value1));
            };
          };

          return _apply(map(k)(v))(v1);
        };
      },
      Functor0: function Functor0() {
        return functorWriterT1;
      }
    };
  };
};

exports.applyWriterT = applyWriterT;

var bindWriterT = function bindWriterT(dictSemigroup) {
  var append = Data_Semigroup.append(dictSemigroup);
  var applyWriterT1 = applyWriterT(dictSemigroup);
  return function (dictBind) {
    var _bind = Control_Bind.bind(dictBind);

    var Apply0 = dictBind.Apply0();
    var map = Data_Functor.map(Apply0.Functor0());
    var applyWriterT2 = applyWriterT1(Apply0);
    return {
      bind: function bind(v) {
        return function (k) {
          return _bind(v)(function (v1) {
            var v2 = k(v1.value0);
            return map(function (v3) {
              return new Data_Tuple.Tuple(v3.value0, append(v1.value1)(v3.value1));
            })(v2);
          });
        };
      },
      Apply0: function Apply0() {
        return applyWriterT2;
      }
    };
  };
};

exports.bindWriterT = bindWriterT;

var semigroupWriterT = function semigroupWriterT(dictApply) {
  return function (dictSemigroup) {
    var lift2 = Control_Apply.lift2(applyWriterT(dictSemigroup)(dictApply));
    return function (dictSemigroup1) {
      return {
        append: lift2(Data_Semigroup.append(dictSemigroup1))
      };
    };
  };
};

exports.semigroupWriterT = semigroupWriterT;

var applicativeWriterT = function applicativeWriterT(dictMonoid) {
  var mempty = Data_Monoid.mempty(dictMonoid);
  var applyWriterT1 = applyWriterT(dictMonoid.Semigroup0());
  return function (dictApplicative) {
    var _pure = Control_Applicative.pure(dictApplicative);

    var applyWriterT2 = applyWriterT1(dictApplicative.Apply0());
    return {
      pure: function pure(a) {
        return _pure(new Data_Tuple.Tuple(a, mempty));
      },
      Apply0: function Apply0() {
        return applyWriterT2;
      }
    };
  };
};

exports.applicativeWriterT = applicativeWriterT;

var monadWriterT = function monadWriterT(dictMonoid) {
  var applicativeWriterT1 = applicativeWriterT(dictMonoid);
  var bindWriterT1 = bindWriterT(dictMonoid.Semigroup0());
  return function (dictMonad) {
    var applicativeWriterT2 = applicativeWriterT1(dictMonad.Applicative0());
    var bindWriterT2 = bindWriterT1(dictMonad.Bind1());
    return {
      Applicative0: function Applicative0() {
        return applicativeWriterT2;
      },
      Bind1: function Bind1() {
        return bindWriterT2;
      }
    };
  };
};

exports.monadWriterT = monadWriterT;

var monadAskWriterT = function monadAskWriterT(dictMonoid) {
  var lift = Control_Monad_Trans_Class.lift(monadTransWriterT(dictMonoid));
  var monadWriterT1 = monadWriterT(dictMonoid);
  return function (dictMonadAsk) {
    var Monad0 = dictMonadAsk.Monad0();
    var monadWriterT2 = monadWriterT1(Monad0);
    return {
      ask: lift(Monad0)(Control_Monad_Reader_Class.ask(dictMonadAsk)),
      Monad0: function Monad0() {
        return monadWriterT2;
      }
    };
  };
};

exports.monadAskWriterT = monadAskWriterT;

var monadReaderWriterT = function monadReaderWriterT(dictMonoid) {
  var monadAskWriterT1 = monadAskWriterT(dictMonoid);
  return function (dictMonadReader) {
    var _local = Control_Monad_Reader_Class.local(dictMonadReader);

    var monadAskWriterT2 = monadAskWriterT1(dictMonadReader.MonadAsk0());
    return {
      local: function local(f) {
        return mapWriterT(_local(f));
      },
      MonadAsk0: function MonadAsk0() {
        return monadAskWriterT2;
      }
    };
  };
};

exports.monadReaderWriterT = monadReaderWriterT;

var monadContWriterT = function monadContWriterT(dictMonoid) {
  var mempty = Data_Monoid.mempty(dictMonoid);
  var monadWriterT1 = monadWriterT(dictMonoid);
  return function (dictMonadCont) {
    var _callCC = Control_Monad_Cont_Class.callCC(dictMonadCont);

    var monadWriterT2 = monadWriterT1(dictMonadCont.Monad0());
    return {
      callCC: function callCC(f) {
        return _callCC(function (c) {
          var v = f(function (a) {
            return c(new Data_Tuple.Tuple(a, mempty));
          });
          return v;
        });
      },
      Monad0: function Monad0() {
        return monadWriterT2;
      }
    };
  };
};

exports.monadContWriterT = monadContWriterT;

var monadEffectWriter = function monadEffectWriter(dictMonoid) {
  var lift = Control_Monad_Trans_Class.lift(monadTransWriterT(dictMonoid));
  var monadWriterT1 = monadWriterT(dictMonoid);
  return function (dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    var monadWriterT2 = monadWriterT1(Monad0);
    return {
      liftEffect: function () {
        var $249 = lift(Monad0);
        var $250 = Effect_Class.liftEffect(dictMonadEffect);
        return function ($251) {
          return $249($250($251));
        };
      }(),
      Monad0: function Monad0() {
        return monadWriterT2;
      }
    };
  };
};

exports.monadEffectWriter = monadEffectWriter;

var monadRecWriterT = function monadRecWriterT(dictMonoid) {
  var append = Data_Semigroup.append(dictMonoid.Semigroup0());
  var mempty = Data_Monoid.mempty(dictMonoid);
  var monadWriterT1 = monadWriterT(dictMonoid);
  return function (dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var bind = Control_Bind.bind(Monad0.Bind1());
    var pure = Control_Applicative.pure(Monad0.Applicative0());

    var _tailRecM = Control_Monad_Rec_Class.tailRecM(dictMonadRec);

    var monadWriterT2 = monadWriterT1(Monad0);
    return {
      tailRecM: function tailRecM(f) {
        return function (a) {
          var f$prime = function f$prime(v) {
            var v1 = f(v.value0);
            return bind(v1)(function (v2) {
              return pure(function () {
                if (v2.value0 instanceof Control_Monad_Rec_Class.Loop) {
                  return new Control_Monad_Rec_Class.Loop(new Data_Tuple.Tuple(v2.value0.value0, append(v.value1)(v2.value1)));
                }

                ;

                if (v2.value0 instanceof Control_Monad_Rec_Class.Done) {
                  return new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(v2.value0.value0, append(v.value1)(v2.value1)));
                }

                ;
                throw new Error("Failed pattern match at Control.Monad.Writer.Trans (line 83, column 16 - line 85, column 47): " + [v2.value0.constructor.name]);
              }());
            });
          };

          return _tailRecM(f$prime)(new Data_Tuple.Tuple(a, mempty));
        };
      },
      Monad0: function Monad0() {
        return monadWriterT2;
      }
    };
  };
};

exports.monadRecWriterT = monadRecWriterT;

var monadStateWriterT = function monadStateWriterT(dictMonoid) {
  var lift = Control_Monad_Trans_Class.lift(monadTransWriterT(dictMonoid));
  var monadWriterT1 = monadWriterT(dictMonoid);
  return function (dictMonadState) {
    var Monad0 = dictMonadState.Monad0();
    var lift1 = lift(Monad0);

    var _state = Control_Monad_State_Class.state(dictMonadState);

    var monadWriterT2 = monadWriterT1(Monad0);
    return {
      state: function state(f) {
        return lift1(_state(f));
      },
      Monad0: function Monad0() {
        return monadWriterT2;
      }
    };
  };
};

exports.monadStateWriterT = monadStateWriterT;

var monadTellWriterT = function monadTellWriterT(dictMonoid) {
  var _Semigroup = dictMonoid.Semigroup0();

  var monadWriterT1 = monadWriterT(dictMonoid);
  return function (dictMonad) {
    var monadWriterT2 = monadWriterT1(dictMonad);
    return {
      tell: function () {
        var $252 = Control_Applicative.pure(dictMonad.Applicative0());
        var $253 = Data_Tuple.Tuple.create(Data_Unit.unit);
        return function ($254) {
          return WriterT($252($253($254)));
        };
      }(),
      Semigroup0: function Semigroup0() {
        return _Semigroup;
      },
      Monad1: function Monad1() {
        return monadWriterT2;
      }
    };
  };
};

exports.monadTellWriterT = monadTellWriterT;

var monadWriterWriterT = function monadWriterWriterT(dictMonoid) {
  var monadTellWriterT1 = monadTellWriterT(dictMonoid);
  return function (dictMonad) {
    var bind = Control_Bind.bind(dictMonad.Bind1());
    var pure = Control_Applicative.pure(dictMonad.Applicative0());
    var monadTellWriterT2 = monadTellWriterT1(dictMonad);
    return {
      listen: function listen(v) {
        return bind(v)(function (v1) {
          return pure(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v1.value0, v1.value1), v1.value1));
        });
      },
      pass: function pass(v) {
        return bind(v)(function (v1) {
          return pure(new Data_Tuple.Tuple(v1.value0.value0, v1.value0.value1(v1.value1)));
        });
      },
      Monoid0: function Monoid0() {
        return dictMonoid;
      },
      MonadTell1: function MonadTell1() {
        return monadTellWriterT2;
      }
    };
  };
};

exports.monadWriterWriterT = monadWriterWriterT;

var monadThrowWriterT = function monadThrowWriterT(dictMonoid) {
  var lift = Control_Monad_Trans_Class.lift(monadTransWriterT(dictMonoid));
  var monadWriterT1 = monadWriterT(dictMonoid);
  return function (dictMonadThrow) {
    var Monad0 = dictMonadThrow.Monad0();
    var lift1 = lift(Monad0);

    var _throwError = Control_Monad_Error_Class.throwError(dictMonadThrow);

    var monadWriterT2 = monadWriterT1(Monad0);
    return {
      throwError: function throwError(e) {
        return lift1(_throwError(e));
      },
      Monad0: function Monad0() {
        return monadWriterT2;
      }
    };
  };
};

exports.monadThrowWriterT = monadThrowWriterT;

var monadErrorWriterT = function monadErrorWriterT(dictMonoid) {
  var monadThrowWriterT1 = monadThrowWriterT(dictMonoid);
  return function (dictMonadError) {
    var _catchError = Control_Monad_Error_Class.catchError(dictMonadError);

    var monadThrowWriterT2 = monadThrowWriterT1(dictMonadError.MonadThrow0());
    return {
      catchError: function catchError(v) {
        return function (h) {
          return _catchError(v)(function (e) {
            var v1 = h(e);
            return v1;
          });
        };
      },
      MonadThrow0: function MonadThrow0() {
        return monadThrowWriterT2;
      }
    };
  };
};

exports.monadErrorWriterT = monadErrorWriterT;

var monoidWriterT = function monoidWriterT(dictApplicative) {
  var semigroupWriterT1 = semigroupWriterT(dictApplicative.Apply0());
  return function (dictMonoid) {
    var pure = Control_Applicative.pure(applicativeWriterT(dictMonoid)(dictApplicative));
    var semigroupWriterT2 = semigroupWriterT1(dictMonoid.Semigroup0());
    return function (dictMonoid1) {
      var semigroupWriterT3 = semigroupWriterT2(dictMonoid1.Semigroup0());
      return {
        mempty: pure(Data_Monoid.mempty(dictMonoid1)),
        Semigroup0: function Semigroup0() {
          return semigroupWriterT3;
        }
      };
    };
  };
};

exports.monoidWriterT = monoidWriterT;

var altWriterT = function altWriterT(dictAlt) {
  var _alt = Control_Alt.alt(dictAlt);

  var functorWriterT1 = functorWriterT(dictAlt.Functor0());
  return {
    alt: function alt(v) {
      return function (v1) {
        return _alt(v)(v1);
      };
    },
    Functor0: function Functor0() {
      return functorWriterT1;
    }
  };
};

exports.altWriterT = altWriterT;

var plusWriterT = function plusWriterT(dictPlus) {
  var altWriterT1 = altWriterT(dictPlus.Alt0());
  return {
    empty: Control_Plus.empty(dictPlus),
    Alt0: function Alt0() {
      return altWriterT1;
    }
  };
};

exports.plusWriterT = plusWriterT;

var alternativeWriterT = function alternativeWriterT(dictMonoid) {
  var applicativeWriterT1 = applicativeWriterT(dictMonoid);
  return function (dictAlternative) {
    var applicativeWriterT2 = applicativeWriterT1(dictAlternative.Applicative0());
    var plusWriterT1 = plusWriterT(dictAlternative.Plus1());
    return {
      Applicative0: function Applicative0() {
        return applicativeWriterT2;
      },
      Plus1: function Plus1() {
        return plusWriterT1;
      }
    };
  };
};

exports.alternativeWriterT = alternativeWriterT;

var monadPlusWriterT = function monadPlusWriterT(dictMonoid) {
  var monadWriterT1 = monadWriterT(dictMonoid);
  var alternativeWriterT1 = alternativeWriterT(dictMonoid);
  return function (dictMonadPlus) {
    var monadWriterT2 = monadWriterT1(dictMonadPlus.Monad0());
    var alternativeWriterT2 = alternativeWriterT1(dictMonadPlus.Alternative1());
    return {
      Monad0: function Monad0() {
        return monadWriterT2;
      },
      Alternative1: function Alternative1() {
        return alternativeWriterT2;
      }
    };
  };
};

exports.monadPlusWriterT = monadPlusWriterT;
},{"../Control.Alt/index.js":"../output/Control.Alt/index.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Monad.Cont.Class/index.js":"../output/Control.Monad.Cont.Class/index.js","../Control.Monad.Error.Class/index.js":"../output/Control.Monad.Error.Class/index.js","../Control.Monad.Reader.Class/index.js":"../output/Control.Monad.Reader.Class/index.js","../Control.Monad.Rec.Class/index.js":"../output/Control.Monad.Rec.Class/index.js","../Control.Monad.State.Class/index.js":"../output/Control.Monad.State.Class/index.js","../Control.Monad.Trans.Class/index.js":"../output/Control.Monad.Trans.Class/index.js","../Control.Monad.Writer.Class/index.js":"../output/Control.Monad.Writer.Class/index.js","../Control.Plus/index.js":"../output/Control.Plus/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Tuple/index.js":"../output/Data.Tuple/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Effect.Class/index.js":"../output/Effect.Class/index.js"}],"../output/Data.Functor.App/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showApp = exports.semigroupApp = exports.plusApp = exports.ordApp = exports.ord1App = exports.newtypeApp = exports.monoidApp = exports.monadPlusApp = exports.monadApp = exports.lazyApp = exports.hoistLowerApp = exports.hoistLiftApp = exports.hoistApp = exports.functorApp = exports.extendApp = exports.eqApp = exports.eq1App = exports.comonadApp = exports.bindApp = exports.applyApp = exports.applicativeApp = exports.alternativeApp = exports.altApp = exports.App = void 0;

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

var Unsafe_Coerce = _interopRequireWildcard(require("../Unsafe.Coerce/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var App = function App(x) {
  return x;
};

exports.App = App;

var showApp = function showApp(dictShow) {
  var _show = Data_Show.show(dictShow);

  return {
    show: function show(v) {
      return "(App " + (_show(v) + ")");
    }
  };
};

exports.showApp = showApp;

var semigroupApp = function semigroupApp(dictApply) {
  var lift2 = Control_Apply.lift2(dictApply);
  return function (dictSemigroup) {
    var append1 = Data_Semigroup.append(dictSemigroup);
    return {
      append: function append(v) {
        return function (v1) {
          return lift2(append1)(v)(v1);
        };
      }
    };
  };
};

exports.semigroupApp = semigroupApp;

var plusApp = function plusApp(dictPlus) {
  return dictPlus;
};

exports.plusApp = plusApp;
var newtypeApp = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeApp = newtypeApp;

var monoidApp = function monoidApp(dictApplicative) {
  var pure = Control_Applicative.pure(dictApplicative);
  var semigroupApp1 = semigroupApp(dictApplicative.Apply0());
  return function (dictMonoid) {
    var semigroupApp2 = semigroupApp1(dictMonoid.Semigroup0());
    return {
      mempty: pure(Data_Monoid.mempty(dictMonoid)),
      Semigroup0: function Semigroup0() {
        return semigroupApp2;
      }
    };
  };
};

exports.monoidApp = monoidApp;

var monadPlusApp = function monadPlusApp(dictMonadPlus) {
  return dictMonadPlus;
};

exports.monadPlusApp = monadPlusApp;

var monadApp = function monadApp(dictMonad) {
  return dictMonad;
};

exports.monadApp = monadApp;

var lazyApp = function lazyApp(dictLazy) {
  return dictLazy;
};

exports.lazyApp = lazyApp;
var hoistLowerApp = Unsafe_Coerce.unsafeCoerce;
exports.hoistLowerApp = hoistLowerApp;
var hoistLiftApp = Unsafe_Coerce.unsafeCoerce;
exports.hoistLiftApp = hoistLiftApp;

var hoistApp = function hoistApp(f) {
  return function (v) {
    return f(v);
  };
};

exports.hoistApp = hoistApp;

var functorApp = function functorApp(dictFunctor) {
  return dictFunctor;
};

exports.functorApp = functorApp;

var extendApp = function extendApp(dictExtend) {
  return dictExtend;
};

exports.extendApp = extendApp;

var eqApp = function eqApp(dictEq1) {
  var eq1 = Data_Eq.eq1(dictEq1);
  return function (dictEq) {
    var eq11 = eq1(dictEq);
    return {
      eq: function eq(x) {
        return function (y) {
          return eq11(x)(y);
        };
      }
    };
  };
};

exports.eqApp = eqApp;

var ordApp = function ordApp(dictOrd1) {
  var compare1 = Data_Ord.compare1(dictOrd1);
  var eqApp1 = eqApp(dictOrd1.Eq10());
  return function (dictOrd) {
    var compare11 = compare1(dictOrd);
    var eqApp2 = eqApp1(dictOrd.Eq0());
    return {
      compare: function compare(x) {
        return function (y) {
          return compare11(x)(y);
        };
      },
      Eq0: function Eq0() {
        return eqApp2;
      }
    };
  };
};

exports.ordApp = ordApp;

var eq1App = function eq1App(dictEq1) {
  var eqApp1 = eqApp(dictEq1);
  return {
    eq1: function eq1(dictEq) {
      return Data_Eq.eq(eqApp1(dictEq));
    }
  };
};

exports.eq1App = eq1App;

var ord1App = function ord1App(dictOrd1) {
  var ordApp1 = ordApp(dictOrd1);
  var eq1App1 = eq1App(dictOrd1.Eq10());
  return {
    compare1: function compare1(dictOrd) {
      return Data_Ord.compare(ordApp1(dictOrd));
    },
    Eq10: function Eq10() {
      return eq1App1;
    }
  };
};

exports.ord1App = ord1App;

var comonadApp = function comonadApp(dictComonad) {
  return dictComonad;
};

exports.comonadApp = comonadApp;

var bindApp = function bindApp(dictBind) {
  return dictBind;
};

exports.bindApp = bindApp;

var applyApp = function applyApp(dictApply) {
  return dictApply;
};

exports.applyApp = applyApp;

var applicativeApp = function applicativeApp(dictApplicative) {
  return dictApplicative;
};

exports.applicativeApp = applicativeApp;

var alternativeApp = function alternativeApp(dictAlternative) {
  return dictAlternative;
};

exports.alternativeApp = alternativeApp;

var altApp = function altApp(dictAlt) {
  return dictAlt;
};

exports.altApp = altApp;
},{"../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Show/index.js":"../output/Data.Show/index.js","../Unsafe.Coerce/index.js":"../output/Unsafe.Coerce/index.js"}],"../output/Data.Functor.Compose/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showCompose = exports.plusCompose = exports.ordCompose = exports.ord1Compose = exports.newtypeCompose = exports.functorCompose = exports.eqCompose = exports.eq1Compose = exports.bihoistCompose = exports.applyCompose = exports.applicativeCompose = exports.alternativeCompose = exports.altCompose = exports.Compose = void 0;

var Control_Alt = _interopRequireWildcard(require("../Control.Alt/index.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Plus = _interopRequireWildcard(require("../Control.Plus/index.js"));

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Functor_App = _interopRequireWildcard(require("../Data.Functor.App/index.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var Compose = function Compose(x) {
  return x;
};

exports.Compose = Compose;

var showCompose = function showCompose(dictShow) {
  var _show = Data_Show.show(dictShow);

  return {
    show: function show(v) {
      return "(Compose " + (_show(v) + ")");
    }
  };
};

exports.showCompose = showCompose;
var newtypeCompose = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeCompose = newtypeCompose;

var functorCompose = function functorCompose(dictFunctor) {
  var _map = Data_Functor.map(dictFunctor);

  return function (dictFunctor1) {
    var map1 = Data_Functor.map(dictFunctor1);
    return {
      map: function map(f) {
        return function (v) {
          return _map(map1(f))(v);
        };
      }
    };
  };
};

exports.functorCompose = functorCompose;

var eqCompose = function eqCompose(dictEq1) {
  var eq1 = Data_Eq.eq1(dictEq1);
  return function (dictEq11) {
    var eqApp = Data_Functor_App.eqApp(dictEq11);
    return function (dictEq) {
      var eq11 = eq1(eqApp(dictEq));
      return {
        eq: function eq(v) {
          return function (v1) {
            return eq11(Data_Functor_App.hoistLiftApp(v))(Data_Functor_App.hoistLiftApp(v1));
          };
        }
      };
    };
  };
};

exports.eqCompose = eqCompose;

var ordCompose = function ordCompose(dictOrd1) {
  var compare1 = Data_Ord.compare1(dictOrd1);
  var eqCompose1 = eqCompose(dictOrd1.Eq10());
  return function (dictOrd11) {
    var ordApp = Data_Functor_App.ordApp(dictOrd11);
    var eqCompose2 = eqCompose1(dictOrd11.Eq10());
    return function (dictOrd) {
      var compare11 = compare1(ordApp(dictOrd));
      var eqCompose3 = eqCompose2(dictOrd.Eq0());
      return {
        compare: function compare(v) {
          return function (v1) {
            return compare11(Data_Functor_App.hoistLiftApp(v))(Data_Functor_App.hoistLiftApp(v1));
          };
        },
        Eq0: function Eq0() {
          return eqCompose3;
        }
      };
    };
  };
};

exports.ordCompose = ordCompose;

var eq1Compose = function eq1Compose(dictEq1) {
  var eqCompose1 = eqCompose(dictEq1);
  return function (dictEq11) {
    var eqCompose2 = eqCompose1(dictEq11);
    return {
      eq1: function eq1(dictEq) {
        return Data_Eq.eq(eqCompose2(dictEq));
      }
    };
  };
};

exports.eq1Compose = eq1Compose;

var ord1Compose = function ord1Compose(dictOrd1) {
  var ordCompose1 = ordCompose(dictOrd1);
  var eq1Compose1 = eq1Compose(dictOrd1.Eq10());
  return function (dictOrd11) {
    var ordCompose2 = ordCompose1(dictOrd11);
    var eq1Compose2 = eq1Compose1(dictOrd11.Eq10());
    return {
      compare1: function compare1(dictOrd) {
        return Data_Ord.compare(ordCompose2(dictOrd));
      },
      Eq10: function Eq10() {
        return eq1Compose2;
      }
    };
  };
};

exports.ord1Compose = ord1Compose;

var bihoistCompose = function bihoistCompose(dictFunctor) {
  var map = Data_Functor.map(dictFunctor);
  return function (natF) {
    return function (natG) {
      return function (v) {
        return natF(map(natG)(v));
      };
    };
  };
};

exports.bihoistCompose = bihoistCompose;

var applyCompose = function applyCompose(dictApply) {
  var _apply = Control_Apply.apply(dictApply);

  var Functor0 = dictApply.Functor0();
  var map = Data_Functor.map(Functor0);
  var functorCompose1 = functorCompose(Functor0);
  return function (dictApply1) {
    var apply1 = Control_Apply.apply(dictApply1);
    var functorCompose2 = functorCompose1(dictApply1.Functor0());
    return {
      apply: function apply(v) {
        return function (v1) {
          return _apply(map(apply1)(v))(v1);
        };
      },
      Functor0: function Functor0() {
        return functorCompose2;
      }
    };
  };
};

exports.applyCompose = applyCompose;

var applicativeCompose = function applicativeCompose(dictApplicative) {
  var pure = Control_Applicative.pure(dictApplicative);
  var applyCompose1 = applyCompose(dictApplicative.Apply0());
  return function (dictApplicative1) {
    var applyCompose2 = applyCompose1(dictApplicative1.Apply0());
    return {
      pure: function () {
        var $112 = Control_Applicative.pure(dictApplicative1);
        return function ($113) {
          return Compose(pure($112($113)));
        };
      }(),
      Apply0: function Apply0() {
        return applyCompose2;
      }
    };
  };
};

exports.applicativeCompose = applicativeCompose;

var altCompose = function altCompose(dictAlt) {
  var _alt = Control_Alt.alt(dictAlt);

  var functorCompose1 = functorCompose(dictAlt.Functor0());
  return function (dictFunctor) {
    var functorCompose2 = functorCompose1(dictFunctor);
    return {
      alt: function alt(v) {
        return function (v1) {
          return _alt(v)(v1);
        };
      },
      Functor0: function Functor0() {
        return functorCompose2;
      }
    };
  };
};

exports.altCompose = altCompose;

var plusCompose = function plusCompose(dictPlus) {
  var empty = Control_Plus.empty(dictPlus);
  var altCompose1 = altCompose(dictPlus.Alt0());
  return function (dictFunctor) {
    var altCompose2 = altCompose1(dictFunctor);
    return {
      empty: empty,
      Alt0: function Alt0() {
        return altCompose2;
      }
    };
  };
};

exports.plusCompose = plusCompose;

var alternativeCompose = function alternativeCompose(dictAlternative) {
  var applicativeCompose1 = applicativeCompose(dictAlternative.Applicative0());
  var plusCompose1 = plusCompose(dictAlternative.Plus1());
  return function (dictApplicative) {
    var applicativeCompose2 = applicativeCompose1(dictApplicative);
    var plusCompose2 = plusCompose1(dictApplicative.Apply0().Functor0());
    return {
      Applicative0: function Applicative0() {
        return applicativeCompose2;
      },
      Plus1: function Plus1() {
        return plusCompose2;
      }
    };
  };
};

exports.alternativeCompose = alternativeCompose;
},{"../Control.Alt/index.js":"../output/Control.Alt/index.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Plus/index.js":"../output/Control.Plus/index.js","../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Functor.App/index.js":"../output/Data.Functor.App/index.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Show/index.js":"../output/Data.Show/index.js"}],"../output/Control.Extend/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.arrayExtend = void 0;

var arrayExtend = function arrayExtend(f) {
  return function (xs) {
    return xs.map(function (_, i, xs) {
      return f(xs.slice(i));
    });
  };
};

exports.arrayExtend = arrayExtend;
},{}],"../output/Control.Extend/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.extendFn = exports.extendFlipped = exports.extendArray = exports.extend = exports.duplicate = exports.composeCoKleisliFlipped = exports.composeCoKleisli = void 0;
Object.defineProperty(exports, "map", {
  enumerable: true,
  get: function () {
    return Data_Functor.map;
  }
});
Object.defineProperty(exports, "void", {
  enumerable: true,
  get: function () {
    return Data_Functor.void;
  }
});

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var identity =
/* #__PURE__ */
Control_Category.identity(Control_Category.categoryFn);

var extendFn = function extendFn(dictSemigroup) {
  var append = Data_Semigroup.append(dictSemigroup);
  return {
    extend: function extend(f) {
      return function (g) {
        return function (w) {
          return f(function (w$prime) {
            return g(append(w)(w$prime));
          });
        };
      };
    },
    Functor0: function Functor0() {
      return Data_Functor.functorFn;
    }
  };
};

exports.extendFn = extendFn;
var extendArray = {
  extend: $foreign.arrayExtend,
  Functor0: function Functor0() {
    return Data_Functor.functorArray;
  }
};
exports.extendArray = extendArray;

var extend = function extend(dict) {
  return dict.extend;
};

exports.extend = extend;

var extendFlipped = function extendFlipped(dictExtend) {
  var extend1 = extend(dictExtend);
  return function (w) {
    return function (f) {
      return extend1(f)(w);
    };
  };
};

exports.extendFlipped = extendFlipped;

var duplicate = function duplicate(dictExtend) {
  return extend(dictExtend)(identity);
};

exports.duplicate = duplicate;

var composeCoKleisliFlipped = function composeCoKleisliFlipped(dictExtend) {
  var extend1 = extend(dictExtend);
  return function (f) {
    return function (g) {
      return function (w) {
        return f(extend1(g)(w));
      };
    };
  };
};

exports.composeCoKleisliFlipped = composeCoKleisliFlipped;

var composeCoKleisli = function composeCoKleisli(dictExtend) {
  var extend1 = extend(dictExtend);
  return function (f) {
    return function (g) {
      return function (w) {
        return g(extend1(f)(w));
      };
    };
  };
};

exports.composeCoKleisli = composeCoKleisli;
},{"./foreign.js":"../output/Control.Extend/foreign.js","../Control.Category/index.js":"../output/Control.Category/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js"}],"../output/Control.Comonad/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "duplicate", {
  enumerable: true,
  get: function () {
    return Control_Extend.duplicate;
  }
});
Object.defineProperty(exports, "extend", {
  enumerable: true,
  get: function () {
    return Control_Extend.extend;
  }
});
exports.extract = void 0;
Object.defineProperty(exports, "map", {
  enumerable: true,
  get: function () {
    return Data_Functor.map;
  }
});
Object.defineProperty(exports, "void", {
  enumerable: true,
  get: function () {
    return Data_Functor.void;
  }
});

var Control_Extend = _interopRequireWildcard(require("../Control.Extend/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var extract = function extract(dict) {
  return dict.extract;
};

exports.extract = extract;
},{"../Control.Extend/index.js":"../output/Control.Extend/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js"}],"../output/Data.Functor.Contravariant/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.imapC = exports.contravariantConst = exports.coerce = exports.cmapFlipped = exports.cmap = void 0;

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Void = _interopRequireWildcard(require("../Data.Void/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var contravariantConst = {
  cmap: function cmap(v) {
    return function (v1) {
      return v1;
    };
  }
};
exports.contravariantConst = contravariantConst;

var cmap = function cmap(dict) {
  return dict.cmap;
};

exports.cmap = cmap;

var cmapFlipped = function cmapFlipped(dictContravariant) {
  var cmap1 = cmap(dictContravariant);
  return function (x) {
    return function (f) {
      return cmap1(f)(x);
    };
  };
};

exports.cmapFlipped = cmapFlipped;

var coerce = function coerce(dictContravariant) {
  var cmap1 = cmap(dictContravariant);
  return function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (a) {
      return map(Data_Void.absurd)(cmap1(Data_Void.absurd)(a));
    };
  };
};

exports.coerce = coerce;

var imapC = function imapC(dictContravariant) {
  var cmap1 = cmap(dictContravariant);
  return function (v) {
    return function (f) {
      return cmap1(f);
    };
  };
};

exports.imapC = imapC;
},{"../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Void/index.js":"../output/Data.Void/index.js"}],"../output/Data.Profunctor/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.wrapIso = exports.unwrapIso = exports.rmap = exports.profunctorFn = exports.lcmap = exports.dimap = exports.arr = void 0;

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Data_Newtype = _interopRequireWildcard(require("../Data.Newtype/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var identity =
/* #__PURE__ */
Control_Category.identity(Control_Category.categoryFn);
var wrap =
/* #__PURE__ */
Data_Newtype.wrap();
var unwrap =
/* #__PURE__ */
Data_Newtype.unwrap();
var profunctorFn = {
  dimap: function dimap(a2b) {
    return function (c2d) {
      return function (b2c) {
        return function ($18) {
          return c2d(b2c(a2b($18)));
        };
      };
    };
  }
};
exports.profunctorFn = profunctorFn;

var dimap = function dimap(dict) {
  return dict.dimap;
};

exports.dimap = dimap;

var lcmap = function lcmap(dictProfunctor) {
  var dimap1 = dimap(dictProfunctor);
  return function (a2b) {
    return dimap1(a2b)(identity);
  };
};

exports.lcmap = lcmap;

var rmap = function rmap(dictProfunctor) {
  var dimap1 = dimap(dictProfunctor);
  return function (b2c) {
    return dimap1(identity)(b2c);
  };
};

exports.rmap = rmap;

var unwrapIso = function unwrapIso(dictProfunctor) {
  var dimap1 = dimap(dictProfunctor);
  return function () {
    return dimap1(wrap)(unwrap);
  };
};

exports.unwrapIso = unwrapIso;

var wrapIso = function wrapIso(dictProfunctor) {
  var dimap1 = dimap(dictProfunctor);
  return function () {
    return function (v) {
      return dimap1(unwrap)(wrap);
    };
  };
};

exports.wrapIso = wrapIso;

var arr = function arr(dictCategory) {
  var identity1 = Control_Category.identity(dictCategory);
  return function (dictProfunctor) {
    var rmap1 = rmap(dictProfunctor);
    return function (f) {
      return rmap1(f)(identity1);
    };
  };
};

exports.arr = arr;
},{"../Control.Category/index.js":"../output/Control.Category/index.js","../Data.Newtype/index.js":"../output/Data.Newtype/index.js"}],"../output/Data.Functor.Costar/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.strongCostar = exports.semigroupoidCostar = exports.profunctorCostar = exports.newtypeCostar = exports.monadCostar = exports.invariantCostar = exports.hoistCostar = exports.functorCostar = exports.distributiveCostar = exports.closedCostar = exports.categoryCostar = exports.bindCostar = exports.bifunctorCostar = exports.applyCostar = exports.applicativeCostar = exports.Costar = void 0;

var Control_Comonad = _interopRequireWildcard(require("../Control.Comonad/index.js"));

var Control_Extend = _interopRequireWildcard(require("../Control.Extend/index.js"));

var Data_Distributive = _interopRequireWildcard(require("../Data.Distributive/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Functor_Contravariant = _interopRequireWildcard(require("../Data.Functor.Contravariant/index.js"));

var Data_Functor_Invariant = _interopRequireWildcard(require("../Data.Functor.Invariant/index.js"));

var Data_Profunctor = _interopRequireWildcard(require("../Data.Profunctor/index.js"));

var Data_Tuple = _interopRequireWildcard(require("../Data.Tuple/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var lcmap =
/* #__PURE__ */
Data_Profunctor.lcmap(Data_Profunctor.profunctorFn);

var Costar = function Costar(x) {
  return x;
};

exports.Costar = Costar;

var semigroupoidCostar = function semigroupoidCostar(dictExtend) {
  var composeCoKleisliFlipped = Control_Extend.composeCoKleisliFlipped(dictExtend);
  return {
    compose: function compose(v) {
      return function (v1) {
        return composeCoKleisliFlipped(v)(v1);
      };
    }
  };
};

exports.semigroupoidCostar = semigroupoidCostar;

var profunctorCostar = function profunctorCostar(dictFunctor) {
  var map = Data_Functor.map(dictFunctor);
  return {
    dimap: function dimap(f) {
      return function (g) {
        return function (v) {
          var $65 = map(f);
          return function ($66) {
            return g(v($65($66)));
          };
        };
      };
    }
  };
};

exports.profunctorCostar = profunctorCostar;

var strongCostar = function strongCostar(dictComonad) {
  var Functor0 = dictComonad.Extend0().Functor0();
  var map = Data_Functor.map(Functor0);
  var extract = Control_Comonad.extract(dictComonad);
  var profunctorCostar1 = profunctorCostar(Functor0);
  return {
    first: function first(v) {
      return function (x) {
        return new Data_Tuple.Tuple(v(map(Data_Tuple.fst)(x)), Data_Tuple.snd(extract(x)));
      };
    },
    second: function second(v) {
      return function (x) {
        return new Data_Tuple.Tuple(Data_Tuple.fst(extract(x)), v(map(Data_Tuple.snd)(x)));
      };
    },
    Profunctor0: function Profunctor0() {
      return profunctorCostar1;
    }
  };
};

exports.strongCostar = strongCostar;
var newtypeCostar = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeCostar = newtypeCostar;

var hoistCostar = function hoistCostar(f) {
  return function (v) {
    return lcmap(f)(v);
  };
};

exports.hoistCostar = hoistCostar;
var functorCostar = {
  map: function map(f) {
    return function (v) {
      return function ($67) {
        return f(v($67));
      };
    };
  }
};
exports.functorCostar = functorCostar;
var invariantCostar = {
  imap:
  /* #__PURE__ */
  Data_Functor_Invariant.imapF(functorCostar)
};
exports.invariantCostar = invariantCostar;
var distributiveCostar = {
  distribute: function distribute(dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (f) {
      return function (a) {
        return map(function (v) {
          return v(a);
        })(f);
      };
    };
  },
  collect: function collect(dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (f) {
      var $68 = Data_Distributive.distribute(distributiveCostar)(dictFunctor);
      var $69 = map(f);
      return function ($70) {
        return $68($69($70));
      };
    };
  },
  Functor0: function Functor0() {
    return functorCostar;
  }
};
exports.distributiveCostar = distributiveCostar;

var closedCostar = function closedCostar(dictFunctor) {
  var map = Data_Functor.map(dictFunctor);
  var profunctorCostar1 = profunctorCostar(dictFunctor);
  return {
    closed: function closed(v) {
      return function (g) {
        return function (x) {
          return v(map(function (v1) {
            return v1(x);
          })(g));
        };
      };
    },
    Profunctor0: function Profunctor0() {
      return profunctorCostar1;
    }
  };
};

exports.closedCostar = closedCostar;

var categoryCostar = function categoryCostar(dictComonad) {
  var semigroupoidCostar1 = semigroupoidCostar(dictComonad.Extend0());
  return {
    identity: Control_Comonad.extract(dictComonad),
    Semigroupoid0: function Semigroupoid0() {
      return semigroupoidCostar1;
    }
  };
};

exports.categoryCostar = categoryCostar;

var bifunctorCostar = function bifunctorCostar(dictContravariant) {
  var cmap = Data_Functor_Contravariant.cmap(dictContravariant);
  return {
    bimap: function bimap(f) {
      return function (g) {
        return function (v) {
          var $71 = cmap(f);
          return function ($72) {
            return g(v($71($72)));
          };
        };
      };
    }
  };
};

exports.bifunctorCostar = bifunctorCostar;
var applyCostar = {
  apply: function apply(v) {
    return function (v1) {
      return function (a) {
        return v(a)(v1(a));
      };
    };
  },
  Functor0: function Functor0() {
    return functorCostar;
  }
};
exports.applyCostar = applyCostar;
var bindCostar = {
  bind: function bind(v) {
    return function (f) {
      return function (x) {
        var v1 = f(v(x));
        return v1(x);
      };
    };
  },
  Apply0: function Apply0() {
    return applyCostar;
  }
};
exports.bindCostar = bindCostar;
var applicativeCostar = {
  pure: function pure(a) {
    return function (v) {
      return a;
    };
  },
  Apply0: function Apply0() {
    return applyCostar;
  }
};
exports.applicativeCostar = applicativeCostar;
var monadCostar = {
  Applicative0: function Applicative0() {
    return applicativeCostar;
  },
  Bind1: function Bind1() {
    return bindCostar;
  }
};
exports.monadCostar = monadCostar;
},{"../Control.Comonad/index.js":"../output/Control.Comonad/index.js","../Control.Extend/index.js":"../output/Control.Extend/index.js","../Data.Distributive/index.js":"../output/Data.Distributive/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Functor.Contravariant/index.js":"../output/Data.Functor.Contravariant/index.js","../Data.Functor.Invariant/index.js":"../output/Data.Functor.Invariant/index.js","../Data.Profunctor/index.js":"../output/Data.Profunctor/index.js","../Data.Tuple/index.js":"../output/Data.Tuple/index.js"}],"../output/Data.Profunctor.Star/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.strongStar = exports.semigroupoidStar = exports.profunctorStar = exports.plusStar = exports.newtypeStar = exports.monadStar = exports.monadPlusStar = exports.invariantStar = exports.hoistStar = exports.functorStar = exports.distributiveStar = exports.closedStar = exports.choiceStar = exports.categoryStar = exports.bindStar = exports.applyStar = exports.applicativeStar = exports.alternativeStar = exports.altStar = exports.Star = void 0;

var Control_Alt = _interopRequireWildcard(require("../Control.Alt/index.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Plus = _interopRequireWildcard(require("../Control.Plus/index.js"));

var Data_Distributive = _interopRequireWildcard(require("../Data.Distributive/index.js"));

var Data_Either = _interopRequireWildcard(require("../Data.Either/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Functor_Invariant = _interopRequireWildcard(require("../Data.Functor.Invariant/index.js"));

var Data_Tuple = _interopRequireWildcard(require("../Data.Tuple/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var Star = function Star(x) {
  return x;
};

exports.Star = Star;

var semigroupoidStar = function semigroupoidStar(dictBind) {
  var bind = Control_Bind.bind(dictBind);
  return {
    compose: function compose(v) {
      return function (v1) {
        return function (x) {
          return bind(v1(x))(v);
        };
      };
    }
  };
};

exports.semigroupoidStar = semigroupoidStar;

var profunctorStar = function profunctorStar(dictFunctor) {
  var map = Data_Functor.map(dictFunctor);
  return {
    dimap: function dimap(f) {
      return function (g) {
        return function (v) {
          var $127 = map(g);
          return function ($128) {
            return $127(v(f($128)));
          };
        };
      };
    }
  };
};

exports.profunctorStar = profunctorStar;

var strongStar = function strongStar(dictFunctor) {
  var map = Data_Functor.map(dictFunctor);
  var profunctorStar1 = profunctorStar(dictFunctor);
  return {
    first: function first(v) {
      return function (v1) {
        return map(function (v2) {
          return new Data_Tuple.Tuple(v2, v1.value1);
        })(v(v1.value0));
      };
    },
    second: function second(v) {
      return function (v1) {
        return map(Data_Tuple.Tuple.create(v1.value0))(v(v1.value1));
      };
    },
    Profunctor0: function Profunctor0() {
      return profunctorStar1;
    }
  };
};

exports.strongStar = strongStar;
var newtypeStar = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeStar = newtypeStar;

var invariantStar = function invariantStar(dictInvariant) {
  var _imap = Data_Functor_Invariant.imap(dictInvariant);

  return {
    imap: function imap(f) {
      return function (g) {
        return function (v) {
          var $129 = _imap(f)(g);

          return function ($130) {
            return $129(v($130));
          };
        };
      };
    }
  };
};

exports.invariantStar = invariantStar;

var hoistStar = function hoistStar(f) {
  return function (v) {
    return function ($131) {
      return f(v($131));
    };
  };
};

exports.hoistStar = hoistStar;

var functorStar = function functorStar(dictFunctor) {
  var _map = Data_Functor.map(dictFunctor);

  return {
    map: function map(f) {
      return function (v) {
        var $132 = _map(f);

        return function ($133) {
          return $132(v($133));
        };
      };
    }
  };
};

exports.functorStar = functorStar;

var distributiveStar = function distributiveStar(dictDistributive) {
  var collect = Data_Distributive.collect(dictDistributive);
  var functorStar1 = functorStar(dictDistributive.Functor0());
  return {
    distribute: function distribute(dictFunctor) {
      var collect1 = collect(dictFunctor);
      return function (f) {
        return function (a) {
          return collect1(function (v) {
            return v(a);
          })(f);
        };
      };
    },
    collect: function collect(dictFunctor) {
      var map = Data_Functor.map(dictFunctor);
      return function (f) {
        var $134 = Data_Distributive.distribute(distributiveStar(dictDistributive))(dictFunctor);
        var $135 = map(f);
        return function ($136) {
          return $134($135($136));
        };
      };
    },
    Functor0: function Functor0() {
      return functorStar1;
    }
  };
};

exports.distributiveStar = distributiveStar;

var closedStar = function closedStar(dictDistributive) {
  var distribute = Data_Distributive.distribute(dictDistributive)(Data_Functor.functorFn);
  var profunctorStar1 = profunctorStar(dictDistributive.Functor0());
  return {
    closed: function closed(v) {
      return function (g) {
        return distribute(function ($137) {
          return v(g($137));
        });
      };
    },
    Profunctor0: function Profunctor0() {
      return profunctorStar1;
    }
  };
};

exports.closedStar = closedStar;

var choiceStar = function choiceStar(dictApplicative) {
  var Functor0 = dictApplicative.Apply0().Functor0();
  var map = Data_Functor.map(Functor0);
  var pure = Control_Applicative.pure(dictApplicative);
  var profunctorStar1 = profunctorStar(Functor0);
  return {
    left: function left(v) {
      return Data_Either.either(function () {
        var $138 = map(Data_Either.Left.create);
        return function ($139) {
          return $138(v($139));
        };
      }())(function ($140) {
        return pure(Data_Either.Right.create($140));
      });
    },
    right: function right(v) {
      return Data_Either.either(function ($141) {
        return pure(Data_Either.Left.create($141));
      })(function () {
        var $142 = map(Data_Either.Right.create);
        return function ($143) {
          return $142(v($143));
        };
      }());
    },
    Profunctor0: function Profunctor0() {
      return profunctorStar1;
    }
  };
};

exports.choiceStar = choiceStar;

var categoryStar = function categoryStar(dictMonad) {
  var semigroupoidStar1 = semigroupoidStar(dictMonad.Bind1());
  return {
    identity: Control_Applicative.pure(dictMonad.Applicative0()),
    Semigroupoid0: function Semigroupoid0() {
      return semigroupoidStar1;
    }
  };
};

exports.categoryStar = categoryStar;

var applyStar = function applyStar(dictApply) {
  var _apply = Control_Apply.apply(dictApply);

  var functorStar1 = functorStar(dictApply.Functor0());
  return {
    apply: function apply(v) {
      return function (v1) {
        return function (a) {
          return _apply(v(a))(v1(a));
        };
      };
    },
    Functor0: function Functor0() {
      return functorStar1;
    }
  };
};

exports.applyStar = applyStar;

var bindStar = function bindStar(dictBind) {
  var _bind = Control_Bind.bind(dictBind);

  var applyStar1 = applyStar(dictBind.Apply0());
  return {
    bind: function bind(v) {
      return function (f) {
        return function (x) {
          return _bind(v(x))(function (a) {
            var v1 = f(a);
            return v1(x);
          });
        };
      };
    },
    Apply0: function Apply0() {
      return applyStar1;
    }
  };
};

exports.bindStar = bindStar;

var applicativeStar = function applicativeStar(dictApplicative) {
  var _pure = Control_Applicative.pure(dictApplicative);

  var applyStar1 = applyStar(dictApplicative.Apply0());
  return {
    pure: function pure(a) {
      return function (v) {
        return _pure(a);
      };
    },
    Apply0: function Apply0() {
      return applyStar1;
    }
  };
};

exports.applicativeStar = applicativeStar;

var monadStar = function monadStar(dictMonad) {
  var applicativeStar1 = applicativeStar(dictMonad.Applicative0());
  var bindStar1 = bindStar(dictMonad.Bind1());
  return {
    Applicative0: function Applicative0() {
      return applicativeStar1;
    },
    Bind1: function Bind1() {
      return bindStar1;
    }
  };
};

exports.monadStar = monadStar;

var altStar = function altStar(dictAlt) {
  var _alt = Control_Alt.alt(dictAlt);

  var functorStar1 = functorStar(dictAlt.Functor0());
  return {
    alt: function alt(v) {
      return function (v1) {
        return function (a) {
          return _alt(v(a))(v1(a));
        };
      };
    },
    Functor0: function Functor0() {
      return functorStar1;
    }
  };
};

exports.altStar = altStar;

var plusStar = function plusStar(dictPlus) {
  var _empty = Control_Plus.empty(dictPlus);

  var altStar1 = altStar(dictPlus.Alt0());
  return {
    empty: function empty(v) {
      return _empty;
    },
    Alt0: function Alt0() {
      return altStar1;
    }
  };
};

exports.plusStar = plusStar;

var alternativeStar = function alternativeStar(dictAlternative) {
  var applicativeStar1 = applicativeStar(dictAlternative.Applicative0());
  var plusStar1 = plusStar(dictAlternative.Plus1());
  return {
    Applicative0: function Applicative0() {
      return applicativeStar1;
    },
    Plus1: function Plus1() {
      return plusStar1;
    }
  };
};

exports.alternativeStar = alternativeStar;

var monadPlusStar = function monadPlusStar(dictMonadPlus) {
  var monadStar1 = monadStar(dictMonadPlus.Monad0());
  var alternativeStar1 = alternativeStar(dictMonadPlus.Alternative1());
  return {
    Monad0: function Monad0() {
      return monadStar1;
    },
    Alternative1: function Alternative1() {
      return alternativeStar1;
    }
  };
};

exports.monadPlusStar = monadPlusStar;
},{"../Control.Alt/index.js":"../output/Control.Alt/index.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Plus/index.js":"../output/Control.Plus/index.js","../Data.Distributive/index.js":"../output/Data.Distributive/index.js","../Data.Either/index.js":"../output/Data.Either/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Functor.Invariant/index.js":"../output/Data.Functor.Invariant/index.js","../Data.Tuple/index.js":"../output/Data.Tuple/index.js"}],"../output/Control.Parallel.Class/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.sequential = exports.plusParCont = exports.parallel = exports.newtypeParCont = exports.monadParWriterT = exports.monadParStar = exports.monadParReaderT = exports.monadParParCont = exports.monadParMaybeT = exports.monadParExceptT = exports.monadParCostar = exports.functorParCont = exports.applyParCont = exports.applicativeParCont = exports.alternativeParCont = exports.altParCont = exports.ParCont = void 0;

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Monad_Cont_Trans = _interopRequireWildcard(require("../Control.Monad.Cont.Trans/index.js"));

var Control_Monad_Except_Trans = _interopRequireWildcard(require("../Control.Monad.Except.Trans/index.js"));

var Control_Monad_Maybe_Trans = _interopRequireWildcard(require("../Control.Monad.Maybe.Trans/index.js"));

var Control_Monad_Reader_Trans = _interopRequireWildcard(require("../Control.Monad.Reader.Trans/index.js"));

var Control_Monad_Writer_Trans = _interopRequireWildcard(require("../Control.Monad.Writer.Trans/index.js"));

var Data_Either = _interopRequireWildcard(require("../Data.Either/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Functor_Compose = _interopRequireWildcard(require("../Data.Functor.Compose/index.js"));

var Data_Functor_Costar = _interopRequireWildcard(require("../Data.Functor.Costar/index.js"));

var Data_Maybe = _interopRequireWildcard(require("../Data.Maybe/index.js"));

var Data_Profunctor_Star = _interopRequireWildcard(require("../Data.Profunctor.Star/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Effect_Class = _interopRequireWildcard(require("../Effect.Class/index.js"));

var Effect_Ref = _interopRequireWildcard(require("../Effect.Ref/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var discard =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit);

var ParCont = function ParCont(x) {
  return x;
};

exports.ParCont = ParCont;

var sequential = function sequential(dict) {
  return dict.sequential;
};

exports.sequential = sequential;

var parallel = function parallel(dict) {
  return dict.parallel;
};

exports.parallel = parallel;
var newtypeParCont = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeParCont = newtypeParCont;

var monadParWriterT = function monadParWriterT(dictMonoid) {
  var applyWriterT = Control_Monad_Writer_Trans.applyWriterT(dictMonoid.Semigroup0());
  return function (dictParallel) {
    var applyWriterT1 = applyWriterT(dictParallel.Apply0());
    var applyWriterT2 = applyWriterT(dictParallel.Apply1());
    return {
      parallel: Control_Monad_Writer_Trans.mapWriterT(parallel(dictParallel)),
      sequential: Control_Monad_Writer_Trans.mapWriterT(sequential(dictParallel)),
      Apply0: function Apply0() {
        return applyWriterT1;
      },
      Apply1: function Apply1() {
        return applyWriterT2;
      }
    };
  };
};

exports.monadParWriterT = monadParWriterT;

var monadParStar = function monadParStar(dictParallel) {
  var parallel1 = parallel(dictParallel);
  var sequential1 = sequential(dictParallel);
  var applyStar = Data_Profunctor_Star.applyStar(dictParallel.Apply0());
  var applyStar1 = Data_Profunctor_Star.applyStar(dictParallel.Apply1());
  return {
    parallel: function parallel(v) {
      return function ($124) {
        return parallel1(v($124));
      };
    },
    sequential: function sequential(v) {
      return function ($125) {
        return sequential1(v($125));
      };
    },
    Apply0: function Apply0() {
      return applyStar;
    },
    Apply1: function Apply1() {
      return applyStar1;
    }
  };
};

exports.monadParStar = monadParStar;

var monadParReaderT = function monadParReaderT(dictParallel) {
  var applyReaderT = Control_Monad_Reader_Trans.applyReaderT(dictParallel.Apply0());
  var applyReaderT1 = Control_Monad_Reader_Trans.applyReaderT(dictParallel.Apply1());
  return {
    parallel: Control_Monad_Reader_Trans.mapReaderT(parallel(dictParallel)),
    sequential: Control_Monad_Reader_Trans.mapReaderT(sequential(dictParallel)),
    Apply0: function Apply0() {
      return applyReaderT;
    },
    Apply1: function Apply1() {
      return applyReaderT1;
    }
  };
};

exports.monadParReaderT = monadParReaderT;

var monadParMaybeT = function monadParMaybeT(dictParallel) {
  var parallel1 = parallel(dictParallel);
  var sequential1 = sequential(dictParallel);
  var applyCompose = Data_Functor_Compose.applyCompose(dictParallel.Apply1())(Data_Maybe.applyMaybe);
  return function (dictMonad) {
    var applyMaybeT = Control_Monad_Maybe_Trans.applyMaybeT(dictMonad);
    return {
      parallel: function parallel(v) {
        return parallel1(v);
      },
      sequential: function sequential(v) {
        return sequential1(v);
      },
      Apply0: function Apply0() {
        return applyMaybeT;
      },
      Apply1: function Apply1() {
        return applyCompose;
      }
    };
  };
};

exports.monadParMaybeT = monadParMaybeT;

var monadParExceptT = function monadParExceptT(dictParallel) {
  var parallel1 = parallel(dictParallel);
  var sequential1 = sequential(dictParallel);
  var applyCompose = Data_Functor_Compose.applyCompose(dictParallel.Apply1())(Data_Either.applyEither);
  return function (dictMonad) {
    var applyExceptT = Control_Monad_Except_Trans.applyExceptT(dictMonad);
    return {
      parallel: function parallel(v) {
        return parallel1(v);
      },
      sequential: function sequential(v) {
        return sequential1(v);
      },
      Apply0: function Apply0() {
        return applyExceptT;
      },
      Apply1: function Apply1() {
        return applyCompose;
      }
    };
  };
};

exports.monadParExceptT = monadParExceptT;

var monadParCostar = function monadParCostar(dictParallel) {
  var sequential1 = sequential(dictParallel);
  var parallel1 = parallel(dictParallel);
  return {
    parallel: function parallel(v) {
      return function ($126) {
        return v(sequential1($126));
      };
    },
    sequential: function sequential(v) {
      return function ($127) {
        return v(parallel1($127));
      };
    },
    Apply0: function Apply0() {
      return Data_Functor_Costar.applyCostar;
    },
    Apply1: function Apply1() {
      return Data_Functor_Costar.applyCostar;
    }
  };
};

exports.monadParCostar = monadParCostar;

var monadParParCont = function monadParParCont(dictMonadEffect) {
  var applyContT = Control_Monad_Cont_Trans.applyContT(dictMonadEffect.Monad0().Bind1().Apply0());
  return {
    parallel: ParCont,
    sequential: function sequential(v) {
      return v;
    },
    Apply0: function Apply0() {
      return applyContT;
    },
    Apply1: function Apply1() {
      return applyParCont(dictMonadEffect);
    }
  };
};

exports.monadParParCont = monadParParCont;

var functorParCont = function functorParCont(dictMonadEffect) {
  var _map = Data_Functor.map(Control_Monad_Cont_Trans.functorContT(dictMonadEffect.Monad0().Bind1().Apply0().Functor0()));

  return {
    map: function map(f) {
      var $128 = parallel(monadParParCont(dictMonadEffect));

      var $129 = _map(f);

      var $130 = sequential(monadParParCont(dictMonadEffect));
      return function ($131) {
        return $128($129($130($131)));
      };
    }
  };
};

exports.functorParCont = functorParCont;

var applyParCont = function applyParCont(dictMonadEffect) {
  var Bind1 = dictMonadEffect.Monad0().Bind1();
  var bind = Control_Bind.bind(Bind1);
  var liftEffect = Effect_Class.liftEffect(dictMonadEffect);
  var discard1 = discard(Bind1);
  return {
    apply: function apply(v) {
      return function (v1) {
        return function (k) {
          return bind(liftEffect(Effect_Ref["new"](Data_Maybe.Nothing.value)))(function (ra) {
            return bind(liftEffect(Effect_Ref["new"](Data_Maybe.Nothing.value)))(function (rb) {
              return discard1(Control_Monad_Cont_Trans.runContT(v)(function (a) {
                return bind(liftEffect(Effect_Ref.read(rb)))(function (mb) {
                  if (mb instanceof Data_Maybe.Nothing) {
                    return liftEffect(Effect_Ref.write(new Data_Maybe.Just(a))(ra));
                  }

                  ;

                  if (mb instanceof Data_Maybe.Just) {
                    return k(a(mb.value0));
                  }

                  ;
                  throw new Error("Failed pattern match at Control.Parallel.Class (line 83, column 7 - line 85, column 26): " + [mb.constructor.name]);
                });
              }))(function () {
                return Control_Monad_Cont_Trans.runContT(v1)(function (b) {
                  return bind(liftEffect(Effect_Ref.read(ra)))(function (ma) {
                    if (ma instanceof Data_Maybe.Nothing) {
                      return liftEffect(Effect_Ref.write(new Data_Maybe.Just(b))(rb));
                    }

                    ;

                    if (ma instanceof Data_Maybe.Just) {
                      return k(ma.value0(b));
                    }

                    ;
                    throw new Error("Failed pattern match at Control.Parallel.Class (line 89, column 7 - line 91, column 26): " + [ma.constructor.name]);
                  });
                });
              });
            });
          });
        };
      };
    },
    Functor0: function Functor0() {
      return functorParCont(dictMonadEffect);
    }
  };
};

exports.applyParCont = applyParCont;

var applicativeParCont = function applicativeParCont(dictMonadEffect) {
  var applyParCont1 = applyParCont(dictMonadEffect);
  return {
    pure: function () {
      var $132 = parallel(monadParParCont(dictMonadEffect));
      var $133 = Control_Applicative.pure(Control_Monad_Cont_Trans.applicativeContT(dictMonadEffect.Monad0().Applicative0()));
      return function ($134) {
        return $132($133($134));
      };
    }(),
    Apply0: function Apply0() {
      return applyParCont1;
    }
  };
};

exports.applicativeParCont = applicativeParCont;

var altParCont = function altParCont(dictMonadEffect) {
  var Monad0 = dictMonadEffect.Monad0();
  var Bind1 = Monad0.Bind1();
  var bind = Control_Bind.bind(Bind1);
  var liftEffect = Effect_Class.liftEffect(dictMonadEffect);
  var discard1 = discard(Bind1);
  var pure = Control_Applicative.pure(Monad0.Applicative0());
  var functorParCont1 = functorParCont(dictMonadEffect);
  return {
    alt: function alt(v) {
      return function (v1) {
        return function (k) {
          return bind(liftEffect(Effect_Ref["new"](false)))(function (done) {
            return discard1(Control_Monad_Cont_Trans.runContT(v)(function (a) {
              return bind(liftEffect(Effect_Ref.read(done)))(function (b) {
                if (b) {
                  return pure(Data_Unit.unit);
                }

                ;
                return discard1(liftEffect(Effect_Ref.write(true)(done)))(function () {
                  return k(a);
                });
              });
            }))(function () {
              return Control_Monad_Cont_Trans.runContT(v1)(function (a) {
                return bind(liftEffect(Effect_Ref.read(done)))(function (b) {
                  if (b) {
                    return pure(Data_Unit.unit);
                  }

                  ;
                  return discard1(liftEffect(Effect_Ref.write(true)(done)))(function () {
                    return k(a);
                  });
                });
              });
            });
          });
        };
      };
    },
    Functor0: function Functor0() {
      return functorParCont1;
    }
  };
};

exports.altParCont = altParCont;

var plusParCont = function plusParCont(dictMonadEffect) {
  var pure = Control_Applicative.pure(dictMonadEffect.Monad0().Applicative0());
  var altParCont1 = altParCont(dictMonadEffect);
  return {
    empty: function empty(v) {
      return pure(Data_Unit.unit);
    },
    Alt0: function Alt0() {
      return altParCont1;
    }
  };
};

exports.plusParCont = plusParCont;

var alternativeParCont = function alternativeParCont(dictMonadEffect) {
  var applicativeParCont1 = applicativeParCont(dictMonadEffect);
  var plusParCont1 = plusParCont(dictMonadEffect);
  return {
    Applicative0: function Applicative0() {
      return applicativeParCont1;
    },
    Plus1: function Plus1() {
      return plusParCont1;
    }
  };
};

exports.alternativeParCont = alternativeParCont;
},{"../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Monad.Cont.Trans/index.js":"../output/Control.Monad.Cont.Trans/index.js","../Control.Monad.Except.Trans/index.js":"../output/Control.Monad.Except.Trans/index.js","../Control.Monad.Maybe.Trans/index.js":"../output/Control.Monad.Maybe.Trans/index.js","../Control.Monad.Reader.Trans/index.js":"../output/Control.Monad.Reader.Trans/index.js","../Control.Monad.Writer.Trans/index.js":"../output/Control.Monad.Writer.Trans/index.js","../Data.Either/index.js":"../output/Data.Either/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Functor.Compose/index.js":"../output/Data.Functor.Compose/index.js","../Data.Functor.Costar/index.js":"../output/Data.Functor.Costar/index.js","../Data.Maybe/index.js":"../output/Data.Maybe/index.js","../Data.Profunctor.Star/index.js":"../output/Data.Profunctor.Star/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Effect.Class/index.js":"../output/Effect.Class/index.js","../Effect.Ref/index.js":"../output/Effect.Ref/index.js"}],"../output/Data.Foldable/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.foldrArray = exports.foldlArray = void 0;

var foldrArray = function foldrArray(f) {
  return function (init) {
    return function (xs) {
      var acc = init;
      var len = xs.length;

      for (var i = len - 1; i >= 0; i--) {
        acc = f(xs[i])(acc);
      }

      return acc;
    };
  };
};

exports.foldrArray = foldrArray;

var foldlArray = function foldlArray(f) {
  return function (init) {
    return function (xs) {
      var acc = init;
      var len = xs.length;

      for (var i = 0; i < len; i++) {
        acc = f(acc)(xs[i]);
      }

      return acc;
    };
  };
};

exports.foldlArray = foldlArray;
},{}],"../output/Data.Bifunctor/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.rmap = exports.lmap = exports.bimap = exports.bifunctorTuple = exports.bifunctorEither = exports.bifunctorConst = void 0;

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Data_Either = _interopRequireWildcard(require("../Data.Either/index.js"));

var Data_Tuple = _interopRequireWildcard(require("../Data.Tuple/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var identity =
/* #__PURE__ */
Control_Category.identity(Control_Category.categoryFn);

var bimap = function bimap(dict) {
  return dict.bimap;
};

exports.bimap = bimap;

var lmap = function lmap(dictBifunctor) {
  var bimap1 = bimap(dictBifunctor);
  return function (f) {
    return bimap1(f)(identity);
  };
};

exports.lmap = lmap;

var rmap = function rmap(dictBifunctor) {
  return bimap(dictBifunctor)(identity);
};

exports.rmap = rmap;
var bifunctorTuple = {
  bimap: function bimap(f) {
    return function (g) {
      return function (v) {
        return new Data_Tuple.Tuple(f(v.value0), g(v.value1));
      };
    };
  }
};
exports.bifunctorTuple = bifunctorTuple;
var bifunctorEither = {
  bimap: function bimap(v) {
    return function (v1) {
      return function (v2) {
        if (v2 instanceof Data_Either.Left) {
          return new Data_Either.Left(v(v2.value0));
        }

        ;

        if (v2 instanceof Data_Either.Right) {
          return new Data_Either.Right(v1(v2.value0));
        }

        ;
        throw new Error("Failed pattern match at Data.Bifunctor (line 32, column 1 - line 34, column 36): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  }
};
exports.bifunctorEither = bifunctorEither;
var bifunctorConst = {
  bimap: function bimap(f) {
    return function (v) {
      return function (v1) {
        return f(v1);
      };
    };
  }
};
exports.bifunctorConst = bifunctorConst;
},{"../Control.Category/index.js":"../output/Control.Category/index.js","../Data.Either/index.js":"../output/Data.Either/index.js","../Data.Tuple/index.js":"../output/Data.Tuple/index.js"}],"../output/Data.Functor.Coproduct/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showCoproduct = exports.right = exports.ordCoproduct = exports.ord1Coproduct = exports.newtypeCoproduct = exports.left = exports.functorCoproduct = exports.extendCoproduct = exports.eqCoproduct = exports.eq1Coproduct = exports.coproduct = exports.comonadCoproduct = exports.bihoistCoproduct = exports.Coproduct = void 0;

var Control_Comonad = _interopRequireWildcard(require("../Control.Comonad/index.js"));

var Control_Extend = _interopRequireWildcard(require("../Control.Extend/index.js"));

var Data_Bifunctor = _interopRequireWildcard(require("../Data.Bifunctor/index.js"));

var Data_Either = _interopRequireWildcard(require("../Data.Either/index.js"));

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Ordering = _interopRequireWildcard(require("../Data.Ordering/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var bimap =
/* #__PURE__ */
Data_Bifunctor.bimap(Data_Bifunctor.bifunctorEither);

var Coproduct = function Coproduct(x) {
  return x;
};

exports.Coproduct = Coproduct;

var showCoproduct = function showCoproduct(dictShow) {
  var _show = Data_Show.show(dictShow);

  return function (dictShow1) {
    var show1 = Data_Show.show(dictShow1);
    return {
      show: function show(v) {
        if (v instanceof Data_Either.Left) {
          return "(left " + (_show(v.value0) + ")");
        }

        ;

        if (v instanceof Data_Either.Right) {
          return "(right " + (show1(v.value0) + ")");
        }

        ;
        throw new Error("Failed pattern match at Data.Functor.Coproduct (line 63, column 1 - line 65, column 60): " + [v.constructor.name]);
      }
    };
  };
};

exports.showCoproduct = showCoproduct;

var right = function right(ga) {
  return new Data_Either.Right(ga);
};

exports.right = right;
var newtypeCoproduct = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeCoproduct = newtypeCoproduct;

var left = function left(fa) {
  return new Data_Either.Left(fa);
};

exports.left = left;

var functorCoproduct = function functorCoproduct(dictFunctor) {
  var _map = Data_Functor.map(dictFunctor);

  return function (dictFunctor1) {
    var map1 = Data_Functor.map(dictFunctor1);
    return {
      map: function map(f) {
        return function (v) {
          return bimap(_map(f))(map1(f))(v);
        };
      }
    };
  };
};

exports.functorCoproduct = functorCoproduct;

var eq1Coproduct = function eq1Coproduct(dictEq1) {
  var _eq = Data_Eq.eq1(dictEq1);

  return function (dictEq11) {
    var eq11 = Data_Eq.eq1(dictEq11);
    return {
      eq1: function eq1(dictEq) {
        var eq12 = _eq(dictEq);

        var eq13 = eq11(dictEq);
        return function (v) {
          return function (v1) {
            if (v instanceof Data_Either.Left && v1 instanceof Data_Either.Left) {
              return eq12(v.value0)(v1.value0);
            }

            ;

            if (v instanceof Data_Either.Right && v1 instanceof Data_Either.Right) {
              return eq13(v.value0)(v1.value0);
            }

            ;
            return false;
          };
        };
      }
    };
  };
};

exports.eq1Coproduct = eq1Coproduct;

var eqCoproduct = function eqCoproduct(dictEq1) {
  var eq1Coproduct1 = eq1Coproduct(dictEq1);
  return function (dictEq11) {
    var eq1 = Data_Eq.eq1(eq1Coproduct1(dictEq11));
    return function (dictEq) {
      return {
        eq: eq1(dictEq)
      };
    };
  };
};

exports.eqCoproduct = eqCoproduct;

var ord1Coproduct = function ord1Coproduct(dictOrd1) {
  var _compare = Data_Ord.compare1(dictOrd1);

  var eq1Coproduct1 = eq1Coproduct(dictOrd1.Eq10());
  return function (dictOrd11) {
    var compare11 = Data_Ord.compare1(dictOrd11);
    var eq1Coproduct2 = eq1Coproduct1(dictOrd11.Eq10());
    return {
      compare1: function compare1(dictOrd) {
        var compare12 = _compare(dictOrd);

        var compare13 = compare11(dictOrd);
        return function (v) {
          return function (v1) {
            if (v instanceof Data_Either.Left && v1 instanceof Data_Either.Left) {
              return compare12(v.value0)(v1.value0);
            }

            ;

            if (v instanceof Data_Either.Left) {
              return Data_Ordering.LT.value;
            }

            ;

            if (v1 instanceof Data_Either.Left) {
              return Data_Ordering.GT.value;
            }

            ;

            if (v instanceof Data_Either.Right && v1 instanceof Data_Either.Right) {
              return compare13(v.value0)(v1.value0);
            }

            ;
            throw new Error("Failed pattern match at Data.Functor.Coproduct (line 57, column 5 - line 61, column 43): " + [v.constructor.name, v1.constructor.name]);
          };
        };
      },
      Eq10: function Eq10() {
        return eq1Coproduct2;
      }
    };
  };
};

exports.ord1Coproduct = ord1Coproduct;

var ordCoproduct = function ordCoproduct(dictOrd1) {
  var ord1Coproduct1 = ord1Coproduct(dictOrd1);
  var eqCoproduct1 = eqCoproduct(dictOrd1.Eq10());
  return function (dictOrd11) {
    var compare1 = Data_Ord.compare1(ord1Coproduct1(dictOrd11));
    var eqCoproduct2 = eqCoproduct1(dictOrd11.Eq10());
    return function (dictOrd) {
      var eqCoproduct3 = eqCoproduct2(dictOrd.Eq0());
      return {
        compare: compare1(dictOrd),
        Eq0: function Eq0() {
          return eqCoproduct3;
        }
      };
    };
  };
};

exports.ordCoproduct = ordCoproduct;

var coproduct = function coproduct(v) {
  return function (v1) {
    return function (v2) {
      if (v2 instanceof Data_Either.Left) {
        return v(v2.value0);
      }

      ;

      if (v2 instanceof Data_Either.Right) {
        return v1(v2.value0);
      }

      ;
      throw new Error("Failed pattern match at Data.Functor.Coproduct (line 27, column 1 - line 27, column 78): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
    };
  };
};

exports.coproduct = coproduct;

var extendCoproduct = function extendCoproduct(dictExtend) {
  var _extend = Control_Extend.extend(dictExtend);

  var functorCoproduct1 = functorCoproduct(dictExtend.Functor0());
  return function (dictExtend1) {
    var extend1 = Control_Extend.extend(dictExtend1);
    var functorCoproduct2 = functorCoproduct1(dictExtend1.Functor0());
    return {
      extend: function extend(f) {
        var $106 = coproduct(function () {
          var $108 = _extend(function ($110) {
            return f(Coproduct(Data_Either.Left.create($110)));
          });

          return function ($109) {
            return Data_Either.Left.create($108($109));
          };
        }())(function () {
          var $111 = extend1(function ($113) {
            return f(Coproduct(Data_Either.Right.create($113)));
          });
          return function ($112) {
            return Data_Either.Right.create($111($112));
          };
        }());
        return function ($107) {
          return Coproduct($106($107));
        };
      },
      Functor0: function Functor0() {
        return functorCoproduct2;
      }
    };
  };
};

exports.extendCoproduct = extendCoproduct;

var comonadCoproduct = function comonadCoproduct(dictComonad) {
  var extract = Control_Comonad.extract(dictComonad);
  var extendCoproduct1 = extendCoproduct(dictComonad.Extend0());
  return function (dictComonad1) {
    var extendCoproduct2 = extendCoproduct1(dictComonad1.Extend0());
    return {
      extract: coproduct(extract)(Control_Comonad.extract(dictComonad1)),
      Extend0: function Extend0() {
        return extendCoproduct2;
      }
    };
  };
};

exports.comonadCoproduct = comonadCoproduct;

var bihoistCoproduct = function bihoistCoproduct(natF) {
  return function (natG) {
    return function (v) {
      return bimap(natF)(natG)(v);
    };
  };
};

exports.bihoistCoproduct = bihoistCoproduct;
},{"../Control.Comonad/index.js":"../output/Control.Comonad/index.js","../Control.Extend/index.js":"../output/Control.Extend/index.js","../Data.Bifunctor/index.js":"../output/Data.Bifunctor/index.js","../Data.Either/index.js":"../output/Data.Either/index.js","../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Ordering/index.js":"../output/Data.Ordering/index.js","../Data.Show/index.js":"../output/Data.Show/index.js"}],"../output/Data.Maybe.First/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showFirst = exports.semigroupFirst = exports.plusFirst = exports.ordFirst = exports.ord1First = exports.newtypeFirst = exports.monoidFirst = exports.monadFirst = exports.invariantFirst = exports.functorFirst = exports.extendFirst = exports.eqFirst = exports.eq1First = exports.boundedFirst = exports.bindFirst = exports.applyFirst = exports.applicativeFirst = exports.alternativeFirst = exports.altFirst = exports.First = void 0;

var Data_Maybe = _interopRequireWildcard(require("../Data.Maybe/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var First = function First(x) {
  return x;
};

exports.First = First;

var showFirst = function showFirst(dictShow) {
  var _show = Data_Show.show(Data_Maybe.showMaybe(dictShow));

  return {
    show: function show(v) {
      return "First (" + (_show(v) + ")");
    }
  };
};

exports.showFirst = showFirst;
var semigroupFirst = {
  append: function append(v) {
    return function (v1) {
      if (v instanceof Data_Maybe.Just) {
        return v;
      }

      ;
      return v1;
    };
  }
};
exports.semigroupFirst = semigroupFirst;

var ordFirst = function ordFirst(dictOrd) {
  return Data_Maybe.ordMaybe(dictOrd);
};

exports.ordFirst = ordFirst;
var ord1First = Data_Maybe.ord1Maybe;
exports.ord1First = ord1First;
var newtypeFirst = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeFirst = newtypeFirst;

var monoidFirst =
/* #__PURE__ */
function () {
  return {
    mempty: Data_Maybe.Nothing.value,
    Semigroup0: function Semigroup0() {
      return semigroupFirst;
    }
  };
}();

exports.monoidFirst = monoidFirst;
var monadFirst = Data_Maybe.monadMaybe;
exports.monadFirst = monadFirst;
var invariantFirst = Data_Maybe.invariantMaybe;
exports.invariantFirst = invariantFirst;
var functorFirst = Data_Maybe.functorMaybe;
exports.functorFirst = functorFirst;
var extendFirst = Data_Maybe.extendMaybe;
exports.extendFirst = extendFirst;

var eqFirst = function eqFirst(dictEq) {
  return Data_Maybe.eqMaybe(dictEq);
};

exports.eqFirst = eqFirst;
var eq1First = Data_Maybe.eq1Maybe;
exports.eq1First = eq1First;

var boundedFirst = function boundedFirst(dictBounded) {
  return Data_Maybe.boundedMaybe(dictBounded);
};

exports.boundedFirst = boundedFirst;
var bindFirst = Data_Maybe.bindMaybe;
exports.bindFirst = bindFirst;
var applyFirst = Data_Maybe.applyMaybe;
exports.applyFirst = applyFirst;
var applicativeFirst = Data_Maybe.applicativeMaybe;
exports.applicativeFirst = applicativeFirst;
var altFirst = {
  alt:
  /* #__PURE__ */
  Data_Semigroup.append(semigroupFirst),
  Functor0: function Functor0() {
    return functorFirst;
  }
};
exports.altFirst = altFirst;
var plusFirst = {
  empty:
  /* #__PURE__ */
  Data_Monoid.mempty(monoidFirst),
  Alt0: function Alt0() {
    return altFirst;
  }
};
exports.plusFirst = plusFirst;
var alternativeFirst = {
  Applicative0: function Applicative0() {
    return applicativeFirst;
  },
  Plus1: function Plus1() {
    return plusFirst;
  }
};
exports.alternativeFirst = alternativeFirst;
},{"../Data.Maybe/index.js":"../output/Data.Maybe/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Show/index.js":"../output/Data.Show/index.js"}],"../output/Data.Monoid.Conj/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showConj = exports.semiringConj = exports.semigroupConj = exports.ordConj = exports.ord1Conj = exports.monoidConj = exports.monadConj = exports.functorConj = exports.eqConj = exports.eq1Conj = exports.boundedConj = exports.bindConj = exports.applyConj = exports.applicativeConj = exports.Conj = void 0;

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_HeytingAlgebra = _interopRequireWildcard(require("../Data.HeytingAlgebra/index.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var Conj = function Conj(x) {
  return x;
};

exports.Conj = Conj;

var showConj = function showConj(dictShow) {
  var _show = Data_Show.show(dictShow);

  return {
    show: function show(v) {
      return "(Conj " + (_show(v) + ")");
    }
  };
};

exports.showConj = showConj;

var semiringConj = function semiringConj(dictHeytingAlgebra) {
  var conj = Data_HeytingAlgebra.conj(dictHeytingAlgebra);
  var disj = Data_HeytingAlgebra.disj(dictHeytingAlgebra);
  return {
    zero: Data_HeytingAlgebra.tt(dictHeytingAlgebra),
    one: Data_HeytingAlgebra.ff(dictHeytingAlgebra),
    add: function add(v) {
      return function (v1) {
        return conj(v)(v1);
      };
    },
    mul: function mul(v) {
      return function (v1) {
        return disj(v)(v1);
      };
    }
  };
};

exports.semiringConj = semiringConj;

var semigroupConj = function semigroupConj(dictHeytingAlgebra) {
  var conj = Data_HeytingAlgebra.conj(dictHeytingAlgebra);
  return {
    append: function append(v) {
      return function (v1) {
        return conj(v)(v1);
      };
    }
  };
};

exports.semigroupConj = semigroupConj;

var ordConj = function ordConj(dictOrd) {
  return dictOrd;
};

exports.ordConj = ordConj;

var monoidConj = function monoidConj(dictHeytingAlgebra) {
  var semigroupConj1 = semigroupConj(dictHeytingAlgebra);
  return {
    mempty: Data_HeytingAlgebra.tt(dictHeytingAlgebra),
    Semigroup0: function Semigroup0() {
      return semigroupConj1;
    }
  };
};

exports.monoidConj = monoidConj;
var functorConj = {
  map: function map(f) {
    return function (m) {
      return f(m);
    };
  }
};
exports.functorConj = functorConj;

var eqConj = function eqConj(dictEq) {
  return dictEq;
};

exports.eqConj = eqConj;
var eq1Conj = {
  eq1: function eq1(dictEq) {
    return Data_Eq.eq(eqConj(dictEq));
  }
};
exports.eq1Conj = eq1Conj;
var ord1Conj = {
  compare1: function compare1(dictOrd) {
    return Data_Ord.compare(ordConj(dictOrd));
  },
  Eq10: function Eq10() {
    return eq1Conj;
  }
};
exports.ord1Conj = ord1Conj;

var boundedConj = function boundedConj(dictBounded) {
  return dictBounded;
};

exports.boundedConj = boundedConj;
var applyConj = {
  apply: function apply(v) {
    return function (v1) {
      return v(v1);
    };
  },
  Functor0: function Functor0() {
    return functorConj;
  }
};
exports.applyConj = applyConj;
var bindConj = {
  bind: function bind(v) {
    return function (f) {
      return f(v);
    };
  },
  Apply0: function Apply0() {
    return applyConj;
  }
};
exports.bindConj = bindConj;
var applicativeConj = {
  pure: Conj,
  Apply0: function Apply0() {
    return applyConj;
  }
};
exports.applicativeConj = applicativeConj;
var monadConj = {
  Applicative0: function Applicative0() {
    return applicativeConj;
  },
  Bind1: function Bind1() {
    return bindConj;
  }
};
exports.monadConj = monadConj;
},{"../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.HeytingAlgebra/index.js":"../output/Data.HeytingAlgebra/index.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Show/index.js":"../output/Data.Show/index.js"}],"../output/Data.Monoid.Disj/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showDisj = exports.semiringDisj = exports.semigroupDisj = exports.ordDisj = exports.ord1Disj = exports.monoidDisj = exports.monadDisj = exports.functorDisj = exports.eqDisj = exports.eq1Disj = exports.boundedDisj = exports.bindDisj = exports.applyDisj = exports.applicativeDisj = exports.Disj = void 0;

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_HeytingAlgebra = _interopRequireWildcard(require("../Data.HeytingAlgebra/index.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var Disj = function Disj(x) {
  return x;
};

exports.Disj = Disj;

var showDisj = function showDisj(dictShow) {
  var _show = Data_Show.show(dictShow);

  return {
    show: function show(v) {
      return "(Disj " + (_show(v) + ")");
    }
  };
};

exports.showDisj = showDisj;

var semiringDisj = function semiringDisj(dictHeytingAlgebra) {
  var disj = Data_HeytingAlgebra.disj(dictHeytingAlgebra);
  var conj = Data_HeytingAlgebra.conj(dictHeytingAlgebra);
  return {
    zero: Data_HeytingAlgebra.ff(dictHeytingAlgebra),
    one: Data_HeytingAlgebra.tt(dictHeytingAlgebra),
    add: function add(v) {
      return function (v1) {
        return disj(v)(v1);
      };
    },
    mul: function mul(v) {
      return function (v1) {
        return conj(v)(v1);
      };
    }
  };
};

exports.semiringDisj = semiringDisj;

var semigroupDisj = function semigroupDisj(dictHeytingAlgebra) {
  var disj = Data_HeytingAlgebra.disj(dictHeytingAlgebra);
  return {
    append: function append(v) {
      return function (v1) {
        return disj(v)(v1);
      };
    }
  };
};

exports.semigroupDisj = semigroupDisj;

var ordDisj = function ordDisj(dictOrd) {
  return dictOrd;
};

exports.ordDisj = ordDisj;

var monoidDisj = function monoidDisj(dictHeytingAlgebra) {
  var semigroupDisj1 = semigroupDisj(dictHeytingAlgebra);
  return {
    mempty: Data_HeytingAlgebra.ff(dictHeytingAlgebra),
    Semigroup0: function Semigroup0() {
      return semigroupDisj1;
    }
  };
};

exports.monoidDisj = monoidDisj;
var functorDisj = {
  map: function map(f) {
    return function (m) {
      return f(m);
    };
  }
};
exports.functorDisj = functorDisj;

var eqDisj = function eqDisj(dictEq) {
  return dictEq;
};

exports.eqDisj = eqDisj;
var eq1Disj = {
  eq1: function eq1(dictEq) {
    return Data_Eq.eq(eqDisj(dictEq));
  }
};
exports.eq1Disj = eq1Disj;
var ord1Disj = {
  compare1: function compare1(dictOrd) {
    return Data_Ord.compare(ordDisj(dictOrd));
  },
  Eq10: function Eq10() {
    return eq1Disj;
  }
};
exports.ord1Disj = ord1Disj;

var boundedDisj = function boundedDisj(dictBounded) {
  return dictBounded;
};

exports.boundedDisj = boundedDisj;
var applyDisj = {
  apply: function apply(v) {
    return function (v1) {
      return v(v1);
    };
  },
  Functor0: function Functor0() {
    return functorDisj;
  }
};
exports.applyDisj = applyDisj;
var bindDisj = {
  bind: function bind(v) {
    return function (f) {
      return f(v);
    };
  },
  Apply0: function Apply0() {
    return applyDisj;
  }
};
exports.bindDisj = bindDisj;
var applicativeDisj = {
  pure: Disj,
  Apply0: function Apply0() {
    return applyDisj;
  }
};
exports.applicativeDisj = applicativeDisj;
var monadDisj = {
  Applicative0: function Applicative0() {
    return applicativeDisj;
  },
  Bind1: function Bind1() {
    return bindDisj;
  }
};
exports.monadDisj = monadDisj;
},{"../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.HeytingAlgebra/index.js":"../output/Data.HeytingAlgebra/index.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Show/index.js":"../output/Data.Show/index.js"}],"../output/Data.Monoid.Dual/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showDual = exports.semigroupDual = exports.ordDual = exports.ord1Dual = exports.monoidDual = exports.monadDual = exports.functorDual = exports.eqDual = exports.eq1Dual = exports.boundedDual = exports.bindDual = exports.applyDual = exports.applicativeDual = exports.Dual = void 0;

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var Dual = function Dual(x) {
  return x;
};

exports.Dual = Dual;

var showDual = function showDual(dictShow) {
  var _show = Data_Show.show(dictShow);

  return {
    show: function show(v) {
      return "(Dual " + (_show(v) + ")");
    }
  };
};

exports.showDual = showDual;

var semigroupDual = function semigroupDual(dictSemigroup) {
  var append1 = Data_Semigroup.append(dictSemigroup);
  return {
    append: function append(v) {
      return function (v1) {
        return append1(v1)(v);
      };
    }
  };
};

exports.semigroupDual = semigroupDual;

var ordDual = function ordDual(dictOrd) {
  return dictOrd;
};

exports.ordDual = ordDual;

var monoidDual = function monoidDual(dictMonoid) {
  var semigroupDual1 = semigroupDual(dictMonoid.Semigroup0());
  return {
    mempty: Data_Monoid.mempty(dictMonoid),
    Semigroup0: function Semigroup0() {
      return semigroupDual1;
    }
  };
};

exports.monoidDual = monoidDual;
var functorDual = {
  map: function map(f) {
    return function (m) {
      return f(m);
    };
  }
};
exports.functorDual = functorDual;

var eqDual = function eqDual(dictEq) {
  return dictEq;
};

exports.eqDual = eqDual;
var eq1Dual = {
  eq1: function eq1(dictEq) {
    return Data_Eq.eq(eqDual(dictEq));
  }
};
exports.eq1Dual = eq1Dual;
var ord1Dual = {
  compare1: function compare1(dictOrd) {
    return Data_Ord.compare(ordDual(dictOrd));
  },
  Eq10: function Eq10() {
    return eq1Dual;
  }
};
exports.ord1Dual = ord1Dual;

var boundedDual = function boundedDual(dictBounded) {
  return dictBounded;
};

exports.boundedDual = boundedDual;
var applyDual = {
  apply: function apply(v) {
    return function (v1) {
      return v(v1);
    };
  },
  Functor0: function Functor0() {
    return functorDual;
  }
};
exports.applyDual = applyDual;
var bindDual = {
  bind: function bind(v) {
    return function (f) {
      return f(v);
    };
  },
  Apply0: function Apply0() {
    return applyDual;
  }
};
exports.bindDual = bindDual;
var applicativeDual = {
  pure: Dual,
  Apply0: function Apply0() {
    return applyDual;
  }
};
exports.applicativeDual = applicativeDual;
var monadDual = {
  Applicative0: function Applicative0() {
    return applicativeDual;
  },
  Bind1: function Bind1() {
    return bindDual;
  }
};
exports.monadDual = monadDual;
},{"../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Show/index.js":"../output/Data.Show/index.js"}],"../output/Data.Monoid.Endo/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showEndo = exports.semigroupEndo = exports.ordEndo = exports.monoidEndo = exports.eqEndo = exports.boundedEndo = exports.Endo = void 0;

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Control_Semigroupoid = _interopRequireWildcard(require("../Control.Semigroupoid/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var Endo = function Endo(x) {
  return x;
};

exports.Endo = Endo;

var showEndo = function showEndo(dictShow) {
  var _show = Data_Show.show(dictShow);

  return {
    show: function show(v) {
      return "(Endo " + (_show(v) + ")");
    }
  };
};

exports.showEndo = showEndo;

var semigroupEndo = function semigroupEndo(dictSemigroupoid) {
  var compose = Control_Semigroupoid.compose(dictSemigroupoid);
  return {
    append: function append(v) {
      return function (v1) {
        return compose(v)(v1);
      };
    }
  };
};

exports.semigroupEndo = semigroupEndo;

var ordEndo = function ordEndo(dictOrd) {
  return dictOrd;
};

exports.ordEndo = ordEndo;

var monoidEndo = function monoidEndo(dictCategory) {
  var semigroupEndo1 = semigroupEndo(dictCategory.Semigroupoid0());
  return {
    mempty: Control_Category.identity(dictCategory),
    Semigroup0: function Semigroup0() {
      return semigroupEndo1;
    }
  };
};

exports.monoidEndo = monoidEndo;

var eqEndo = function eqEndo(dictEq) {
  return dictEq;
};

exports.eqEndo = eqEndo;

var boundedEndo = function boundedEndo(dictBounded) {
  return dictBounded;
};

exports.boundedEndo = boundedEndo;
},{"../Control.Category/index.js":"../output/Control.Category/index.js","../Control.Semigroupoid/index.js":"../output/Control.Semigroupoid/index.js","../Data.Show/index.js":"../output/Data.Show/index.js"}],"../output/Data.Foldable/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.traverse_ = exports.surroundMap = exports.surround = exports.sum = exports.sequence_ = exports.product = exports.or = exports.oneOfMap = exports.oneOf = exports.null = exports.notElem = exports.minimumBy = exports.minimum = exports.maximumBy = exports.maximum = exports.lookup = exports.length = exports.intercalate = exports.indexr = exports.indexl = exports.for_ = exports.foldrDefault = exports.foldr = exports.foldlDefault = exports.foldl = exports.foldableTuple = exports.foldableProduct = exports.foldableMultiplicative = exports.foldableMaybe = exports.foldableLast = exports.foldableIdentity = exports.foldableFirst = exports.foldableEither = exports.foldableDual = exports.foldableDisj = exports.foldableCoproduct = exports.foldableConst = exports.foldableConj = exports.foldableCompose = exports.foldableArray = exports.foldableApp = exports.foldableAdditive = exports.foldMapDefaultR = exports.foldMapDefaultL = exports.foldMap = exports.foldM = exports.fold = exports.findMap = exports.find = exports.elem = exports.any = exports.and = exports.all = void 0;

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Control_Alt = _interopRequireWildcard(require("../Control.Alt/index.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Control_Plus = _interopRequireWildcard(require("../Control.Plus/index.js"));

var Data_Either = _interopRequireWildcard(require("../Data.Either/index.js"));

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_Function = _interopRequireWildcard(require("../Data.Function/index.js"));

var Data_Functor_Coproduct = _interopRequireWildcard(require("../Data.Functor.Coproduct/index.js"));

var Data_HeytingAlgebra = _interopRequireWildcard(require("../Data.HeytingAlgebra/index.js"));

var Data_Maybe = _interopRequireWildcard(require("../Data.Maybe/index.js"));

var Data_Maybe_First = _interopRequireWildcard(require("../Data.Maybe.First/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Monoid_Conj = _interopRequireWildcard(require("../Data.Monoid.Conj/index.js"));

var Data_Monoid_Disj = _interopRequireWildcard(require("../Data.Monoid.Disj/index.js"));

var Data_Monoid_Dual = _interopRequireWildcard(require("../Data.Monoid.Dual/index.js"));

var Data_Monoid_Endo = _interopRequireWildcard(require("../Data.Monoid.Endo/index.js"));

var Data_Newtype = _interopRequireWildcard(require("../Data.Newtype/index.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Ordering = _interopRequireWildcard(require("../Data.Ordering/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Semiring = _interopRequireWildcard(require("../Data.Semiring/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var identity =
/* #__PURE__ */
Control_Category.identity(Control_Category.categoryFn);
var eq1 =
/* #__PURE__ */
Data_Eq.eq(Data_Ordering.eqOrdering);
var unwrap =
/* #__PURE__ */
Data_Newtype.unwrap();
var monoidEndo =
/* #__PURE__ */
Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn);
var monoidDual =
/* #__PURE__ */
Data_Monoid_Dual.monoidDual(monoidEndo);
var alaF =
/* #__PURE__ */
Data_Newtype.alaF()()()();

var foldr = function foldr(dict) {
  return dict.foldr;
};

exports.foldr = foldr;

var indexr = function indexr(dictFoldable) {
  var foldr2 = foldr(dictFoldable);
  return function (idx) {
    var go = function go(a) {
      return function (cursor) {
        if (cursor.elem instanceof Data_Maybe.Just) {
          return cursor;
        }

        ;
        var $292 = cursor.pos === idx;

        if ($292) {
          return {
            elem: new Data_Maybe.Just(a),
            pos: cursor.pos
          };
        }

        ;
        return {
          pos: cursor.pos + 1 | 0,
          elem: cursor.elem
        };
      };
    };

    var $451 = foldr2(go)({
      elem: Data_Maybe.Nothing.value,
      pos: 0
    });
    return function ($452) {
      return function (v) {
        return v.elem;
      }($451($452));
    };
  };
};

exports.indexr = indexr;

var $$null = function $$null(dictFoldable) {
  return foldr(dictFoldable)(function (v) {
    return function (v1) {
      return false;
    };
  })(true);
};

exports.null = $$null;

var oneOf = function oneOf(dictFoldable) {
  var foldr2 = foldr(dictFoldable);
  return function (dictPlus) {
    return foldr2(Control_Alt.alt(dictPlus.Alt0()))(Control_Plus.empty(dictPlus));
  };
};

exports.oneOf = oneOf;

var oneOfMap = function oneOfMap(dictFoldable) {
  var foldr2 = foldr(dictFoldable);
  return function (dictPlus) {
    var alt = Control_Alt.alt(dictPlus.Alt0());
    var empty = Control_Plus.empty(dictPlus);
    return function (f) {
      return foldr2(function ($453) {
        return alt(f($453));
      })(empty);
    };
  };
};

exports.oneOfMap = oneOfMap;

var traverse_ = function traverse_(dictApplicative) {
  var applySecond = Control_Apply.applySecond(dictApplicative.Apply0());
  var pure = Control_Applicative.pure(dictApplicative);
  return function (dictFoldable) {
    var foldr2 = foldr(dictFoldable);
    return function (f) {
      return foldr2(function ($454) {
        return applySecond(f($454));
      })(pure(Data_Unit.unit));
    };
  };
};

exports.traverse_ = traverse_;

var for_ = function for_(dictApplicative) {
  var traverse_1 = traverse_(dictApplicative);
  return function (dictFoldable) {
    return Data_Function.flip(traverse_1(dictFoldable));
  };
};

exports.for_ = for_;

var sequence_ = function sequence_(dictApplicative) {
  var traverse_1 = traverse_(dictApplicative);
  return function (dictFoldable) {
    return traverse_1(dictFoldable)(identity);
  };
};

exports.sequence_ = sequence_;

var foldl = function foldl(dict) {
  return dict.foldl;
};

exports.foldl = foldl;

var indexl = function indexl(dictFoldable) {
  var foldl2 = foldl(dictFoldable);
  return function (idx) {
    var go = function go(cursor) {
      return function (a) {
        if (cursor.elem instanceof Data_Maybe.Just) {
          return cursor;
        }

        ;
        var $296 = cursor.pos === idx;

        if ($296) {
          return {
            elem: new Data_Maybe.Just(a),
            pos: cursor.pos
          };
        }

        ;
        return {
          pos: cursor.pos + 1 | 0,
          elem: cursor.elem
        };
      };
    };

    var $455 = foldl2(go)({
      elem: Data_Maybe.Nothing.value,
      pos: 0
    });
    return function ($456) {
      return function (v) {
        return v.elem;
      }($455($456));
    };
  };
};

exports.indexl = indexl;

var intercalate = function intercalate(dictFoldable) {
  var foldl2 = foldl(dictFoldable);
  return function (dictMonoid) {
    var append = Data_Semigroup.append(dictMonoid.Semigroup0());
    var mempty = Data_Monoid.mempty(dictMonoid);
    return function (sep) {
      return function (xs) {
        var go = function go(v) {
          return function (v1) {
            if (v.init) {
              return {
                init: false,
                acc: v1
              };
            }

            ;
            return {
              init: false,
              acc: append(v.acc)(append(sep)(v1))
            };
          };
        };

        return foldl2(go)({
          init: true,
          acc: mempty
        })(xs).acc;
      };
    };
  };
};

exports.intercalate = intercalate;

var length = function length(dictFoldable) {
  var foldl2 = foldl(dictFoldable);
  return function (dictSemiring) {
    var add1 = Data_Semiring.add(dictSemiring);
    var one = Data_Semiring.one(dictSemiring);
    return foldl2(function (c) {
      return function (v) {
        return add1(one)(c);
      };
    })(Data_Semiring.zero(dictSemiring));
  };
};

exports.length = length;

var maximumBy = function maximumBy(dictFoldable) {
  var foldl2 = foldl(dictFoldable);
  return function (cmp) {
    var max$prime = function max$prime(v) {
      return function (v1) {
        if (v instanceof Data_Maybe.Nothing) {
          return new Data_Maybe.Just(v1);
        }

        ;

        if (v instanceof Data_Maybe.Just) {
          return new Data_Maybe.Just(function () {
            var $303 = eq1(cmp(v.value0)(v1))(Data_Ordering.GT.value);

            if ($303) {
              return v.value0;
            }

            ;
            return v1;
          }());
        }

        ;
        throw new Error("Failed pattern match at Data.Foldable (line 441, column 3 - line 441, column 27): " + [v.constructor.name, v1.constructor.name]);
      };
    };

    return foldl2(max$prime)(Data_Maybe.Nothing.value);
  };
};

exports.maximumBy = maximumBy;

var maximum = function maximum(dictOrd) {
  var compare = Data_Ord.compare(dictOrd);
  return function (dictFoldable) {
    return maximumBy(dictFoldable)(compare);
  };
};

exports.maximum = maximum;

var minimumBy = function minimumBy(dictFoldable) {
  var foldl2 = foldl(dictFoldable);
  return function (cmp) {
    var min$prime = function min$prime(v) {
      return function (v1) {
        if (v instanceof Data_Maybe.Nothing) {
          return new Data_Maybe.Just(v1);
        }

        ;

        if (v instanceof Data_Maybe.Just) {
          return new Data_Maybe.Just(function () {
            var $307 = eq1(cmp(v.value0)(v1))(Data_Ordering.LT.value);

            if ($307) {
              return v.value0;
            }

            ;
            return v1;
          }());
        }

        ;
        throw new Error("Failed pattern match at Data.Foldable (line 454, column 3 - line 454, column 27): " + [v.constructor.name, v1.constructor.name]);
      };
    };

    return foldl2(min$prime)(Data_Maybe.Nothing.value);
  };
};

exports.minimumBy = minimumBy;

var minimum = function minimum(dictOrd) {
  var compare = Data_Ord.compare(dictOrd);
  return function (dictFoldable) {
    return minimumBy(dictFoldable)(compare);
  };
};

exports.minimum = minimum;

var product = function product(dictFoldable) {
  var foldl2 = foldl(dictFoldable);
  return function (dictSemiring) {
    return foldl2(Data_Semiring.mul(dictSemiring))(Data_Semiring.one(dictSemiring));
  };
};

exports.product = product;

var sum = function sum(dictFoldable) {
  var foldl2 = foldl(dictFoldable);
  return function (dictSemiring) {
    return foldl2(Data_Semiring.add(dictSemiring))(Data_Semiring.zero(dictSemiring));
  };
};

exports.sum = sum;
var foldableTuple = {
  foldr: function foldr(f) {
    return function (z) {
      return function (v) {
        return f(v.value1)(z);
      };
    };
  },
  foldl: function foldl(f) {
    return function (z) {
      return function (v) {
        return f(z)(v.value1);
      };
    };
  },
  foldMap: function foldMap(dictMonoid) {
    return function (f) {
      return function (v) {
        return f(v.value1);
      };
    };
  }
};
exports.foldableTuple = foldableTuple;
var foldableMultiplicative = {
  foldr: function foldr(f) {
    return function (z) {
      return function (v) {
        return f(v)(z);
      };
    };
  },
  foldl: function foldl(f) {
    return function (z) {
      return function (v) {
        return f(z)(v);
      };
    };
  },
  foldMap: function foldMap(dictMonoid) {
    return function (f) {
      return function (v) {
        return f(v);
      };
    };
  }
};
exports.foldableMultiplicative = foldableMultiplicative;
var foldableMaybe = {
  foldr: function foldr(v) {
    return function (v1) {
      return function (v2) {
        if (v2 instanceof Data_Maybe.Nothing) {
          return v1;
        }

        ;

        if (v2 instanceof Data_Maybe.Just) {
          return v(v2.value0)(v1);
        }

        ;
        throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  },
  foldl: function foldl(v) {
    return function (v1) {
      return function (v2) {
        if (v2 instanceof Data_Maybe.Nothing) {
          return v1;
        }

        ;

        if (v2 instanceof Data_Maybe.Just) {
          return v(v1)(v2.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  },
  foldMap: function foldMap(dictMonoid) {
    var mempty = Data_Monoid.mempty(dictMonoid);
    return function (v) {
      return function (v1) {
        if (v1 instanceof Data_Maybe.Nothing) {
          return mempty;
        }

        ;

        if (v1 instanceof Data_Maybe.Just) {
          return v(v1.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  }
};
exports.foldableMaybe = foldableMaybe;
var foldr1 =
/* #__PURE__ */
foldr(foldableMaybe);
var foldl1 =
/* #__PURE__ */
foldl(foldableMaybe);
var foldableIdentity = {
  foldr: function foldr(f) {
    return function (z) {
      return function (v) {
        return f(v)(z);
      };
    };
  },
  foldl: function foldl(f) {
    return function (z) {
      return function (v) {
        return f(z)(v);
      };
    };
  },
  foldMap: function foldMap(dictMonoid) {
    return function (f) {
      return function (v) {
        return f(v);
      };
    };
  }
};
exports.foldableIdentity = foldableIdentity;
var foldableEither = {
  foldr: function foldr(v) {
    return function (v1) {
      return function (v2) {
        if (v2 instanceof Data_Either.Left) {
          return v1;
        }

        ;

        if (v2 instanceof Data_Either.Right) {
          return v(v2.value0)(v1);
        }

        ;
        throw new Error("Failed pattern match at Data.Foldable (line 181, column 1 - line 187, column 28): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  },
  foldl: function foldl(v) {
    return function (v1) {
      return function (v2) {
        if (v2 instanceof Data_Either.Left) {
          return v1;
        }

        ;

        if (v2 instanceof Data_Either.Right) {
          return v(v1)(v2.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.Foldable (line 181, column 1 - line 187, column 28): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  },
  foldMap: function foldMap(dictMonoid) {
    var mempty = Data_Monoid.mempty(dictMonoid);
    return function (v) {
      return function (v1) {
        if (v1 instanceof Data_Either.Left) {
          return mempty;
        }

        ;

        if (v1 instanceof Data_Either.Right) {
          return v(v1.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.Foldable (line 181, column 1 - line 187, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  }
};
exports.foldableEither = foldableEither;
var foldableDual = {
  foldr: function foldr(f) {
    return function (z) {
      return function (v) {
        return f(v)(z);
      };
    };
  },
  foldl: function foldl(f) {
    return function (z) {
      return function (v) {
        return f(z)(v);
      };
    };
  },
  foldMap: function foldMap(dictMonoid) {
    return function (f) {
      return function (v) {
        return f(v);
      };
    };
  }
};
exports.foldableDual = foldableDual;
var foldableDisj = {
  foldr: function foldr(f) {
    return function (z) {
      return function (v) {
        return f(v)(z);
      };
    };
  },
  foldl: function foldl(f) {
    return function (z) {
      return function (v) {
        return f(z)(v);
      };
    };
  },
  foldMap: function foldMap(dictMonoid) {
    return function (f) {
      return function (v) {
        return f(v);
      };
    };
  }
};
exports.foldableDisj = foldableDisj;
var foldableConst = {
  foldr: function foldr(v) {
    return function (z) {
      return function (v1) {
        return z;
      };
    };
  },
  foldl: function foldl(v) {
    return function (z) {
      return function (v1) {
        return z;
      };
    };
  },
  foldMap: function foldMap(dictMonoid) {
    var mempty = Data_Monoid.mempty(dictMonoid);
    return function (v) {
      return function (v1) {
        return mempty;
      };
    };
  }
};
exports.foldableConst = foldableConst;
var foldableConj = {
  foldr: function foldr(f) {
    return function (z) {
      return function (v) {
        return f(v)(z);
      };
    };
  },
  foldl: function foldl(f) {
    return function (z) {
      return function (v) {
        return f(z)(v);
      };
    };
  },
  foldMap: function foldMap(dictMonoid) {
    return function (f) {
      return function (v) {
        return f(v);
      };
    };
  }
};
exports.foldableConj = foldableConj;
var foldableAdditive = {
  foldr: function foldr(f) {
    return function (z) {
      return function (v) {
        return f(v)(z);
      };
    };
  },
  foldl: function foldl(f) {
    return function (z) {
      return function (v) {
        return f(z)(v);
      };
    };
  },
  foldMap: function foldMap(dictMonoid) {
    return function (f) {
      return function (v) {
        return f(v);
      };
    };
  }
};
exports.foldableAdditive = foldableAdditive;

var foldMapDefaultR = function foldMapDefaultR(dictFoldable) {
  var foldr2 = foldr(dictFoldable);
  return function (dictMonoid) {
    var append = Data_Semigroup.append(dictMonoid.Semigroup0());
    var mempty = Data_Monoid.mempty(dictMonoid);
    return function (f) {
      return foldr2(function (x) {
        return function (acc) {
          return append(f(x))(acc);
        };
      })(mempty);
    };
  };
};

exports.foldMapDefaultR = foldMapDefaultR;
var foldableArray = {
  foldr: $foreign.foldrArray,
  foldl: $foreign.foldlArray,
  foldMap: function foldMap(dictMonoid) {
    return foldMapDefaultR(foldableArray)(dictMonoid);
  }
};
exports.foldableArray = foldableArray;

var foldMapDefaultL = function foldMapDefaultL(dictFoldable) {
  var foldl2 = foldl(dictFoldable);
  return function (dictMonoid) {
    var append = Data_Semigroup.append(dictMonoid.Semigroup0());
    var mempty = Data_Monoid.mempty(dictMonoid);
    return function (f) {
      return foldl2(function (acc) {
        return function (x) {
          return append(acc)(f(x));
        };
      })(mempty);
    };
  };
};

exports.foldMapDefaultL = foldMapDefaultL;

var foldMap = function foldMap(dict) {
  return dict.foldMap;
};

exports.foldMap = foldMap;
var foldMap1 =
/* #__PURE__ */
foldMap(foldableMaybe);

var foldableApp = function foldableApp(dictFoldable) {
  var foldr2 = foldr(dictFoldable);
  var foldl2 = foldl(dictFoldable);
  var foldMap2 = foldMap(dictFoldable);
  return {
    foldr: function foldr(f) {
      return function (i) {
        return function (v) {
          return foldr2(f)(i)(v);
        };
      };
    },
    foldl: function foldl(f) {
      return function (i) {
        return function (v) {
          return foldl2(f)(i)(v);
        };
      };
    },
    foldMap: function foldMap(dictMonoid) {
      var foldMap3 = foldMap2(dictMonoid);
      return function (f) {
        return function (v) {
          return foldMap3(f)(v);
        };
      };
    }
  };
};

exports.foldableApp = foldableApp;

var foldableCompose = function foldableCompose(dictFoldable) {
  var foldr2 = foldr(dictFoldable);
  var foldl2 = foldl(dictFoldable);
  var foldMap2 = foldMap(dictFoldable);
  return function (dictFoldable1) {
    var foldr3 = foldr(dictFoldable1);
    var foldl3 = foldl(dictFoldable1);
    var foldMap3 = foldMap(dictFoldable1);
    return {
      foldr: function foldr(f) {
        return function (i) {
          return function (v) {
            return foldr2(Data_Function.flip(foldr3(f)))(i)(v);
          };
        };
      },
      foldl: function foldl(f) {
        return function (i) {
          return function (v) {
            return foldl2(foldl3(f))(i)(v);
          };
        };
      },
      foldMap: function foldMap(dictMonoid) {
        var foldMap4 = foldMap2(dictMonoid);
        var foldMap5 = foldMap3(dictMonoid);
        return function (f) {
          return function (v) {
            return foldMap4(foldMap5(f))(v);
          };
        };
      }
    };
  };
};

exports.foldableCompose = foldableCompose;

var foldableCoproduct = function foldableCoproduct(dictFoldable) {
  var foldr2 = foldr(dictFoldable);
  var foldl2 = foldl(dictFoldable);
  var foldMap2 = foldMap(dictFoldable);
  return function (dictFoldable1) {
    var foldr3 = foldr(dictFoldable1);
    var foldl3 = foldl(dictFoldable1);
    var foldMap3 = foldMap(dictFoldable1);
    return {
      foldr: function foldr(f) {
        return function (z) {
          return Data_Functor_Coproduct.coproduct(foldr2(f)(z))(foldr3(f)(z));
        };
      },
      foldl: function foldl(f) {
        return function (z) {
          return Data_Functor_Coproduct.coproduct(foldl2(f)(z))(foldl3(f)(z));
        };
      },
      foldMap: function foldMap(dictMonoid) {
        var foldMap4 = foldMap2(dictMonoid);
        var foldMap5 = foldMap3(dictMonoid);
        return function (f) {
          return Data_Functor_Coproduct.coproduct(foldMap4(f))(foldMap5(f));
        };
      }
    };
  };
};

exports.foldableCoproduct = foldableCoproduct;
var foldableFirst = {
  foldr: function foldr(f) {
    return function (z) {
      return function (v) {
        return foldr1(f)(z)(v);
      };
    };
  },
  foldl: function foldl(f) {
    return function (z) {
      return function (v) {
        return foldl1(f)(z)(v);
      };
    };
  },
  foldMap: function foldMap(dictMonoid) {
    var foldMap2 = foldMap1(dictMonoid);
    return function (f) {
      return function (v) {
        return foldMap2(f)(v);
      };
    };
  }
};
exports.foldableFirst = foldableFirst;
var foldableLast = {
  foldr: function foldr(f) {
    return function (z) {
      return function (v) {
        return foldr1(f)(z)(v);
      };
    };
  },
  foldl: function foldl(f) {
    return function (z) {
      return function (v) {
        return foldl1(f)(z)(v);
      };
    };
  },
  foldMap: function foldMap(dictMonoid) {
    var foldMap2 = foldMap1(dictMonoid);
    return function (f) {
      return function (v) {
        return foldMap2(f)(v);
      };
    };
  }
};
exports.foldableLast = foldableLast;

var foldableProduct = function foldableProduct(dictFoldable) {
  var foldr2 = foldr(dictFoldable);
  var foldl2 = foldl(dictFoldable);
  var foldMap2 = foldMap(dictFoldable);
  return function (dictFoldable1) {
    var foldr3 = foldr(dictFoldable1);
    var foldl3 = foldl(dictFoldable1);
    var foldMap3 = foldMap(dictFoldable1);
    return {
      foldr: function foldr(f) {
        return function (z) {
          return function (v) {
            return foldr2(f)(foldr3(f)(z)(v.value1))(v.value0);
          };
        };
      },
      foldl: function foldl(f) {
        return function (z) {
          return function (v) {
            return foldl3(f)(foldl2(f)(z)(v.value0))(v.value1);
          };
        };
      },
      foldMap: function foldMap(dictMonoid) {
        var append = Data_Semigroup.append(dictMonoid.Semigroup0());
        var foldMap4 = foldMap2(dictMonoid);
        var foldMap5 = foldMap3(dictMonoid);
        return function (f) {
          return function (v) {
            return append(foldMap4(f)(v.value0))(foldMap5(f)(v.value1));
          };
        };
      }
    };
  };
};

exports.foldableProduct = foldableProduct;

var foldlDefault = function foldlDefault(dictFoldable) {
  var foldMap2 = foldMap(dictFoldable)(monoidDual);
  return function (c) {
    return function (u) {
      return function (xs) {
        return unwrap(unwrap(foldMap2(function () {
          var $457 = Data_Function.flip(c);
          return function ($458) {
            return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo($457($458)));
          };
        }())(xs)))(u);
      };
    };
  };
};

exports.foldlDefault = foldlDefault;

var foldrDefault = function foldrDefault(dictFoldable) {
  var foldMap2 = foldMap(dictFoldable)(monoidEndo);
  return function (c) {
    return function (u) {
      return function (xs) {
        return unwrap(foldMap2(function ($459) {
          return Data_Monoid_Endo.Endo(c($459));
        })(xs))(u);
      };
    };
  };
};

exports.foldrDefault = foldrDefault;

var lookup = function lookup(dictFoldable) {
  var foldMap2 = foldMap(dictFoldable)(Data_Maybe_First.monoidFirst);
  return function (dictEq) {
    var eq2 = Data_Eq.eq(dictEq);
    return function (a) {
      var $460 = foldMap2(function (v) {
        var $444 = eq2(a)(v.value0);

        if ($444) {
          return new Data_Maybe.Just(v.value1);
        }

        ;
        return Data_Maybe.Nothing.value;
      });
      return function ($461) {
        return unwrap($460($461));
      };
    };
  };
};

exports.lookup = lookup;

var surroundMap = function surroundMap(dictFoldable) {
  var foldMap2 = foldMap(dictFoldable)(monoidEndo);
  return function (dictSemigroup) {
    var append = Data_Semigroup.append(dictSemigroup);
    return function (d) {
      return function (t) {
        return function (f) {
          var joined = function joined(a) {
            return function (m) {
              return append(d)(append(t(a))(m));
            };
          };

          return unwrap(foldMap2(joined)(f))(d);
        };
      };
    };
  };
};

exports.surroundMap = surroundMap;

var surround = function surround(dictFoldable) {
  var surroundMap1 = surroundMap(dictFoldable);
  return function (dictSemigroup) {
    var surroundMap2 = surroundMap1(dictSemigroup);
    return function (d) {
      return surroundMap2(d)(identity);
    };
  };
};

exports.surround = surround;

var foldM = function foldM(dictFoldable) {
  var foldl2 = foldl(dictFoldable);
  return function (dictMonad) {
    var bind = Control_Bind.bind(dictMonad.Bind1());
    var pure = Control_Applicative.pure(dictMonad.Applicative0());
    return function (f) {
      return function (b0) {
        return foldl2(function (b) {
          return function (a) {
            return bind(b)(Data_Function.flip(f)(a));
          };
        })(pure(b0));
      };
    };
  };
};

exports.foldM = foldM;

var fold = function fold(dictFoldable) {
  var foldMap2 = foldMap(dictFoldable);
  return function (dictMonoid) {
    return foldMap2(dictMonoid)(identity);
  };
};

exports.fold = fold;

var findMap = function findMap(dictFoldable) {
  var foldl2 = foldl(dictFoldable);
  return function (p) {
    var go = function go(v) {
      return function (v1) {
        if (v instanceof Data_Maybe.Nothing) {
          return p(v1);
        }

        ;
        return v;
      };
    };

    return foldl2(go)(Data_Maybe.Nothing.value);
  };
};

exports.findMap = findMap;

var find = function find(dictFoldable) {
  var foldl2 = foldl(dictFoldable);
  return function (p) {
    var go = function go(v) {
      return function (v1) {
        if (v instanceof Data_Maybe.Nothing && p(v1)) {
          return new Data_Maybe.Just(v1);
        }

        ;
        return v;
      };
    };

    return foldl2(go)(Data_Maybe.Nothing.value);
  };
};

exports.find = find;

var any = function any(dictFoldable) {
  var foldMap2 = foldMap(dictFoldable);
  return function (dictHeytingAlgebra) {
    return alaF(Data_Monoid_Disj.Disj)(foldMap2(Data_Monoid_Disj.monoidDisj(dictHeytingAlgebra)));
  };
};

exports.any = any;

var elem = function elem(dictFoldable) {
  var any1 = any(dictFoldable)(Data_HeytingAlgebra.heytingAlgebraBoolean);
  return function (dictEq) {
    var $462 = Data_Eq.eq(dictEq);
    return function ($463) {
      return any1($462($463));
    };
  };
};

exports.elem = elem;

var notElem = function notElem(dictFoldable) {
  var elem1 = elem(dictFoldable);
  return function (dictEq) {
    var elem2 = elem1(dictEq);
    return function (x) {
      var $464 = elem2(x);
      return function ($465) {
        return !$464($465);
      };
    };
  };
};

exports.notElem = notElem;

var or = function or(dictFoldable) {
  var any1 = any(dictFoldable);
  return function (dictHeytingAlgebra) {
    return any1(dictHeytingAlgebra)(identity);
  };
};

exports.or = or;

var all = function all(dictFoldable) {
  var foldMap2 = foldMap(dictFoldable);
  return function (dictHeytingAlgebra) {
    return alaF(Data_Monoid_Conj.Conj)(foldMap2(Data_Monoid_Conj.monoidConj(dictHeytingAlgebra)));
  };
};

exports.all = all;

var and = function and(dictFoldable) {
  var all1 = all(dictFoldable);
  return function (dictHeytingAlgebra) {
    return all1(dictHeytingAlgebra)(identity);
  };
};

exports.and = and;
},{"./foreign.js":"../output/Data.Foldable/foreign.js","../Control.Alt/index.js":"../output/Control.Alt/index.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Category/index.js":"../output/Control.Category/index.js","../Control.Plus/index.js":"../output/Control.Plus/index.js","../Data.Either/index.js":"../output/Data.Either/index.js","../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.Function/index.js":"../output/Data.Function/index.js","../Data.Functor.Coproduct/index.js":"../output/Data.Functor.Coproduct/index.js","../Data.HeytingAlgebra/index.js":"../output/Data.HeytingAlgebra/index.js","../Data.Maybe/index.js":"../output/Data.Maybe/index.js","../Data.Maybe.First/index.js":"../output/Data.Maybe.First/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Monoid.Conj/index.js":"../output/Data.Monoid.Conj/index.js","../Data.Monoid.Disj/index.js":"../output/Data.Monoid.Disj/index.js","../Data.Monoid.Dual/index.js":"../output/Data.Monoid.Dual/index.js","../Data.Monoid.Endo/index.js":"../output/Data.Monoid.Endo/index.js","../Data.Newtype/index.js":"../output/Data.Newtype/index.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Ordering/index.js":"../output/Data.Ordering/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Semiring/index.js":"../output/Data.Semiring/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js"}],"../output/Data.Traversable/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.traverseArrayImpl = void 0;

// jshint maxparams: 3
var traverseArrayImpl = function () {
  function array1(a) {
    return [a];
  }

  function array2(a) {
    return function (b) {
      return [a, b];
    };
  }

  function array3(a) {
    return function (b) {
      return function (c) {
        return [a, b, c];
      };
    };
  }

  function concat2(xs) {
    return function (ys) {
      return xs.concat(ys);
    };
  }

  return function (apply) {
    return function (map) {
      return function (pure) {
        return function (f) {
          return function (array) {
            function go(bot, top) {
              switch (top - bot) {
                case 0:
                  return pure([]);

                case 1:
                  return map(array1)(f(array[bot]));

                case 2:
                  return apply(map(array2)(f(array[bot])))(f(array[bot + 1]));

                case 3:
                  return apply(apply(map(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));

                default:
                  // This slightly tricky pivot selection aims to produce two
                  // even-length partitions where possible.
                  var pivot = bot + Math.floor((top - bot) / 4) * 2;
                  return apply(map(concat2)(go(bot, pivot)))(go(pivot, top));
              }
            }

            return go(0, array.length);
          };
        };
      };
    };
  };
}();

exports.traverseArrayImpl = traverseArrayImpl;
},{}],"../output/Data.Const/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showConst = exports.semiringConst = exports.semigroupoidConst = exports.semigroupConst = exports.ringConst = exports.ordConst = exports.ord1Const = exports.newtypeConst = exports.monoidConst = exports.invariantConst = exports.heytingAlgebraConst = exports.functorConst = exports.euclideanRingConst = exports.eqConst = exports.eq1Const = exports.commutativeRingConst = exports.boundedConst = exports.booleanAlgebraConst = exports.applyConst = exports.applicativeConst = exports.Const = void 0;

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_Functor_Invariant = _interopRequireWildcard(require("../Data.Functor.Invariant/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var Const = function Const(x) {
  return x;
};

exports.Const = Const;

var showConst = function showConst(dictShow) {
  var _show = Data_Show.show(dictShow);

  return {
    show: function show(v) {
      return "(Const " + (_show(v) + ")");
    }
  };
};

exports.showConst = showConst;

var semiringConst = function semiringConst(dictSemiring) {
  return dictSemiring;
};

exports.semiringConst = semiringConst;
var semigroupoidConst = {
  compose: function compose(v) {
    return function (v1) {
      return v1;
    };
  }
};
exports.semigroupoidConst = semigroupoidConst;

var semigroupConst = function semigroupConst(dictSemigroup) {
  return dictSemigroup;
};

exports.semigroupConst = semigroupConst;

var ringConst = function ringConst(dictRing) {
  return dictRing;
};

exports.ringConst = ringConst;

var ordConst = function ordConst(dictOrd) {
  return dictOrd;
};

exports.ordConst = ordConst;
var newtypeConst = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeConst = newtypeConst;

var monoidConst = function monoidConst(dictMonoid) {
  return dictMonoid;
};

exports.monoidConst = monoidConst;

var heytingAlgebraConst = function heytingAlgebraConst(dictHeytingAlgebra) {
  return dictHeytingAlgebra;
};

exports.heytingAlgebraConst = heytingAlgebraConst;
var functorConst = {
  map: function map(f) {
    return function (m) {
      return m;
    };
  }
};
exports.functorConst = functorConst;
var invariantConst = {
  imap:
  /* #__PURE__ */
  Data_Functor_Invariant.imapF(functorConst)
};
exports.invariantConst = invariantConst;

var euclideanRingConst = function euclideanRingConst(dictEuclideanRing) {
  return dictEuclideanRing;
};

exports.euclideanRingConst = euclideanRingConst;

var eqConst = function eqConst(dictEq) {
  return dictEq;
};

exports.eqConst = eqConst;

var eq1Const = function eq1Const(dictEq) {
  var eq = Data_Eq.eq(eqConst(dictEq));
  return {
    eq1: function eq1(dictEq1) {
      return eq;
    }
  };
};

exports.eq1Const = eq1Const;

var ord1Const = function ord1Const(dictOrd) {
  var compare = Data_Ord.compare(ordConst(dictOrd));
  var eq1Const1 = eq1Const(dictOrd.Eq0());
  return {
    compare1: function compare1(dictOrd1) {
      return compare;
    },
    Eq10: function Eq10() {
      return eq1Const1;
    }
  };
};

exports.ord1Const = ord1Const;

var commutativeRingConst = function commutativeRingConst(dictCommutativeRing) {
  return dictCommutativeRing;
};

exports.commutativeRingConst = commutativeRingConst;

var boundedConst = function boundedConst(dictBounded) {
  return dictBounded;
};

exports.boundedConst = boundedConst;

var booleanAlgebraConst = function booleanAlgebraConst(dictBooleanAlgebra) {
  return dictBooleanAlgebra;
};

exports.booleanAlgebraConst = booleanAlgebraConst;

var applyConst = function applyConst(dictSemigroup) {
  var append1 = Data_Semigroup.append(dictSemigroup);
  return {
    apply: function apply(v) {
      return function (v1) {
        return append1(v)(v1);
      };
    },
    Functor0: function Functor0() {
      return functorConst;
    }
  };
};

exports.applyConst = applyConst;

var applicativeConst = function applicativeConst(dictMonoid) {
  var mempty = Data_Monoid.mempty(dictMonoid);
  var applyConst1 = applyConst(dictMonoid.Semigroup0());
  return {
    pure: function pure(v) {
      return mempty;
    },
    Apply0: function Apply0() {
      return applyConst1;
    }
  };
};

exports.applicativeConst = applicativeConst;
},{"../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.Functor.Invariant/index.js":"../output/Data.Functor.Invariant/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Show/index.js":"../output/Data.Show/index.js"}],"../output/Data.Functor.Product/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showProduct = exports.product = exports.ordProduct = exports.ord1Product = exports.newtypeProduct = exports.monadProduct = exports.functorProduct = exports.eqProduct = exports.eq1Product = exports.bindProduct = exports.bihoistProduct = exports.applyProduct = exports.applicativeProduct = exports.Product = void 0;

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Data_Bifunctor = _interopRequireWildcard(require("../Data.Bifunctor/index.js"));

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Newtype = _interopRequireWildcard(require("../Data.Newtype/index.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Ordering = _interopRequireWildcard(require("../Data.Ordering/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

var Data_Tuple = _interopRequireWildcard(require("../Data.Tuple/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var bimap =
/* #__PURE__ */
Data_Bifunctor.bimap(Data_Bifunctor.bifunctorTuple);
var unwrap =
/* #__PURE__ */
Data_Newtype.unwrap();

var Product = function Product(x) {
  return x;
};

exports.Product = Product;

var showProduct = function showProduct(dictShow) {
  var _show = Data_Show.show(dictShow);

  return function (dictShow1) {
    var show1 = Data_Show.show(dictShow1);
    return {
      show: function show(v) {
        return "(product " + (_show(v.value0) + (" " + (show1(v.value1) + ")")));
      }
    };
  };
};

exports.showProduct = showProduct;

var product = function product(fa) {
  return function (ga) {
    return new Data_Tuple.Tuple(fa, ga);
  };
};

exports.product = product;
var newtypeProduct = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeProduct = newtypeProduct;

var functorProduct = function functorProduct(dictFunctor) {
  var _map = Data_Functor.map(dictFunctor);

  return function (dictFunctor1) {
    var map1 = Data_Functor.map(dictFunctor1);
    return {
      map: function map(f) {
        return function (v) {
          return bimap(_map(f))(map1(f))(v);
        };
      }
    };
  };
};

exports.functorProduct = functorProduct;

var eq1Product = function eq1Product(dictEq1) {
  var _eq = Data_Eq.eq1(dictEq1);

  return function (dictEq11) {
    var eq11 = Data_Eq.eq1(dictEq11);
    return {
      eq1: function eq1(dictEq) {
        var eq12 = _eq(dictEq);

        var eq13 = eq11(dictEq);
        return function (v) {
          return function (v1) {
            return eq12(v.value0)(v1.value0) && eq13(v.value1)(v1.value1);
          };
        };
      }
    };
  };
};

exports.eq1Product = eq1Product;

var eqProduct = function eqProduct(dictEq1) {
  var eq1Product1 = eq1Product(dictEq1);
  return function (dictEq11) {
    var eq1 = Data_Eq.eq1(eq1Product1(dictEq11));
    return function (dictEq) {
      return {
        eq: eq1(dictEq)
      };
    };
  };
};

exports.eqProduct = eqProduct;

var ord1Product = function ord1Product(dictOrd1) {
  var _compare = Data_Ord.compare1(dictOrd1);

  var eq1Product1 = eq1Product(dictOrd1.Eq10());
  return function (dictOrd11) {
    var compare11 = Data_Ord.compare1(dictOrd11);
    var eq1Product2 = eq1Product1(dictOrd11.Eq10());
    return {
      compare1: function compare1(dictOrd) {
        var compare12 = _compare(dictOrd);

        var compare13 = compare11(dictOrd);
        return function (v) {
          return function (v1) {
            var v2 = compare12(v.value0)(v1.value0);

            if (v2 instanceof Data_Ordering.EQ) {
              return compare13(v.value1)(v1.value1);
            }

            ;
            return v2;
          };
        };
      },
      Eq10: function Eq10() {
        return eq1Product2;
      }
    };
  };
};

exports.ord1Product = ord1Product;

var ordProduct = function ordProduct(dictOrd1) {
  var ord1Product1 = ord1Product(dictOrd1);
  var eqProduct1 = eqProduct(dictOrd1.Eq10());
  return function (dictOrd11) {
    var compare1 = Data_Ord.compare1(ord1Product1(dictOrd11));
    var eqProduct2 = eqProduct1(dictOrd11.Eq10());
    return function (dictOrd) {
      var eqProduct3 = eqProduct2(dictOrd.Eq0());
      return {
        compare: compare1(dictOrd),
        Eq0: function Eq0() {
          return eqProduct3;
        }
      };
    };
  };
};

exports.ordProduct = ordProduct;

var bihoistProduct = function bihoistProduct(natF) {
  return function (natG) {
    return function (v) {
      return bimap(natF)(natG)(v);
    };
  };
};

exports.bihoistProduct = bihoistProduct;

var applyProduct = function applyProduct(dictApply) {
  var _apply = Control_Apply.apply(dictApply);

  var functorProduct1 = functorProduct(dictApply.Functor0());
  return function (dictApply1) {
    var apply1 = Control_Apply.apply(dictApply1);
    var functorProduct2 = functorProduct1(dictApply1.Functor0());
    return {
      apply: function apply(v) {
        return function (v1) {
          return product(_apply(v.value0)(v1.value0))(apply1(v.value1)(v1.value1));
        };
      },
      Functor0: function Functor0() {
        return functorProduct2;
      }
    };
  };
};

exports.applyProduct = applyProduct;

var bindProduct = function bindProduct(dictBind) {
  var _bind = Control_Bind.bind(dictBind);

  var applyProduct1 = applyProduct(dictBind.Apply0());
  return function (dictBind1) {
    var bind1 = Control_Bind.bind(dictBind1);
    var applyProduct2 = applyProduct1(dictBind1.Apply0());
    return {
      bind: function bind(v) {
        return function (f) {
          return product(_bind(v.value0)(function ($128) {
            return Data_Tuple.fst(unwrap(f($128)));
          }))(bind1(v.value1)(function ($129) {
            return Data_Tuple.snd(unwrap(f($129)));
          }));
        };
      },
      Apply0: function Apply0() {
        return applyProduct2;
      }
    };
  };
};

exports.bindProduct = bindProduct;

var applicativeProduct = function applicativeProduct(dictApplicative) {
  var _pure = Control_Applicative.pure(dictApplicative);

  var applyProduct1 = applyProduct(dictApplicative.Apply0());
  return function (dictApplicative1) {
    var pure1 = Control_Applicative.pure(dictApplicative1);
    var applyProduct2 = applyProduct1(dictApplicative1.Apply0());
    return {
      pure: function pure(a) {
        return product(_pure(a))(pure1(a));
      },
      Apply0: function Apply0() {
        return applyProduct2;
      }
    };
  };
};

exports.applicativeProduct = applicativeProduct;

var monadProduct = function monadProduct(dictMonad) {
  var applicativeProduct1 = applicativeProduct(dictMonad.Applicative0());
  var bindProduct1 = bindProduct(dictMonad.Bind1());
  return function (dictMonad1) {
    var applicativeProduct2 = applicativeProduct1(dictMonad1.Applicative0());
    var bindProduct2 = bindProduct1(dictMonad1.Bind1());
    return {
      Applicative0: function Applicative0() {
        return applicativeProduct2;
      },
      Bind1: function Bind1() {
        return bindProduct2;
      }
    };
  };
};

exports.monadProduct = monadProduct;
},{"../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Data.Bifunctor/index.js":"../output/Data.Bifunctor/index.js","../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Newtype/index.js":"../output/Data.Newtype/index.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Ordering/index.js":"../output/Data.Ordering/index.js","../Data.Show/index.js":"../output/Data.Show/index.js","../Data.Tuple/index.js":"../output/Data.Tuple/index.js"}],"../output/Data.Maybe.Last/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showLast = exports.semigroupLast = exports.plusLast = exports.ordLast = exports.ord1Last = exports.newtypeLast = exports.monoidLast = exports.monadLast = exports.invariantLast = exports.functorLast = exports.extendLast = exports.eqLast = exports.eq1Last = exports.boundedLast = exports.bindLast = exports.applyLast = exports.applicativeLast = exports.alternativeLast = exports.altLast = exports.Last = void 0;

var Data_Maybe = _interopRequireWildcard(require("../Data.Maybe/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var Last = function Last(x) {
  return x;
};

exports.Last = Last;

var showLast = function showLast(dictShow) {
  var _show = Data_Show.show(Data_Maybe.showMaybe(dictShow));

  return {
    show: function show(v) {
      return "(Last " + (_show(v) + ")");
    }
  };
};

exports.showLast = showLast;
var semigroupLast = {
  append: function append(v) {
    return function (v1) {
      if (v1 instanceof Data_Maybe.Just) {
        return v1;
      }

      ;

      if (v1 instanceof Data_Maybe.Nothing) {
        return v;
      }

      ;
      throw new Error("Failed pattern match at Data.Maybe.Last (line 54, column 1 - line 56, column 36): " + [v.constructor.name, v1.constructor.name]);
    };
  }
};
exports.semigroupLast = semigroupLast;

var ordLast = function ordLast(dictOrd) {
  return Data_Maybe.ordMaybe(dictOrd);
};

exports.ordLast = ordLast;
var ord1Last = Data_Maybe.ord1Maybe;
exports.ord1Last = ord1Last;
var newtypeLast = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeLast = newtypeLast;

var monoidLast =
/* #__PURE__ */
function () {
  return {
    mempty: Data_Maybe.Nothing.value,
    Semigroup0: function Semigroup0() {
      return semigroupLast;
    }
  };
}();

exports.monoidLast = monoidLast;
var monadLast = Data_Maybe.monadMaybe;
exports.monadLast = monadLast;
var invariantLast = Data_Maybe.invariantMaybe;
exports.invariantLast = invariantLast;
var functorLast = Data_Maybe.functorMaybe;
exports.functorLast = functorLast;
var extendLast = Data_Maybe.extendMaybe;
exports.extendLast = extendLast;

var eqLast = function eqLast(dictEq) {
  return Data_Maybe.eqMaybe(dictEq);
};

exports.eqLast = eqLast;
var eq1Last = Data_Maybe.eq1Maybe;
exports.eq1Last = eq1Last;

var boundedLast = function boundedLast(dictBounded) {
  return Data_Maybe.boundedMaybe(dictBounded);
};

exports.boundedLast = boundedLast;
var bindLast = Data_Maybe.bindMaybe;
exports.bindLast = bindLast;
var applyLast = Data_Maybe.applyMaybe;
exports.applyLast = applyLast;
var applicativeLast = Data_Maybe.applicativeMaybe;
exports.applicativeLast = applicativeLast;
var altLast = {
  alt:
  /* #__PURE__ */
  Data_Semigroup.append(semigroupLast),
  Functor0: function Functor0() {
    return functorLast;
  }
};
exports.altLast = altLast;
var plusLast = {
  empty:
  /* #__PURE__ */
  Data_Monoid.mempty(monoidLast),
  Alt0: function Alt0() {
    return altLast;
  }
};
exports.plusLast = plusLast;
var alternativeLast = {
  Applicative0: function Applicative0() {
    return applicativeLast;
  },
  Plus1: function Plus1() {
    return plusLast;
  }
};
exports.alternativeLast = alternativeLast;
},{"../Data.Maybe/index.js":"../output/Data.Maybe/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Show/index.js":"../output/Data.Show/index.js"}],"../output/Data.Monoid.Additive/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showAdditive = exports.semigroupAdditive = exports.ordAdditive = exports.ord1Additive = exports.monoidAdditive = exports.monadAdditive = exports.functorAdditive = exports.eqAdditive = exports.eq1Additive = exports.boundedAdditive = exports.bindAdditive = exports.applyAdditive = exports.applicativeAdditive = exports.Additive = void 0;

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Semiring = _interopRequireWildcard(require("../Data.Semiring/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var Additive = function Additive(x) {
  return x;
};

exports.Additive = Additive;

var showAdditive = function showAdditive(dictShow) {
  var _show = Data_Show.show(dictShow);

  return {
    show: function show(v) {
      return "(Additive " + (_show(v) + ")");
    }
  };
};

exports.showAdditive = showAdditive;

var semigroupAdditive = function semigroupAdditive(dictSemiring) {
  var add = Data_Semiring.add(dictSemiring);
  return {
    append: function append(v) {
      return function (v1) {
        return add(v)(v1);
      };
    }
  };
};

exports.semigroupAdditive = semigroupAdditive;

var ordAdditive = function ordAdditive(dictOrd) {
  return dictOrd;
};

exports.ordAdditive = ordAdditive;

var monoidAdditive = function monoidAdditive(dictSemiring) {
  var semigroupAdditive1 = semigroupAdditive(dictSemiring);
  return {
    mempty: Data_Semiring.zero(dictSemiring),
    Semigroup0: function Semigroup0() {
      return semigroupAdditive1;
    }
  };
};

exports.monoidAdditive = monoidAdditive;
var functorAdditive = {
  map: function map(f) {
    return function (m) {
      return f(m);
    };
  }
};
exports.functorAdditive = functorAdditive;

var eqAdditive = function eqAdditive(dictEq) {
  return dictEq;
};

exports.eqAdditive = eqAdditive;
var eq1Additive = {
  eq1: function eq1(dictEq) {
    return Data_Eq.eq(eqAdditive(dictEq));
  }
};
exports.eq1Additive = eq1Additive;
var ord1Additive = {
  compare1: function compare1(dictOrd) {
    return Data_Ord.compare(ordAdditive(dictOrd));
  },
  Eq10: function Eq10() {
    return eq1Additive;
  }
};
exports.ord1Additive = ord1Additive;

var boundedAdditive = function boundedAdditive(dictBounded) {
  return dictBounded;
};

exports.boundedAdditive = boundedAdditive;
var applyAdditive = {
  apply: function apply(v) {
    return function (v1) {
      return v(v1);
    };
  },
  Functor0: function Functor0() {
    return functorAdditive;
  }
};
exports.applyAdditive = applyAdditive;
var bindAdditive = {
  bind: function bind(v) {
    return function (f) {
      return f(v);
    };
  },
  Apply0: function Apply0() {
    return applyAdditive;
  }
};
exports.bindAdditive = bindAdditive;
var applicativeAdditive = {
  pure: Additive,
  Apply0: function Apply0() {
    return applyAdditive;
  }
};
exports.applicativeAdditive = applicativeAdditive;
var monadAdditive = {
  Applicative0: function Applicative0() {
    return applicativeAdditive;
  },
  Bind1: function Bind1() {
    return bindAdditive;
  }
};
exports.monadAdditive = monadAdditive;
},{"../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Semiring/index.js":"../output/Data.Semiring/index.js","../Data.Show/index.js":"../output/Data.Show/index.js"}],"../output/Data.Monoid.Multiplicative/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.showMultiplicative = exports.semigroupMultiplicative = exports.ordMultiplicative = exports.ord1Multiplicative = exports.monoidMultiplicative = exports.monadMultiplicative = exports.functorMultiplicative = exports.eqMultiplicative = exports.eq1Multiplicative = exports.boundedMultiplicative = exports.bindMultiplicative = exports.applyMultiplicative = exports.applicativeMultiplicative = exports.Multiplicative = void 0;

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Semiring = _interopRequireWildcard(require("../Data.Semiring/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var Multiplicative = function Multiplicative(x) {
  return x;
};

exports.Multiplicative = Multiplicative;

var showMultiplicative = function showMultiplicative(dictShow) {
  var _show = Data_Show.show(dictShow);

  return {
    show: function show(v) {
      return "(Multiplicative " + (_show(v) + ")");
    }
  };
};

exports.showMultiplicative = showMultiplicative;

var semigroupMultiplicative = function semigroupMultiplicative(dictSemiring) {
  var mul = Data_Semiring.mul(dictSemiring);
  return {
    append: function append(v) {
      return function (v1) {
        return mul(v)(v1);
      };
    }
  };
};

exports.semigroupMultiplicative = semigroupMultiplicative;

var ordMultiplicative = function ordMultiplicative(dictOrd) {
  return dictOrd;
};

exports.ordMultiplicative = ordMultiplicative;

var monoidMultiplicative = function monoidMultiplicative(dictSemiring) {
  var semigroupMultiplicative1 = semigroupMultiplicative(dictSemiring);
  return {
    mempty: Data_Semiring.one(dictSemiring),
    Semigroup0: function Semigroup0() {
      return semigroupMultiplicative1;
    }
  };
};

exports.monoidMultiplicative = monoidMultiplicative;
var functorMultiplicative = {
  map: function map(f) {
    return function (m) {
      return f(m);
    };
  }
};
exports.functorMultiplicative = functorMultiplicative;

var eqMultiplicative = function eqMultiplicative(dictEq) {
  return dictEq;
};

exports.eqMultiplicative = eqMultiplicative;
var eq1Multiplicative = {
  eq1: function eq1(dictEq) {
    return Data_Eq.eq(eqMultiplicative(dictEq));
  }
};
exports.eq1Multiplicative = eq1Multiplicative;
var ord1Multiplicative = {
  compare1: function compare1(dictOrd) {
    return Data_Ord.compare(ordMultiplicative(dictOrd));
  },
  Eq10: function Eq10() {
    return eq1Multiplicative;
  }
};
exports.ord1Multiplicative = ord1Multiplicative;

var boundedMultiplicative = function boundedMultiplicative(dictBounded) {
  return dictBounded;
};

exports.boundedMultiplicative = boundedMultiplicative;
var applyMultiplicative = {
  apply: function apply(v) {
    return function (v1) {
      return v(v1);
    };
  },
  Functor0: function Functor0() {
    return functorMultiplicative;
  }
};
exports.applyMultiplicative = applyMultiplicative;
var bindMultiplicative = {
  bind: function bind(v) {
    return function (f) {
      return f(v);
    };
  },
  Apply0: function Apply0() {
    return applyMultiplicative;
  }
};
exports.bindMultiplicative = bindMultiplicative;
var applicativeMultiplicative = {
  pure: Multiplicative,
  Apply0: function Apply0() {
    return applyMultiplicative;
  }
};
exports.applicativeMultiplicative = applicativeMultiplicative;
var monadMultiplicative = {
  Applicative0: function Applicative0() {
    return applicativeMultiplicative;
  },
  Bind1: function Bind1() {
    return bindMultiplicative;
  }
};
exports.monadMultiplicative = monadMultiplicative;
},{"../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Semiring/index.js":"../output/Data.Semiring/index.js","../Data.Show/index.js":"../output/Data.Show/index.js"}],"../output/Data.Traversable.Accum/index.js":[function(require,module,exports) {
// Generated by purs version 0.15.10
},{}],"../output/Data.Traversable.Accum.Internal/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.stateR = exports.stateL = exports.functorStateR = exports.functorStateL = exports.applyStateR = exports.applyStateL = exports.applicativeStateR = exports.applicativeStateL = exports.StateR = exports.StateL = void 0;

// Generated by purs version 0.15.10
var StateR = function StateR(x) {
  return x;
};

exports.StateR = StateR;

var StateL = function StateL(x) {
  return x;
};

exports.StateL = StateL;

var stateR = function stateR(v) {
  return v;
};

exports.stateR = stateR;

var stateL = function stateL(v) {
  return v;
};

exports.stateL = stateL;
var functorStateR = {
  map: function map(f) {
    return function (k) {
      return function (s) {
        var v = stateR(k)(s);
        return {
          accum: v.accum,
          value: f(v.value)
        };
      };
    };
  }
};
exports.functorStateR = functorStateR;
var functorStateL = {
  map: function map(f) {
    return function (k) {
      return function (s) {
        var v = stateL(k)(s);
        return {
          accum: v.accum,
          value: f(v.value)
        };
      };
    };
  }
};
exports.functorStateL = functorStateL;
var applyStateR = {
  apply: function apply(f) {
    return function (x) {
      return function (s) {
        var v = stateR(x)(s);
        var v1 = stateR(f)(v.accum);
        return {
          accum: v1.accum,
          value: v1.value(v.value)
        };
      };
    };
  },
  Functor0: function Functor0() {
    return functorStateR;
  }
};
exports.applyStateR = applyStateR;
var applyStateL = {
  apply: function apply(f) {
    return function (x) {
      return function (s) {
        var v = stateL(f)(s);
        var v1 = stateL(x)(v.accum);
        return {
          accum: v1.accum,
          value: v.value(v1.value)
        };
      };
    };
  },
  Functor0: function Functor0() {
    return functorStateL;
  }
};
exports.applyStateL = applyStateL;
var applicativeStateR = {
  pure: function pure(a) {
    return function (s) {
      return {
        accum: s,
        value: a
      };
    };
  },
  Apply0: function Apply0() {
    return applyStateR;
  }
};
exports.applicativeStateR = applicativeStateR;
var applicativeStateL = {
  pure: function pure(a) {
    return function (s) {
      return {
        accum: s,
        value: a
      };
    };
  },
  Apply0: function Apply0() {
    return applyStateL;
  }
};
exports.applicativeStateL = applicativeStateL;
},{}],"../output/Data.Traversable/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "all", {
  enumerable: true,
  get: function () {
    return Data_Foldable.all;
  }
});
Object.defineProperty(exports, "and", {
  enumerable: true,
  get: function () {
    return Data_Foldable.and;
  }
});
Object.defineProperty(exports, "any", {
  enumerable: true,
  get: function () {
    return Data_Foldable.any;
  }
});
Object.defineProperty(exports, "elem", {
  enumerable: true,
  get: function () {
    return Data_Foldable.elem;
  }
});
Object.defineProperty(exports, "find", {
  enumerable: true,
  get: function () {
    return Data_Foldable.find;
  }
});
Object.defineProperty(exports, "fold", {
  enumerable: true,
  get: function () {
    return Data_Foldable.fold;
  }
});
Object.defineProperty(exports, "foldMap", {
  enumerable: true,
  get: function () {
    return Data_Foldable.foldMap;
  }
});
Object.defineProperty(exports, "foldMapDefaultL", {
  enumerable: true,
  get: function () {
    return Data_Foldable.foldMapDefaultL;
  }
});
Object.defineProperty(exports, "foldMapDefaultR", {
  enumerable: true,
  get: function () {
    return Data_Foldable.foldMapDefaultR;
  }
});
Object.defineProperty(exports, "foldl", {
  enumerable: true,
  get: function () {
    return Data_Foldable.foldl;
  }
});
Object.defineProperty(exports, "foldlDefault", {
  enumerable: true,
  get: function () {
    return Data_Foldable.foldlDefault;
  }
});
Object.defineProperty(exports, "foldr", {
  enumerable: true,
  get: function () {
    return Data_Foldable.foldr;
  }
});
Object.defineProperty(exports, "foldrDefault", {
  enumerable: true,
  get: function () {
    return Data_Foldable.foldrDefault;
  }
});
exports.for = void 0;
Object.defineProperty(exports, "for_", {
  enumerable: true,
  get: function () {
    return Data_Foldable.for_;
  }
});
Object.defineProperty(exports, "intercalate", {
  enumerable: true,
  get: function () {
    return Data_Foldable.intercalate;
  }
});
exports.mapAccumR = exports.mapAccumL = void 0;
Object.defineProperty(exports, "maximum", {
  enumerable: true,
  get: function () {
    return Data_Foldable.maximum;
  }
});
Object.defineProperty(exports, "maximumBy", {
  enumerable: true,
  get: function () {
    return Data_Foldable.maximumBy;
  }
});
Object.defineProperty(exports, "minimum", {
  enumerable: true,
  get: function () {
    return Data_Foldable.minimum;
  }
});
Object.defineProperty(exports, "minimumBy", {
  enumerable: true,
  get: function () {
    return Data_Foldable.minimumBy;
  }
});
Object.defineProperty(exports, "notElem", {
  enumerable: true,
  get: function () {
    return Data_Foldable.notElem;
  }
});
Object.defineProperty(exports, "oneOf", {
  enumerable: true,
  get: function () {
    return Data_Foldable.oneOf;
  }
});
Object.defineProperty(exports, "or", {
  enumerable: true,
  get: function () {
    return Data_Foldable.or;
  }
});
exports.sequenceDefault = exports.sequence = exports.scanr = exports.scanl = void 0;
Object.defineProperty(exports, "sequence_", {
  enumerable: true,
  get: function () {
    return Data_Foldable.sequence_;
  }
});
Object.defineProperty(exports, "sum", {
  enumerable: true,
  get: function () {
    return Data_Foldable.sum;
  }
});
exports.traverseDefault = exports.traverse = exports.traversableTuple = exports.traversableProduct = exports.traversableMultiplicative = exports.traversableMaybe = exports.traversableLast = exports.traversableIdentity = exports.traversableFirst = exports.traversableEither = exports.traversableDual = exports.traversableDisj = exports.traversableCoproduct = exports.traversableConst = exports.traversableConj = exports.traversableCompose = exports.traversableArray = exports.traversableApp = exports.traversableAdditive = void 0;
Object.defineProperty(exports, "traverse_", {
  enumerable: true,
  get: function () {
    return Data_Foldable.traverse_;
  }
});

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Data_Const = _interopRequireWildcard(require("../Data.Const/index.js"));

var Data_Either = _interopRequireWildcard(require("../Data.Either/index.js"));

var Data_Foldable = _interopRequireWildcard(require("../Data.Foldable/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Functor_App = _interopRequireWildcard(require("../Data.Functor.App/index.js"));

var Data_Functor_Compose = _interopRequireWildcard(require("../Data.Functor.Compose/index.js"));

var Data_Functor_Coproduct = _interopRequireWildcard(require("../Data.Functor.Coproduct/index.js"));

var Data_Functor_Product = _interopRequireWildcard(require("../Data.Functor.Product/index.js"));

var Data_Identity = _interopRequireWildcard(require("../Data.Identity/index.js"));

var Data_Maybe = _interopRequireWildcard(require("../Data.Maybe/index.js"));

var Data_Maybe_First = _interopRequireWildcard(require("../Data.Maybe.First/index.js"));

var Data_Maybe_Last = _interopRequireWildcard(require("../Data.Maybe.Last/index.js"));

var Data_Monoid_Additive = _interopRequireWildcard(require("../Data.Monoid.Additive/index.js"));

var Data_Monoid_Conj = _interopRequireWildcard(require("../Data.Monoid.Conj/index.js"));

var Data_Monoid_Disj = _interopRequireWildcard(require("../Data.Monoid.Disj/index.js"));

var Data_Monoid_Dual = _interopRequireWildcard(require("../Data.Monoid.Dual/index.js"));

var Data_Monoid_Multiplicative = _interopRequireWildcard(require("../Data.Monoid.Multiplicative/index.js"));

var Data_Traversable_Accum = _interopRequireWildcard(require("../Data.Traversable.Accum/index.js"));

var Data_Traversable_Accum_Internal = _interopRequireWildcard(require("../Data.Traversable.Accum.Internal/index.js"));

var Data_Tuple = _interopRequireWildcard(require("../Data.Tuple/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var identity =
/* #__PURE__ */
Control_Category.identity(Control_Category.categoryFn);

var traverse = function traverse(dict) {
  return dict.traverse;
};

exports.traverse = traverse;
var traversableTuple = {
  traverse: function traverse(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (f) {
      return function (v) {
        return map(Data_Tuple.Tuple.create(v.value0))(f(v.value1));
      };
    };
  },
  sequence: function sequence(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (v) {
      return map(Data_Tuple.Tuple.create(v.value0))(v.value1);
    };
  },
  Functor0: function Functor0() {
    return Data_Tuple.functorTuple;
  },
  Foldable1: function Foldable1() {
    return Data_Foldable.foldableTuple;
  }
};
exports.traversableTuple = traversableTuple;
var traversableMultiplicative = {
  traverse: function traverse(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (f) {
      return function (v) {
        return map(Data_Monoid_Multiplicative.Multiplicative)(f(v));
      };
    };
  },
  sequence: function sequence(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (v) {
      return map(Data_Monoid_Multiplicative.Multiplicative)(v);
    };
  },
  Functor0: function Functor0() {
    return Data_Monoid_Multiplicative.functorMultiplicative;
  },
  Foldable1: function Foldable1() {
    return Data_Foldable.foldableMultiplicative;
  }
};
exports.traversableMultiplicative = traversableMultiplicative;
var traversableMaybe = {
  traverse: function traverse(dictApplicative) {
    var pure = Control_Applicative.pure(dictApplicative);
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (v) {
      return function (v1) {
        if (v1 instanceof Data_Maybe.Nothing) {
          return pure(Data_Maybe.Nothing.value);
        }

        ;

        if (v1 instanceof Data_Maybe.Just) {
          return map(Data_Maybe.Just.create)(v(v1.value0));
        }

        ;
        throw new Error("Failed pattern match at Data.Traversable (line 115, column 1 - line 119, column 33): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  },
  sequence: function sequence(dictApplicative) {
    var pure = Control_Applicative.pure(dictApplicative);
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (v) {
      if (v instanceof Data_Maybe.Nothing) {
        return pure(Data_Maybe.Nothing.value);
      }

      ;

      if (v instanceof Data_Maybe.Just) {
        return map(Data_Maybe.Just.create)(v.value0);
      }

      ;
      throw new Error("Failed pattern match at Data.Traversable (line 115, column 1 - line 119, column 33): " + [v.constructor.name]);
    };
  },
  Functor0: function Functor0() {
    return Data_Maybe.functorMaybe;
  },
  Foldable1: function Foldable1() {
    return Data_Foldable.foldableMaybe;
  }
};
exports.traversableMaybe = traversableMaybe;
var traverse1 =
/* #__PURE__ */
traverse(traversableMaybe);
var traversableIdentity = {
  traverse: function traverse(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (f) {
      return function (v) {
        return map(Data_Identity.Identity)(f(v));
      };
    };
  },
  sequence: function sequence(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (v) {
      return map(Data_Identity.Identity)(v);
    };
  },
  Functor0: function Functor0() {
    return Data_Identity.functorIdentity;
  },
  Foldable1: function Foldable1() {
    return Data_Foldable.foldableIdentity;
  }
};
exports.traversableIdentity = traversableIdentity;
var traversableEither = {
  traverse: function traverse(dictApplicative) {
    var pure = Control_Applicative.pure(dictApplicative);
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (v) {
      return function (v1) {
        if (v1 instanceof Data_Either.Left) {
          return pure(new Data_Either.Left(v1.value0));
        }

        ;

        if (v1 instanceof Data_Either.Right) {
          return map(Data_Either.Right.create)(v(v1.value0));
        }

        ;
        throw new Error("Failed pattern match at Data.Traversable (line 149, column 1 - line 153, column 36): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  },
  sequence: function sequence(dictApplicative) {
    var pure = Control_Applicative.pure(dictApplicative);
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (v) {
      if (v instanceof Data_Either.Left) {
        return pure(new Data_Either.Left(v.value0));
      }

      ;

      if (v instanceof Data_Either.Right) {
        return map(Data_Either.Right.create)(v.value0);
      }

      ;
      throw new Error("Failed pattern match at Data.Traversable (line 149, column 1 - line 153, column 36): " + [v.constructor.name]);
    };
  },
  Functor0: function Functor0() {
    return Data_Either.functorEither;
  },
  Foldable1: function Foldable1() {
    return Data_Foldable.foldableEither;
  }
};
exports.traversableEither = traversableEither;
var traversableDual = {
  traverse: function traverse(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (f) {
      return function (v) {
        return map(Data_Monoid_Dual.Dual)(f(v));
      };
    };
  },
  sequence: function sequence(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (v) {
      return map(Data_Monoid_Dual.Dual)(v);
    };
  },
  Functor0: function Functor0() {
    return Data_Monoid_Dual.functorDual;
  },
  Foldable1: function Foldable1() {
    return Data_Foldable.foldableDual;
  }
};
exports.traversableDual = traversableDual;
var traversableDisj = {
  traverse: function traverse(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (f) {
      return function (v) {
        return map(Data_Monoid_Disj.Disj)(f(v));
      };
    };
  },
  sequence: function sequence(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (v) {
      return map(Data_Monoid_Disj.Disj)(v);
    };
  },
  Functor0: function Functor0() {
    return Data_Monoid_Disj.functorDisj;
  },
  Foldable1: function Foldable1() {
    return Data_Foldable.foldableDisj;
  }
};
exports.traversableDisj = traversableDisj;
var traversableConst = {
  traverse: function traverse(dictApplicative) {
    var pure = Control_Applicative.pure(dictApplicative);
    return function (v) {
      return function (v1) {
        return pure(v1);
      };
    };
  },
  sequence: function sequence(dictApplicative) {
    var pure = Control_Applicative.pure(dictApplicative);
    return function (v) {
      return pure(v);
    };
  },
  Functor0: function Functor0() {
    return Data_Const.functorConst;
  },
  Foldable1: function Foldable1() {
    return Data_Foldable.foldableConst;
  }
};
exports.traversableConst = traversableConst;
var traversableConj = {
  traverse: function traverse(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (f) {
      return function (v) {
        return map(Data_Monoid_Conj.Conj)(f(v));
      };
    };
  },
  sequence: function sequence(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (v) {
      return map(Data_Monoid_Conj.Conj)(v);
    };
  },
  Functor0: function Functor0() {
    return Data_Monoid_Conj.functorConj;
  },
  Foldable1: function Foldable1() {
    return Data_Foldable.foldableConj;
  }
};
exports.traversableConj = traversableConj;

var traversableCompose = function traversableCompose(dictTraversable) {
  var traverse2 = traverse(dictTraversable);
  var functorCompose = Data_Functor_Compose.functorCompose(dictTraversable.Functor0());
  var foldableCompose = Data_Foldable.foldableCompose(dictTraversable.Foldable1());
  return function (dictTraversable1) {
    var traverse3 = traverse(dictTraversable1);
    var functorCompose1 = functorCompose(dictTraversable1.Functor0());
    var foldableCompose1 = foldableCompose(dictTraversable1.Foldable1());
    return {
      traverse: function traverse(dictApplicative) {
        var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
        var traverse4 = traverse2(dictApplicative);
        var traverse5 = traverse3(dictApplicative);
        return function (f) {
          return function (v) {
            return map(Data_Functor_Compose.Compose)(traverse4(traverse5(f))(v));
          };
        };
      },
      sequence: function sequence(dictApplicative) {
        return traverse(traversableCompose(dictTraversable)(dictTraversable1))(dictApplicative)(identity);
      },
      Functor0: function Functor0() {
        return functorCompose1;
      },
      Foldable1: function Foldable1() {
        return foldableCompose1;
      }
    };
  };
};

exports.traversableCompose = traversableCompose;
var traversableAdditive = {
  traverse: function traverse(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (f) {
      return function (v) {
        return map(Data_Monoid_Additive.Additive)(f(v));
      };
    };
  },
  sequence: function sequence(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    return function (v) {
      return map(Data_Monoid_Additive.Additive)(v);
    };
  },
  Functor0: function Functor0() {
    return Data_Monoid_Additive.functorAdditive;
  },
  Foldable1: function Foldable1() {
    return Data_Foldable.foldableAdditive;
  }
};
exports.traversableAdditive = traversableAdditive;

var sequenceDefault = function sequenceDefault(dictTraversable) {
  var traverse2 = traverse(dictTraversable);
  return function (dictApplicative) {
    return traverse2(dictApplicative)(identity);
  };
};

exports.sequenceDefault = sequenceDefault;
var traversableArray = {
  traverse: function traverse(dictApplicative) {
    var Apply0 = dictApplicative.Apply0();
    return $foreign.traverseArrayImpl(Control_Apply.apply(Apply0))(Data_Functor.map(Apply0.Functor0()))(Control_Applicative.pure(dictApplicative));
  },
  sequence: function sequence(dictApplicative) {
    return sequenceDefault(traversableArray)(dictApplicative);
  },
  Functor0: function Functor0() {
    return Data_Functor.functorArray;
  },
  Foldable1: function Foldable1() {
    return Data_Foldable.foldableArray;
  }
};
exports.traversableArray = traversableArray;

var sequence = function sequence(dict) {
  return dict.sequence;
};

exports.sequence = sequence;
var sequence1 =
/* #__PURE__ */
sequence(traversableMaybe);

var traversableApp = function traversableApp(dictTraversable) {
  var traverse2 = traverse(dictTraversable);
  var sequence2 = sequence(dictTraversable);
  var functorApp = Data_Functor_App.functorApp(dictTraversable.Functor0());
  var foldableApp = Data_Foldable.foldableApp(dictTraversable.Foldable1());
  return {
    traverse: function traverse(dictApplicative) {
      var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
      var traverse3 = traverse2(dictApplicative);
      return function (f) {
        return function (v) {
          return map(Data_Functor_App.App)(traverse3(f)(v));
        };
      };
    },
    sequence: function sequence(dictApplicative) {
      var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
      var sequence3 = sequence2(dictApplicative);
      return function (v) {
        return map(Data_Functor_App.App)(sequence3(v));
      };
    },
    Functor0: function Functor0() {
      return functorApp;
    },
    Foldable1: function Foldable1() {
      return foldableApp;
    }
  };
};

exports.traversableApp = traversableApp;

var traversableCoproduct = function traversableCoproduct(dictTraversable) {
  var traverse2 = traverse(dictTraversable);
  var sequence2 = sequence(dictTraversable);
  var functorCoproduct = Data_Functor_Coproduct.functorCoproduct(dictTraversable.Functor0());
  var foldableCoproduct = Data_Foldable.foldableCoproduct(dictTraversable.Foldable1());
  return function (dictTraversable1) {
    var traverse3 = traverse(dictTraversable1);
    var sequence3 = sequence(dictTraversable1);
    var functorCoproduct1 = functorCoproduct(dictTraversable1.Functor0());
    var foldableCoproduct1 = foldableCoproduct(dictTraversable1.Foldable1());
    return {
      traverse: function traverse(dictApplicative) {
        var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
        var traverse4 = traverse2(dictApplicative);
        var traverse5 = traverse3(dictApplicative);
        return function (f) {
          return Data_Functor_Coproduct.coproduct(function () {
            var $313 = map(function ($316) {
              return Data_Functor_Coproduct.Coproduct(Data_Either.Left.create($316));
            });
            var $314 = traverse4(f);
            return function ($315) {
              return $313($314($315));
            };
          }())(function () {
            var $317 = map(function ($320) {
              return Data_Functor_Coproduct.Coproduct(Data_Either.Right.create($320));
            });
            var $318 = traverse5(f);
            return function ($319) {
              return $317($318($319));
            };
          }());
        };
      },
      sequence: function sequence(dictApplicative) {
        var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
        return Data_Functor_Coproduct.coproduct(function () {
          var $321 = map(function ($324) {
            return Data_Functor_Coproduct.Coproduct(Data_Either.Left.create($324));
          });
          var $322 = sequence2(dictApplicative);
          return function ($323) {
            return $321($322($323));
          };
        }())(function () {
          var $325 = map(function ($328) {
            return Data_Functor_Coproduct.Coproduct(Data_Either.Right.create($328));
          });
          var $326 = sequence3(dictApplicative);
          return function ($327) {
            return $325($326($327));
          };
        }());
      },
      Functor0: function Functor0() {
        return functorCoproduct1;
      },
      Foldable1: function Foldable1() {
        return foldableCoproduct1;
      }
    };
  };
};

exports.traversableCoproduct = traversableCoproduct;
var traversableFirst = {
  traverse: function traverse(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    var traverse2 = traverse1(dictApplicative);
    return function (f) {
      return function (v) {
        return map(Data_Maybe_First.First)(traverse2(f)(v));
      };
    };
  },
  sequence: function sequence(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    var sequence2 = sequence1(dictApplicative);
    return function (v) {
      return map(Data_Maybe_First.First)(sequence2(v));
    };
  },
  Functor0: function Functor0() {
    return Data_Maybe_First.functorFirst;
  },
  Foldable1: function Foldable1() {
    return Data_Foldable.foldableFirst;
  }
};
exports.traversableFirst = traversableFirst;
var traversableLast = {
  traverse: function traverse(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    var traverse2 = traverse1(dictApplicative);
    return function (f) {
      return function (v) {
        return map(Data_Maybe_Last.Last)(traverse2(f)(v));
      };
    };
  },
  sequence: function sequence(dictApplicative) {
    var map = Data_Functor.map(dictApplicative.Apply0().Functor0());
    var sequence2 = sequence1(dictApplicative);
    return function (v) {
      return map(Data_Maybe_Last.Last)(sequence2(v));
    };
  },
  Functor0: function Functor0() {
    return Data_Maybe_Last.functorLast;
  },
  Foldable1: function Foldable1() {
    return Data_Foldable.foldableLast;
  }
};
exports.traversableLast = traversableLast;

var traversableProduct = function traversableProduct(dictTraversable) {
  var traverse2 = traverse(dictTraversable);
  var sequence2 = sequence(dictTraversable);
  var functorProduct = Data_Functor_Product.functorProduct(dictTraversable.Functor0());
  var foldableProduct = Data_Foldable.foldableProduct(dictTraversable.Foldable1());
  return function (dictTraversable1) {
    var traverse3 = traverse(dictTraversable1);
    var sequence3 = sequence(dictTraversable1);
    var functorProduct1 = functorProduct(dictTraversable1.Functor0());
    var foldableProduct1 = foldableProduct(dictTraversable1.Foldable1());
    return {
      traverse: function traverse(dictApplicative) {
        var lift2 = Control_Apply.lift2(dictApplicative.Apply0());
        var traverse4 = traverse2(dictApplicative);
        var traverse5 = traverse3(dictApplicative);
        return function (f) {
          return function (v) {
            return lift2(Data_Functor_Product.product)(traverse4(f)(v.value0))(traverse5(f)(v.value1));
          };
        };
      },
      sequence: function sequence(dictApplicative) {
        var lift2 = Control_Apply.lift2(dictApplicative.Apply0());
        var sequence4 = sequence2(dictApplicative);
        var sequence5 = sequence3(dictApplicative);
        return function (v) {
          return lift2(Data_Functor_Product.product)(sequence4(v.value0))(sequence5(v.value1));
        };
      },
      Functor0: function Functor0() {
        return functorProduct1;
      },
      Foldable1: function Foldable1() {
        return foldableProduct1;
      }
    };
  };
};

exports.traversableProduct = traversableProduct;

var traverseDefault = function traverseDefault(dictTraversable) {
  var sequence2 = sequence(dictTraversable);
  var map = Data_Functor.map(dictTraversable.Functor0());
  return function (dictApplicative) {
    var sequence3 = sequence2(dictApplicative);
    return function (f) {
      return function (ta) {
        return sequence3(map(f)(ta));
      };
    };
  };
};

exports.traverseDefault = traverseDefault;

var mapAccumR = function mapAccumR(dictTraversable) {
  var traverse2 = traverse(dictTraversable)(Data_Traversable_Accum_Internal.applicativeStateR);
  return function (f) {
    return function (s0) {
      return function (xs) {
        return Data_Traversable_Accum_Internal.stateR(traverse2(function (a) {
          return function (s) {
            return f(s)(a);
          };
        })(xs))(s0);
      };
    };
  };
};

exports.mapAccumR = mapAccumR;

var scanr = function scanr(dictTraversable) {
  var mapAccumR1 = mapAccumR(dictTraversable);
  return function (f) {
    return function (b0) {
      return function (xs) {
        return mapAccumR1(function (b) {
          return function (a) {
            var b$prime = f(a)(b);
            return {
              accum: b$prime,
              value: b$prime
            };
          };
        })(b0)(xs).value;
      };
    };
  };
};

exports.scanr = scanr;

var mapAccumL = function mapAccumL(dictTraversable) {
  var traverse2 = traverse(dictTraversable)(Data_Traversable_Accum_Internal.applicativeStateL);
  return function (f) {
    return function (s0) {
      return function (xs) {
        return Data_Traversable_Accum_Internal.stateL(traverse2(function (a) {
          return function (s) {
            return f(s)(a);
          };
        })(xs))(s0);
      };
    };
  };
};

exports.mapAccumL = mapAccumL;

var scanl = function scanl(dictTraversable) {
  var mapAccumL1 = mapAccumL(dictTraversable);
  return function (f) {
    return function (b0) {
      return function (xs) {
        return mapAccumL1(function (b) {
          return function (a) {
            var b$prime = f(b)(a);
            return {
              accum: b$prime,
              value: b$prime
            };
          };
        })(b0)(xs).value;
      };
    };
  };
};

exports.scanl = scanl;

var $$for = function $$for(dictApplicative) {
  return function (dictTraversable) {
    var traverse2 = traverse(dictTraversable)(dictApplicative);
    return function (x) {
      return function (f) {
        return traverse2(f)(x);
      };
    };
  };
};

exports.for = $$for;
},{"./foreign.js":"../output/Data.Traversable/foreign.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Category/index.js":"../output/Control.Category/index.js","../Data.Const/index.js":"../output/Data.Const/index.js","../Data.Either/index.js":"../output/Data.Either/index.js","../Data.Foldable/index.js":"../output/Data.Foldable/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Functor.App/index.js":"../output/Data.Functor.App/index.js","../Data.Functor.Compose/index.js":"../output/Data.Functor.Compose/index.js","../Data.Functor.Coproduct/index.js":"../output/Data.Functor.Coproduct/index.js","../Data.Functor.Product/index.js":"../output/Data.Functor.Product/index.js","../Data.Identity/index.js":"../output/Data.Identity/index.js","../Data.Maybe/index.js":"../output/Data.Maybe/index.js","../Data.Maybe.First/index.js":"../output/Data.Maybe.First/index.js","../Data.Maybe.Last/index.js":"../output/Data.Maybe.Last/index.js","../Data.Monoid.Additive/index.js":"../output/Data.Monoid.Additive/index.js","../Data.Monoid.Conj/index.js":"../output/Data.Monoid.Conj/index.js","../Data.Monoid.Disj/index.js":"../output/Data.Monoid.Disj/index.js","../Data.Monoid.Dual/index.js":"../output/Data.Monoid.Dual/index.js","../Data.Monoid.Multiplicative/index.js":"../output/Data.Monoid.Multiplicative/index.js","../Data.Traversable.Accum/index.js":"../output/Data.Traversable.Accum/index.js","../Data.Traversable.Accum.Internal/index.js":"../output/Data.Traversable.Accum.Internal/index.js","../Data.Tuple/index.js":"../output/Data.Tuple/index.js"}],"../output/Control.Parallel/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "ParCont", {
  enumerable: true,
  get: function () {
    return Control_Parallel_Class.ParCont;
  }
});
exports.parTraverse_ = exports.parTraverse = exports.parSequence_ = exports.parSequence = exports.parOneOfMap = exports.parOneOf = exports.parApply = void 0;
Object.defineProperty(exports, "parallel", {
  enumerable: true,
  get: function () {
    return Control_Parallel_Class.parallel;
  }
});
Object.defineProperty(exports, "sequential", {
  enumerable: true,
  get: function () {
    return Control_Parallel_Class.sequential;
  }
});

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Control_Parallel_Class = _interopRequireWildcard(require("../Control.Parallel.Class/index.js"));

var Data_Foldable = _interopRequireWildcard(require("../Data.Foldable/index.js"));

var Data_Traversable = _interopRequireWildcard(require("../Data.Traversable/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var identity =
/* #__PURE__ */
Control_Category.identity(Control_Category.categoryFn);

var parTraverse_ = function parTraverse_(dictParallel) {
  var sequential = Control_Parallel_Class.sequential(dictParallel);
  var parallel = Control_Parallel_Class.parallel(dictParallel);
  return function (dictApplicative) {
    var traverse_ = Data_Foldable.traverse_(dictApplicative);
    return function (dictFoldable) {
      var traverse_1 = traverse_(dictFoldable);
      return function (f) {
        var $51 = traverse_1(function ($53) {
          return parallel(f($53));
        });
        return function ($52) {
          return sequential($51($52));
        };
      };
    };
  };
};

exports.parTraverse_ = parTraverse_;

var parTraverse = function parTraverse(dictParallel) {
  var sequential = Control_Parallel_Class.sequential(dictParallel);
  var parallel = Control_Parallel_Class.parallel(dictParallel);
  return function (dictApplicative) {
    return function (dictTraversable) {
      var traverse = Data_Traversable.traverse(dictTraversable)(dictApplicative);
      return function (f) {
        var $54 = traverse(function ($56) {
          return parallel(f($56));
        });
        return function ($55) {
          return sequential($54($55));
        };
      };
    };
  };
};

exports.parTraverse = parTraverse;

var parSequence_ = function parSequence_(dictParallel) {
  var parTraverse_1 = parTraverse_(dictParallel);
  return function (dictApplicative) {
    var parTraverse_2 = parTraverse_1(dictApplicative);
    return function (dictFoldable) {
      return parTraverse_2(dictFoldable)(identity);
    };
  };
};

exports.parSequence_ = parSequence_;

var parSequence = function parSequence(dictParallel) {
  var parTraverse1 = parTraverse(dictParallel);
  return function (dictApplicative) {
    var parTraverse2 = parTraverse1(dictApplicative);
    return function (dictTraversable) {
      return parTraverse2(dictTraversable)(identity);
    };
  };
};

exports.parSequence = parSequence;

var parOneOfMap = function parOneOfMap(dictParallel) {
  var sequential = Control_Parallel_Class.sequential(dictParallel);
  var parallel = Control_Parallel_Class.parallel(dictParallel);
  return function (dictAlternative) {
    var Plus1 = dictAlternative.Plus1();
    return function (dictFoldable) {
      var oneOfMap = Data_Foldable.oneOfMap(dictFoldable)(Plus1);
      return function (dictFunctor) {
        return function (f) {
          var $57 = oneOfMap(function ($59) {
            return parallel(f($59));
          });
          return function ($58) {
            return sequential($57($58));
          };
        };
      };
    };
  };
};

exports.parOneOfMap = parOneOfMap;

var parOneOf = function parOneOf(dictParallel) {
  var sequential = Control_Parallel_Class.sequential(dictParallel);
  var parallel = Control_Parallel_Class.parallel(dictParallel);
  return function (dictAlternative) {
    var Plus1 = dictAlternative.Plus1();
    return function (dictFoldable) {
      var oneOfMap = Data_Foldable.oneOfMap(dictFoldable)(Plus1);
      return function (dictFunctor) {
        var $60 = oneOfMap(parallel);
        return function ($61) {
          return sequential($60($61));
        };
      };
    };
  };
};

exports.parOneOf = parOneOf;

var parApply = function parApply(dictParallel) {
  var sequential = Control_Parallel_Class.sequential(dictParallel);
  var apply = Control_Apply.apply(dictParallel.Apply1());
  var parallel = Control_Parallel_Class.parallel(dictParallel);
  return function (mf) {
    return function (ma) {
      return sequential(apply(parallel(mf))(parallel(ma)));
    };
  };
};

exports.parApply = parApply;
},{"../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Category/index.js":"../output/Control.Category/index.js","../Control.Parallel.Class/index.js":"../output/Control.Parallel.Class/index.js","../Data.Foldable/index.js":"../output/Data.Foldable/index.js","../Data.Traversable/index.js":"../output/Data.Traversable/index.js"}],"../output/Data.Time.Duration/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.toDuration = exports.showSeconds = exports.showMinutes = exports.showMilliseconds = exports.showHours = exports.showDays = exports.semigroupSeconds = exports.semigroupMinutes = exports.semigroupMilliseconds = exports.semigroupHours = exports.semigroupDays = exports.ordSeconds = exports.ordMinutes = exports.ordMilliseconds = exports.ordHours = exports.ordDays = exports.newtypeSeconds = exports.newtypeMinutes = exports.newtypeMilliseconds = exports.newtypeHours = exports.newtypeDays = exports.negateDuration = exports.monoidSeconds = exports.monoidMinutes = exports.monoidMilliseconds = exports.monoidHours = exports.monoidDays = exports.fromDuration = exports.eqSeconds = exports.eqMinutes = exports.eqMilliseconds = exports.eqHours = exports.eqDays = exports.durationSeconds = exports.durationMinutes = exports.durationMilliseconds = exports.durationHours = exports.durationDays = exports.convertDuration = exports.Seconds = exports.Minutes = exports.Milliseconds = exports.Hours = exports.Days = void 0;

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Data_Eq = _interopRequireWildcard(require("../Data.Eq/index.js"));

var Data_Newtype = _interopRequireWildcard(require("../Data.Newtype/index.js"));

var Data_Ord = _interopRequireWildcard(require("../Data.Ord/index.js"));

var Data_Ring = _interopRequireWildcard(require("../Data.Ring/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var _show =
/* #__PURE__ */
Data_Show.show(Data_Show.showNumber);

var over =
/* #__PURE__ */
Data_Newtype.over()();
var negate =
/* #__PURE__ */
Data_Ring.negate(Data_Ring.ringNumber);
var identity =
/* #__PURE__ */
Control_Category.identity(Control_Category.categoryFn);

var Seconds = function Seconds(x) {
  return x;
};

exports.Seconds = Seconds;

var Minutes = function Minutes(x) {
  return x;
};

exports.Minutes = Minutes;

var Milliseconds = function Milliseconds(x) {
  return x;
};

exports.Milliseconds = Milliseconds;

var Hours = function Hours(x) {
  return x;
};

exports.Hours = Hours;

var Days = function Days(x) {
  return x;
};

exports.Days = Days;

var toDuration = function toDuration(dict) {
  return dict.toDuration;
};

exports.toDuration = toDuration;
var showSeconds = {
  show: function show(v) {
    return "(Seconds " + (_show(v) + ")");
  }
};
exports.showSeconds = showSeconds;
var showMinutes = {
  show: function show(v) {
    return "(Minutes " + (_show(v) + ")");
  }
};
exports.showMinutes = showMinutes;
var showMilliseconds = {
  show: function show(v) {
    return "(Milliseconds " + (_show(v) + ")");
  }
};
exports.showMilliseconds = showMilliseconds;
var showHours = {
  show: function show(v) {
    return "(Hours " + (_show(v) + ")");
  }
};
exports.showHours = showHours;
var showDays = {
  show: function show(v) {
    return "(Days " + (_show(v) + ")");
  }
};
exports.showDays = showDays;
var semigroupSeconds = {
  append: function append(v) {
    return function (v1) {
      return v + v1;
    };
  }
};
exports.semigroupSeconds = semigroupSeconds;
var semigroupMinutes = {
  append: function append(v) {
    return function (v1) {
      return v + v1;
    };
  }
};
exports.semigroupMinutes = semigroupMinutes;
var semigroupMilliseconds = {
  append: function append(v) {
    return function (v1) {
      return v + v1;
    };
  }
};
exports.semigroupMilliseconds = semigroupMilliseconds;
var semigroupHours = {
  append: function append(v) {
    return function (v1) {
      return v + v1;
    };
  }
};
exports.semigroupHours = semigroupHours;
var semigroupDays = {
  append: function append(v) {
    return function (v1) {
      return v + v1;
    };
  }
};
exports.semigroupDays = semigroupDays;
var ordSeconds = Data_Ord.ordNumber;
exports.ordSeconds = ordSeconds;
var ordMinutes = Data_Ord.ordNumber;
exports.ordMinutes = ordMinutes;
var ordMilliseconds = Data_Ord.ordNumber;
exports.ordMilliseconds = ordMilliseconds;
var ordHours = Data_Ord.ordNumber;
exports.ordHours = ordHours;
var ordDays = Data_Ord.ordNumber;
exports.ordDays = ordDays;
var newtypeSeconds = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeSeconds = newtypeSeconds;
var newtypeMinutes = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeMinutes = newtypeMinutes;
var newtypeMilliseconds = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeMilliseconds = newtypeMilliseconds;
var newtypeHours = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeHours = newtypeHours;
var newtypeDays = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeDays = newtypeDays;
var monoidSeconds = {
  mempty: 0.0,
  Semigroup0: function Semigroup0() {
    return semigroupSeconds;
  }
};
exports.monoidSeconds = monoidSeconds;
var monoidMinutes = {
  mempty: 0.0,
  Semigroup0: function Semigroup0() {
    return semigroupMinutes;
  }
};
exports.monoidMinutes = monoidMinutes;
var monoidMilliseconds = {
  mempty: 0.0,
  Semigroup0: function Semigroup0() {
    return semigroupMilliseconds;
  }
};
exports.monoidMilliseconds = monoidMilliseconds;
var monoidHours = {
  mempty: 0.0,
  Semigroup0: function Semigroup0() {
    return semigroupHours;
  }
};
exports.monoidHours = monoidHours;
var monoidDays = {
  mempty: 0.0,
  Semigroup0: function Semigroup0() {
    return semigroupDays;
  }
};
exports.monoidDays = monoidDays;

var fromDuration = function fromDuration(dict) {
  return dict.fromDuration;
};

exports.fromDuration = fromDuration;

var negateDuration = function negateDuration(dictDuration) {
  var $57 = toDuration(dictDuration);
  var $58 = over(Milliseconds)(negate);
  var $59 = fromDuration(dictDuration);
  return function ($60) {
    return $57($58($59($60)));
  };
};

exports.negateDuration = negateDuration;
var eqSeconds = Data_Eq.eqNumber;
exports.eqSeconds = eqSeconds;
var eqMinutes = Data_Eq.eqNumber;
exports.eqMinutes = eqMinutes;
var eqMilliseconds = Data_Eq.eqNumber;
exports.eqMilliseconds = eqMilliseconds;
var eqHours = Data_Eq.eqNumber;
exports.eqHours = eqHours;
var eqDays = Data_Eq.eqNumber;
exports.eqDays = eqDays;
var durationSeconds = {
  fromDuration:
  /* #__PURE__ */
  over(Seconds)(function (v) {
    return v * 1000.0;
  }),
  toDuration:
  /* #__PURE__ */
  over(Milliseconds)(function (v) {
    return v / 1000.0;
  })
};
exports.durationSeconds = durationSeconds;
var durationMinutes = {
  fromDuration:
  /* #__PURE__ */
  over(Minutes)(function (v) {
    return v * 60000.0;
  }),
  toDuration:
  /* #__PURE__ */
  over(Milliseconds)(function (v) {
    return v / 60000.0;
  })
};
exports.durationMinutes = durationMinutes;
var durationMilliseconds = {
  fromDuration: identity,
  toDuration: identity
};
exports.durationMilliseconds = durationMilliseconds;
var durationHours = {
  fromDuration:
  /* #__PURE__ */
  over(Hours)(function (v) {
    return v * 3600000.0;
  }),
  toDuration:
  /* #__PURE__ */
  over(Milliseconds)(function (v) {
    return v / 3600000.0;
  })
};
exports.durationHours = durationHours;
var durationDays = {
  fromDuration:
  /* #__PURE__ */
  over(Days)(function (v) {
    return v * 8.64e7;
  }),
  toDuration:
  /* #__PURE__ */
  over(Milliseconds)(function (v) {
    return v / 8.64e7;
  })
};
exports.durationDays = durationDays;

var convertDuration = function convertDuration(dictDuration) {
  var fromDuration1 = fromDuration(dictDuration);
  return function (dictDuration1) {
    var $61 = toDuration(dictDuration1);
    return function ($62) {
      return $61(fromDuration1($62));
    };
  };
};

exports.convertDuration = convertDuration;
},{"../Control.Category/index.js":"../output/Control.Category/index.js","../Data.Eq/index.js":"../output/Data.Eq/index.js","../Data.Newtype/index.js":"../output/Data.Newtype/index.js","../Data.Ord/index.js":"../output/Data.Ord/index.js","../Data.Ring/index.js":"../output/Data.Ring/index.js","../Data.Show/index.js":"../output/Data.Show/index.js"}],"../output/Effect.Unsafe/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.unsafePerformEffect = void 0;

var unsafePerformEffect = function unsafePerformEffect(f) {
  return f();
};

exports.unsafePerformEffect = unsafePerformEffect;
},{}],"../output/Effect.Unsafe/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "unsafePerformEffect", {
  enumerable: true,
  get: function () {
    return $foreign.unsafePerformEffect;
  }
});

var $foreign = _interopRequireWildcard(require("./foreign.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }
},{"./foreign.js":"../output/Effect.Unsafe/foreign.js"}],"../output/Partial.Unsafe/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports._unsafePartial = void 0;

// module Partial.Unsafe
var _unsafePartial = function _unsafePartial(f) {
  return f();
};

exports._unsafePartial = _unsafePartial;
},{}],"../output/Partial/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports._crashWith = void 0;

// module Partial
var _crashWith = function _crashWith(msg) {
  throw new Error(msg);
};

exports._crashWith = _crashWith;
},{}],"../output/Partial/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.crashWith = exports.crash = void 0;

var $foreign = _interopRequireWildcard(require("./foreign.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var crashWith = function crashWith() {
  return $foreign["_crashWith"];
};

exports.crashWith = crashWith;
var crashWith1 =
/* #__PURE__ */
crashWith();

var crash = function crash() {
  return crashWith1("Partial.crash: partial function");
};

exports.crash = crash;
},{"./foreign.js":"../output/Partial/foreign.js"}],"../output/Partial.Unsafe/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.unsafePartial = exports.unsafeCrashWith = void 0;

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Partial = _interopRequireWildcard(require("../Partial/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var crashWith =
/* #__PURE__ */
Partial.crashWith();
var unsafePartial = $foreign["_unsafePartial"];
exports.unsafePartial = unsafePartial;

var unsafeCrashWith = function unsafeCrashWith(msg) {
  return unsafePartial(function () {
    return crashWith(msg);
  });
};

exports.unsafeCrashWith = unsafeCrashWith;
},{"./foreign.js":"../output/Partial.Unsafe/foreign.js","../Partial/index.js":"../output/Partial/index.js"}],"../output/Effect.Aff/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Canceler = void 0;
Object.defineProperty(exports, "Milliseconds", {
  enumerable: true,
  get: function () {
    return Data_Time_Duration.Milliseconds;
  }
});
exports.cancelWith = exports.bracket = exports.bindAff = exports.attempt = exports.applyParAff = exports.applyFiber = exports.applyAff = exports.applicativeParAff = exports.applicativeFiber = exports.applicativeAff = exports.apathize = exports.alternativeParAff = exports.altParAff = exports.altAff = void 0;
Object.defineProperty(exports, "catchError", {
  enumerable: true,
  get: function () {
    return Control_Monad_Error_Class.catchError;
  }
});
exports.effectCanceler = exports.delay = void 0;
Object.defineProperty(exports, "error", {
  enumerable: true,
  get: function () {
    return Effect_Exception.error;
  }
});
exports.functorParAff = exports.functorFiber = exports.functorAff = exports.forkAff = exports.finally = exports.fiberCanceler = void 0;
Object.defineProperty(exports, "generalBracket", {
  enumerable: true,
  get: function () {
    return $foreign.generalBracket;
  }
});
exports.lazyAff = exports.launchSuspendedAff = exports.launchAff_ = exports.launchAff = exports.killFiber = exports.joinFiber = exports.invincible = void 0;
Object.defineProperty(exports, "makeAff", {
  enumerable: true,
  get: function () {
    return $foreign.makeAff;
  }
});
Object.defineProperty(exports, "message", {
  enumerable: true,
  get: function () {
    return Effect_Exception.message;
  }
});
exports.nonCanceler = exports.newtypeCanceler = exports.never = exports.monoidParAff = exports.monoidCanceler = exports.monoidAff = exports.monadThrowAff = exports.monadSTAff = exports.monadRecAff = exports.monadErrorAff = exports.monadEffectAff = exports.monadAff = void 0;
Object.defineProperty(exports, "parallel", {
  enumerable: true,
  get: function () {
    return Control_Parallel_Class.parallel;
  }
});
exports.semigroupParAff = exports.semigroupCanceler = exports.semigroupAff = exports.runSuspendedAff = exports.runAff_ = exports.runAff = exports.plusParAff = exports.plusAff = exports.parallelAff = void 0;
Object.defineProperty(exports, "sequential", {
  enumerable: true,
  get: function () {
    return Control_Parallel_Class.sequential;
  }
});
exports.suspendAff = exports.supervise = void 0;
Object.defineProperty(exports, "throwError", {
  enumerable: true,
  get: function () {
    return Control_Monad_Error_Class.throwError;
  }
});
Object.defineProperty(exports, "try", {
  enumerable: true,
  get: function () {
    return Control_Monad_Error_Class.try;
  }
});

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Monad = _interopRequireWildcard(require("../Control.Monad/index.js"));

var Control_Monad_Error_Class = _interopRequireWildcard(require("../Control.Monad.Error.Class/index.js"));

var Control_Monad_Rec_Class = _interopRequireWildcard(require("../Control.Monad.Rec.Class/index.js"));

var Control_Monad_ST_Class = _interopRequireWildcard(require("../Control.Monad.ST.Class/index.js"));

var Control_Parallel = _interopRequireWildcard(require("../Control.Parallel/index.js"));

var Control_Parallel_Class = _interopRequireWildcard(require("../Control.Parallel.Class/index.js"));

var Control_Plus = _interopRequireWildcard(require("../Control.Plus/index.js"));

var Data_Either = _interopRequireWildcard(require("../Data.Either/index.js"));

var Data_Foldable = _interopRequireWildcard(require("../Data.Foldable/index.js"));

var Data_Function = _interopRequireWildcard(require("../Data.Function/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Time_Duration = _interopRequireWildcard(require("../Data.Time.Duration/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Effect = _interopRequireWildcard(require("../Effect/index.js"));

var Effect_Class = _interopRequireWildcard(require("../Effect.Class/index.js"));

var Effect_Exception = _interopRequireWildcard(require("../Effect.Exception/index.js"));

var Effect_Unsafe = _interopRequireWildcard(require("../Effect.Unsafe/index.js"));

var Partial_Unsafe = _interopRequireWildcard(require("../Partial.Unsafe/index.js"));

var Unsafe_Coerce = _interopRequireWildcard(require("../Unsafe.Coerce/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var $runtime_lazy = function $runtime_lazy(name, moduleName, init) {
  var state = 0;
  var val;
  return function (lineNumber) {
    if (state === 2) return val;
    if (state === 1) throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state = 1;
    val = init();
    state = 2;
    return val;
  };
};

var pure =
/* #__PURE__ */
Control_Applicative.pure(Effect.applicativeEffect);
var $$void =
/* #__PURE__ */
Data_Functor["void"](Effect.functorEffect);
var map =
/* #__PURE__ */
Data_Functor.map(Effect.functorEffect);
var pure1 =
/* #__PURE__ */
Control_Applicative.pure(Data_Either.applicativeEither);

var Fiber = function Fiber(x) {
  return x;
};

var FFIUtil = function FFIUtil(x) {
  return x;
};

var Canceler = function Canceler(x) {
  return x;
};

exports.Canceler = Canceler;
var suspendAff =
/* #__PURE__ */
$foreign["_fork"](false);
exports.suspendAff = suspendAff;
var newtypeCanceler = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeCanceler = newtypeCanceler;
var functorParAff = {
  map: $foreign["_parAffMap"]
};
exports.functorParAff = functorParAff;
var functorAff = {
  map: $foreign["_map"]
};
exports.functorAff = functorAff;
var map1 =
/* #__PURE__ */
Data_Functor.map(functorAff);
var forkAff =
/* #__PURE__ */
$foreign["_fork"](true);
exports.forkAff = forkAff;

var ffiUtil =
/* #__PURE__ */
function () {
  var unsafeFromRight = function unsafeFromRight(v) {
    if (v instanceof Data_Either.Right) {
      return v.value0;
    }

    ;

    if (v instanceof Data_Either.Left) {
      return Partial_Unsafe.unsafeCrashWith("unsafeFromRight: Left");
    }

    ;
    throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): " + [v.constructor.name]);
  };

  var unsafeFromLeft = function unsafeFromLeft(v) {
    if (v instanceof Data_Either.Left) {
      return v.value0;
    }

    ;

    if (v instanceof Data_Either.Right) {
      return Partial_Unsafe.unsafeCrashWith("unsafeFromLeft: Right");
    }

    ;
    throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): " + [v.constructor.name]);
  };

  var isLeft = function isLeft(v) {
    if (v instanceof Data_Either.Left) {
      return true;
    }

    ;

    if (v instanceof Data_Either.Right) {
      return false;
    }

    ;
    throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): " + [v.constructor.name]);
  };

  return {
    isLeft: isLeft,
    fromLeft: unsafeFromLeft,
    fromRight: unsafeFromRight,
    left: Data_Either.Left.create,
    right: Data_Either.Right.create
  };
}();

var makeFiber = function makeFiber(aff) {
  return $foreign["_makeFiber"](ffiUtil, aff);
};

var launchAff = function launchAff(aff) {
  return function __do() {
    var fiber = makeFiber(aff)();
    fiber.run();
    return fiber;
  };
};

exports.launchAff = launchAff;

var launchAff_ = function launchAff_($75) {
  return $$void(launchAff($75));
};

exports.launchAff_ = launchAff_;
var launchSuspendedAff = makeFiber;
exports.launchSuspendedAff = launchSuspendedAff;

var delay = function delay(v) {
  return $foreign["_delay"](Data_Either.Right.create, v);
};

exports.delay = delay;

var bracket = function bracket(acquire) {
  return function (completed) {
    return $foreign.generalBracket(acquire)({
      killed: Data_Function["const"](completed),
      failed: Data_Function["const"](completed),
      completed: Data_Function["const"](completed)
    });
  };
};

exports.bracket = bracket;
var applyParAff = {
  apply: $foreign["_parAffApply"],
  Functor0: function Functor0() {
    return functorParAff;
  }
};
exports.applyParAff = applyParAff;
var lift2 =
/* #__PURE__ */
Control_Apply.lift2(applyParAff);

var semigroupParAff = function semigroupParAff(dictSemigroup) {
  return {
    append: lift2(Data_Semigroup.append(dictSemigroup))
  };
};

exports.semigroupParAff = semigroupParAff;
var monadAff = {
  Applicative0: function Applicative0() {
    return applicativeAff;
  },
  Bind1: function Bind1() {
    return bindAff;
  }
};
exports.monadAff = monadAff;
var bindAff = {
  bind: $foreign["_bind"],
  Apply0: function Apply0() {
    return $lazy_applyAff(0);
  }
};
exports.bindAff = bindAff;
var applicativeAff = {
  pure: $foreign["_pure"],
  Apply0: function Apply0() {
    return $lazy_applyAff(0);
  }
};
exports.applicativeAff = applicativeAff;
var $lazy_applyAff =
/* #__PURE__ */
$runtime_lazy("applyAff", "Effect.Aff", function () {
  return {
    apply: Control_Monad.ap(monadAff),
    Functor0: function Functor0() {
      return functorAff;
    }
  };
});
var applyAff =
/* #__PURE__ */
$lazy_applyAff(73);
exports.applyAff = applyAff;
var pure2 =
/* #__PURE__ */
Control_Applicative.pure(applicativeAff);
var bind1 =
/* #__PURE__ */
Control_Bind.bind(bindAff);
var lift21 =
/* #__PURE__ */
Control_Apply.lift2(applyAff);

var _apply =
/* #__PURE__ */
Control_Apply.apply(applyAff);

var bindFlipped =
/* #__PURE__ */
Control_Bind.bindFlipped(bindAff);

var cancelWith = function cancelWith(aff) {
  return function (v) {
    return $foreign.generalBracket(pure2(Data_Unit.unit))({
      killed: function killed(e) {
        return function (v1) {
          return v(e);
        };
      },
      failed: Data_Function["const"](pure2),
      completed: Data_Function["const"](pure2)
    })(Data_Function["const"](aff));
  };
};

exports.cancelWith = cancelWith;

var $$finally = function $$finally(fin) {
  return function (a) {
    return bracket(pure2(Data_Unit.unit))(Data_Function["const"](fin))(Data_Function["const"](a));
  };
};

exports.finally = $$finally;

var invincible = function invincible(a) {
  return bracket(a)(Data_Function["const"](pure2(Data_Unit.unit)))(pure2);
};

exports.invincible = invincible;
var lazyAff = {
  defer: function defer(f) {
    return bind1(pure2(Data_Unit.unit))(f);
  }
};
exports.lazyAff = lazyAff;
var parallelAff = {
  parallel: Unsafe_Coerce.unsafeCoerce,
  sequential: $foreign["_sequential"],
  Apply0: function Apply0() {
    return applyAff;
  },
  Apply1: function Apply1() {
    return applyParAff;
  }
};
exports.parallelAff = parallelAff;
var parallel =
/* #__PURE__ */
Control_Parallel_Class.parallel(parallelAff);
var applicativeParAff = {
  pure: function pure($76) {
    return parallel(pure2($76));
  },
  Apply0: function Apply0() {
    return applyParAff;
  }
};
exports.applicativeParAff = applicativeParAff;
var pure3 =
/* #__PURE__ */
Control_Applicative.pure(applicativeParAff);
var parSequence_ =
/* #__PURE__ */
Control_Parallel.parSequence_(parallelAff)(applicativeParAff)(Data_Foldable.foldableArray);

var monoidParAff = function monoidParAff(dictMonoid) {
  var semigroupParAff1 = semigroupParAff(dictMonoid.Semigroup0());
  return {
    mempty: pure3(Data_Monoid.mempty(dictMonoid)),
    Semigroup0: function Semigroup0() {
      return semigroupParAff1;
    }
  };
};

exports.monoidParAff = monoidParAff;
var semigroupCanceler = {
  append: function append(v) {
    return function (v1) {
      return function (err) {
        return parSequence_([v(err), v1(err)]);
      };
    };
  }
};
exports.semigroupCanceler = semigroupCanceler;

var semigroupAff = function semigroupAff(dictSemigroup) {
  return {
    append: lift21(Data_Semigroup.append(dictSemigroup))
  };
};

exports.semigroupAff = semigroupAff;
var monadEffectAff = {
  liftEffect: $foreign["_liftEffect"],
  Monad0: function Monad0() {
    return monadAff;
  }
};
exports.monadEffectAff = monadEffectAff;
var liftEffect =
/* #__PURE__ */
Effect_Class.liftEffect(monadEffectAff);

var effectCanceler = function effectCanceler($77) {
  return Canceler(Data_Function["const"](liftEffect($77)));
};

exports.effectCanceler = effectCanceler;

var joinFiber = function joinFiber(v) {
  return $foreign.makeAff(function (k) {
    return map(effectCanceler)(v.join(k));
  });
};

exports.joinFiber = joinFiber;
var functorFiber = {
  map: function map(f) {
    return function (t) {
      return Effect_Unsafe.unsafePerformEffect(makeFiber(map1(f)(joinFiber(t))));
    };
  }
};
exports.functorFiber = functorFiber;
var applyFiber = {
  apply: function apply(t1) {
    return function (t2) {
      return Effect_Unsafe.unsafePerformEffect(makeFiber(_apply(joinFiber(t1))(joinFiber(t2))));
    };
  },
  Functor0: function Functor0() {
    return functorFiber;
  }
};
exports.applyFiber = applyFiber;
var applicativeFiber = {
  pure: function pure(a) {
    return Effect_Unsafe.unsafePerformEffect(makeFiber(pure2(a)));
  },
  Apply0: function Apply0() {
    return applyFiber;
  }
};
exports.applicativeFiber = applicativeFiber;

var killFiber = function killFiber(e) {
  return function (v) {
    return bind1(liftEffect(v.isSuspended))(function (suspended) {
      if (suspended) {
        return liftEffect($$void(v.kill(e, Data_Function["const"](pure(Data_Unit.unit)))));
      }

      ;
      return $foreign.makeAff(function (k) {
        return map(effectCanceler)(v.kill(e, k));
      });
    });
  };
};

exports.killFiber = killFiber;

var fiberCanceler =
/* #__PURE__ */
function () {
  var $78 = Data_Function.flip(killFiber);
  return function ($79) {
    return Canceler($78($79));
  };
}();

exports.fiberCanceler = fiberCanceler;

var supervise = function supervise(aff) {
  var killError = Effect_Exception.error("[Aff] Child fiber outlived parent");

  var killAll = function killAll(err) {
    return function (sup) {
      return $foreign.makeAff(function (k) {
        return $foreign["_killAll"](err, sup.supervisor, k(pure1(Data_Unit.unit)));
      });
    };
  };

  var acquire = function __do() {
    var sup = $foreign["_makeSupervisedFiber"](ffiUtil, aff)();
    sup.fiber.run();
    return sup;
  };

  return $foreign.generalBracket(liftEffect(acquire))({
    killed: function killed(err) {
      return function (sup) {
        return parSequence_([killFiber(err)(sup.fiber), killAll(err)(sup)]);
      };
    },
    failed: Data_Function["const"](killAll(killError)),
    completed: Data_Function["const"](killAll(killError))
  })(function ($80) {
    return joinFiber(function (v) {
      return v.fiber;
    }($80));
  });
};

exports.supervise = supervise;
var monadSTAff = {
  liftST:
  /* #__PURE__ */
  function () {
    var $81 = Control_Monad_ST_Class.liftST(Control_Monad_ST_Class.monadSTEffect);
    return function ($82) {
      return liftEffect($81($82));
    };
  }(),
  Monad0: function Monad0() {
    return monadAff;
  }
};
exports.monadSTAff = monadSTAff;
var monadThrowAff = {
  throwError: $foreign["_throwError"],
  Monad0: function Monad0() {
    return monadAff;
  }
};
exports.monadThrowAff = monadThrowAff;
var monadErrorAff = {
  catchError: $foreign["_catchError"],
  MonadThrow0: function MonadThrow0() {
    return monadThrowAff;
  }
};
exports.monadErrorAff = monadErrorAff;
var $$try =
/* #__PURE__ */
Control_Monad_Error_Class["try"](monadErrorAff);
var catchError =
/* #__PURE__ */
Control_Monad_Error_Class.catchError(monadErrorAff);
var attempt = $$try;
exports.attempt = attempt;

var runAff = function runAff(k) {
  return function (aff) {
    return launchAff(bindFlipped(function ($83) {
      return liftEffect(k($83));
    })($$try(aff)));
  };
};

exports.runAff = runAff;

var runAff_ = function runAff_(k) {
  return function (aff) {
    return $$void(runAff(k)(aff));
  };
};

exports.runAff_ = runAff_;

var runSuspendedAff = function runSuspendedAff(k) {
  return function (aff) {
    return launchSuspendedAff(bindFlipped(function ($84) {
      return liftEffect(k($84));
    })($$try(aff)));
  };
};

exports.runSuspendedAff = runSuspendedAff;
var monadRecAff = {
  tailRecM: function tailRecM(k) {
    var go = function go(a) {
      return bind1(k(a))(function (res) {
        if (res instanceof Control_Monad_Rec_Class.Done) {
          return pure2(res.value0);
        }

        ;

        if (res instanceof Control_Monad_Rec_Class.Loop) {
          return go(res.value0);
        }

        ;
        throw new Error("Failed pattern match at Effect.Aff (line 104, column 7 - line 106, column 23): " + [res.constructor.name]);
      });
    };

    return go;
  },
  Monad0: function Monad0() {
    return monadAff;
  }
};
exports.monadRecAff = monadRecAff;

var monoidAff = function monoidAff(dictMonoid) {
  var semigroupAff1 = semigroupAff(dictMonoid.Semigroup0());
  return {
    mempty: pure2(Data_Monoid.mempty(dictMonoid)),
    Semigroup0: function Semigroup0() {
      return semigroupAff1;
    }
  };
};

exports.monoidAff = monoidAff;
var nonCanceler =
/* #__PURE__ */
Data_Function["const"](
/* #__PURE__ */
pure2(Data_Unit.unit));
exports.nonCanceler = nonCanceler;
var monoidCanceler = {
  mempty: nonCanceler,
  Semigroup0: function Semigroup0() {
    return semigroupCanceler;
  }
};
exports.monoidCanceler = monoidCanceler;
var mempty =
/* #__PURE__ */
Data_Monoid.mempty(monoidCanceler);
var never =
/* #__PURE__ */
$foreign.makeAff(function (v) {
  return pure(mempty);
});
exports.never = never;

var apathize =
/* #__PURE__ */
function () {
  var $85 = map1(Data_Function["const"](Data_Unit.unit));
  return function ($86) {
    return $85(attempt($86));
  };
}();

exports.apathize = apathize;
var altParAff = {
  alt: $foreign["_parAffAlt"],
  Functor0: function Functor0() {
    return functorParAff;
  }
};
exports.altParAff = altParAff;
var altAff = {
  alt: function alt(a1) {
    return function (a2) {
      return catchError(a1)(Data_Function["const"](a2));
    };
  },
  Functor0: function Functor0() {
    return functorAff;
  }
};
exports.altAff = altAff;
var plusAff = {
  empty:
  /* #__PURE__ */
  Control_Monad_Error_Class.throwError(monadThrowAff)(
  /* #__PURE__ */
  Effect_Exception.error("Always fails")),
  Alt0: function Alt0() {
    return altAff;
  }
};
exports.plusAff = plusAff;
var plusParAff = {
  empty:
  /* #__PURE__ */
  parallel(
  /* #__PURE__ */
  Control_Plus.empty(plusAff)),
  Alt0: function Alt0() {
    return altParAff;
  }
};
exports.plusParAff = plusParAff;
var alternativeParAff = {
  Applicative0: function Applicative0() {
    return applicativeParAff;
  },
  Plus1: function Plus1() {
    return plusParAff;
  }
};
exports.alternativeParAff = alternativeParAff;
},{"./foreign.js":"../output/Effect.Aff/foreign.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Monad/index.js":"../output/Control.Monad/index.js","../Control.Monad.Error.Class/index.js":"../output/Control.Monad.Error.Class/index.js","../Control.Monad.Rec.Class/index.js":"../output/Control.Monad.Rec.Class/index.js","../Control.Monad.ST.Class/index.js":"../output/Control.Monad.ST.Class/index.js","../Control.Parallel/index.js":"../output/Control.Parallel/index.js","../Control.Parallel.Class/index.js":"../output/Control.Parallel.Class/index.js","../Control.Plus/index.js":"../output/Control.Plus/index.js","../Data.Either/index.js":"../output/Data.Either/index.js","../Data.Foldable/index.js":"../output/Data.Foldable/index.js","../Data.Function/index.js":"../output/Data.Function/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Time.Duration/index.js":"../output/Data.Time.Duration/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Effect/index.js":"../output/Effect/index.js","../Effect.Class/index.js":"../output/Effect.Class/index.js","../Effect.Exception/index.js":"../output/Effect.Exception/index.js","../Effect.Unsafe/index.js":"../output/Effect.Unsafe/index.js","../Partial.Unsafe/index.js":"../output/Partial.Unsafe/index.js","../Unsafe.Coerce/index.js":"../output/Unsafe.Coerce/index.js"}],"../output/Effect.Console/foreign.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.warn = exports.timeLog = exports.timeEnd = exports.time = exports.log = exports.info = exports.error = exports.debug = exports.clear = void 0;

var log = function log(s) {
  return function () {
    console.log(s);
  };
};

exports.log = log;

var warn = function warn(s) {
  return function () {
    console.warn(s);
  };
};

exports.warn = warn;

var error = function error(s) {
  return function () {
    console.error(s);
  };
};

exports.error = error;

var info = function info(s) {
  return function () {
    console.info(s);
  };
};

exports.info = info;

var debug = function debug(s) {
  return function () {
    console.debug(s);
  };
};

exports.debug = debug;

var time = function time(s) {
  return function () {
    console.time(s);
  };
};

exports.time = time;

var timeLog = function timeLog(s) {
  return function () {
    console.timeLog(s);
  };
};

exports.timeLog = timeLog;

var timeEnd = function timeEnd(s) {
  return function () {
    console.timeEnd(s);
  };
};

exports.timeEnd = timeEnd;

var clear = function clear() {
  console.clear();
};

exports.clear = clear;
},{}],"../output/Effect.Console/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
Object.defineProperty(exports, "clear", {
  enumerable: true,
  get: function () {
    return $foreign.clear;
  }
});
Object.defineProperty(exports, "debug", {
  enumerable: true,
  get: function () {
    return $foreign.debug;
  }
});
exports.debugShow = void 0;
Object.defineProperty(exports, "error", {
  enumerable: true,
  get: function () {
    return $foreign.error;
  }
});
exports.errorShow = void 0;
Object.defineProperty(exports, "info", {
  enumerable: true,
  get: function () {
    return $foreign.info;
  }
});
exports.infoShow = void 0;
Object.defineProperty(exports, "log", {
  enumerable: true,
  get: function () {
    return $foreign.log;
  }
});
exports.logShow = void 0;
Object.defineProperty(exports, "time", {
  enumerable: true,
  get: function () {
    return $foreign.time;
  }
});
Object.defineProperty(exports, "timeEnd", {
  enumerable: true,
  get: function () {
    return $foreign.timeEnd;
  }
});
Object.defineProperty(exports, "timeLog", {
  enumerable: true,
  get: function () {
    return $foreign.timeLog;
  }
});
Object.defineProperty(exports, "warn", {
  enumerable: true,
  get: function () {
    return $foreign.warn;
  }
});
exports.warnShow = void 0;

var $foreign = _interopRequireWildcard(require("./foreign.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var warnShow = function warnShow(dictShow) {
  var show = Data_Show.show(dictShow);
  return function (a) {
    return $foreign.warn(show(a));
  };
};

exports.warnShow = warnShow;

var logShow = function logShow(dictShow) {
  var show = Data_Show.show(dictShow);
  return function (a) {
    return $foreign.log(show(a));
  };
};

exports.logShow = logShow;

var infoShow = function infoShow(dictShow) {
  var show = Data_Show.show(dictShow);
  return function (a) {
    return $foreign.info(show(a));
  };
};

exports.infoShow = infoShow;

var errorShow = function errorShow(dictShow) {
  var show = Data_Show.show(dictShow);
  return function (a) {
    return $foreign.error(show(a));
  };
};

exports.errorShow = errorShow;

var debugShow = function debugShow(dictShow) {
  var show = Data_Show.show(dictShow);
  return function (a) {
    return $foreign.debug(show(a));
  };
};

exports.debugShow = debugShow;
},{"./foreign.js":"../output/Effect.Console/foreign.js","../Data.Show/index.js":"../output/Data.Show/index.js"}],"../output/Aff.Util/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.affLog = void 0;

var Effect_Aff = _interopRequireWildcard(require("../Effect.Aff/index.js"));

var Effect_Class = _interopRequireWildcard(require("../Effect.Class/index.js"));

var Effect_Console = _interopRequireWildcard(require("../Effect.Console/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var affLog =
/* #__PURE__ */
function () {
  var $2 = Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
  return function ($3) {
    return $2(Effect_Console.log($3));
  };
}();

exports.affLog = affLog;
},{"../Effect.Aff/index.js":"../output/Effect.Aff/index.js","../Effect.Class/index.js":"../output/Effect.Class/index.js","../Effect.Console/index.js":"../output/Effect.Console/index.js"}],"../output/Effect.Class.Console/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.warnShow = exports.warn = exports.timeLog = exports.timeEnd = exports.time = exports.logShow = exports.log = exports.infoShow = exports.info = exports.errorShow = exports.error = exports.debugShow = exports.debug = exports.clear = void 0;

var Effect_Class = _interopRequireWildcard(require("../Effect.Class/index.js"));

var Effect_Console = _interopRequireWildcard(require("../Effect.Console/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var warnShow = function warnShow(dictMonadEffect) {
  var liftEffect = Effect_Class.liftEffect(dictMonadEffect);
  return function (dictShow) {
    var $39 = Effect_Console.warnShow(dictShow);
    return function ($40) {
      return liftEffect($39($40));
    };
  };
};

exports.warnShow = warnShow;

var warn = function warn(dictMonadEffect) {
  var $41 = Effect_Class.liftEffect(dictMonadEffect);
  return function ($42) {
    return $41(Effect_Console.warn($42));
  };
};

exports.warn = warn;

var timeLog = function timeLog(dictMonadEffect) {
  var $43 = Effect_Class.liftEffect(dictMonadEffect);
  return function ($44) {
    return $43(Effect_Console.timeLog($44));
  };
};

exports.timeLog = timeLog;

var timeEnd = function timeEnd(dictMonadEffect) {
  var $45 = Effect_Class.liftEffect(dictMonadEffect);
  return function ($46) {
    return $45(Effect_Console.timeEnd($46));
  };
};

exports.timeEnd = timeEnd;

var time = function time(dictMonadEffect) {
  var $47 = Effect_Class.liftEffect(dictMonadEffect);
  return function ($48) {
    return $47(Effect_Console.time($48));
  };
};

exports.time = time;

var logShow = function logShow(dictMonadEffect) {
  var liftEffect = Effect_Class.liftEffect(dictMonadEffect);
  return function (dictShow) {
    var $49 = Effect_Console.logShow(dictShow);
    return function ($50) {
      return liftEffect($49($50));
    };
  };
};

exports.logShow = logShow;

var log = function log(dictMonadEffect) {
  var $51 = Effect_Class.liftEffect(dictMonadEffect);
  return function ($52) {
    return $51(Effect_Console.log($52));
  };
};

exports.log = log;

var infoShow = function infoShow(dictMonadEffect) {
  var liftEffect = Effect_Class.liftEffect(dictMonadEffect);
  return function (dictShow) {
    var $53 = Effect_Console.infoShow(dictShow);
    return function ($54) {
      return liftEffect($53($54));
    };
  };
};

exports.infoShow = infoShow;

var info = function info(dictMonadEffect) {
  var $55 = Effect_Class.liftEffect(dictMonadEffect);
  return function ($56) {
    return $55(Effect_Console.info($56));
  };
};

exports.info = info;

var errorShow = function errorShow(dictMonadEffect) {
  var liftEffect = Effect_Class.liftEffect(dictMonadEffect);
  return function (dictShow) {
    var $57 = Effect_Console.errorShow(dictShow);
    return function ($58) {
      return liftEffect($57($58));
    };
  };
};

exports.errorShow = errorShow;

var error = function error(dictMonadEffect) {
  var $59 = Effect_Class.liftEffect(dictMonadEffect);
  return function ($60) {
    return $59(Effect_Console.error($60));
  };
};

exports.error = error;

var debugShow = function debugShow(dictMonadEffect) {
  var liftEffect = Effect_Class.liftEffect(dictMonadEffect);
  return function (dictShow) {
    var $61 = Effect_Console.debugShow(dictShow);
    return function ($62) {
      return liftEffect($61($62));
    };
  };
};

exports.debugShow = debugShow;

var debug = function debug(dictMonadEffect) {
  var $63 = Effect_Class.liftEffect(dictMonadEffect);
  return function ($64) {
    return $63(Effect_Console.debug($64));
  };
};

exports.debug = debug;

var clear = function clear(dictMonadEffect) {
  return Effect_Class.liftEffect(dictMonadEffect)(Effect_Console.clear);
};

exports.clear = clear;
},{"../Effect.Class/index.js":"../output/Effect.Class/index.js","../Effect.Console/index.js":"../output/Effect.Console/index.js"}],"../output/Aff.Apathize/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.example = void 0;

var Aff_Util = _interopRequireWildcard(require("../Aff.Util/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Effect_Aff = _interopRequireWildcard(require("../Effect.Aff/index.js"));

var Effect_Class_Console = _interopRequireWildcard(require("../Effect.Class.Console/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var example =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(
/* #__PURE__ */
Effect_Aff.apathize(
/* #__PURE__ */
Effect_Aff.forkAff(
/* #__PURE__ */
Effect_Class_Console.error(Effect_Aff.monadEffectAff)("error"))))(function () {
  return Aff_Util.affLog("apathize");
});
exports.example = example;
},{"../Aff.Util/index.js":"../output/Aff.Util/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Effect.Aff/index.js":"../output/Effect.Aff/index.js","../Effect.Class.Console/index.js":"../output/Effect.Class.Console/index.js"}],"../output/Aff.Attempt/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.example = void 0;

var Aff_Util = _interopRequireWildcard(require("../Aff.Util/index.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Data_Either = _interopRequireWildcard(require("../Data.Either/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Effect_Aff = _interopRequireWildcard(require("../Effect.Aff/index.js"));

var Effect_Class_Console = _interopRequireWildcard(require("../Effect.Class.Console/index.js"));

var Effect_Exception = _interopRequireWildcard(require("../Effect.Exception/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var pure =
/* #__PURE__ */
Control_Applicative.pure(Effect_Aff.applicativeAff);
var example =
/* #__PURE__ */
Control_Bind.bind(Effect_Aff.bindAff)(
/* #__PURE__ */
Effect_Aff.attempt(
/* #__PURE__ */
Effect_Aff.forkAff(
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(
/* #__PURE__ */
Effect_Class_Console.error(Effect_Aff.monadEffectAff)("error"))(function () {
  return pure("unit");
}))))(function (v) {
  if (v instanceof Data_Either.Left) {
    return Aff_Util.affLog(Effect_Exception.message(v.value0));
  }

  ;

  if (v instanceof Data_Either.Right) {
    return pure(Data_Unit.unit);
  }

  ;
  throw new Error("Failed pattern match at Aff.Attempt (line 16, column 3 - line 18, column 25): " + [v.constructor.name]);
});
exports.example = example;
},{"../Aff.Util/index.js":"../output/Aff.Util/index.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Data.Either/index.js":"../output/Data.Either/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Effect.Aff/index.js":"../output/Effect.Aff/index.js","../Effect.Class.Console/index.js":"../output/Effect.Class.Console/index.js","../Effect.Exception/index.js":"../output/Effect.Exception/index.js"}],"../output/Aff.Bracket/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.example = void 0;

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Effect_Aff = _interopRequireWildcard(require("../Effect.Aff/index.js"));

var Effect_Class = _interopRequireWildcard(require("../Effect.Class/index.js"));

var Effect_Console = _interopRequireWildcard(require("../Effect.Console/index.js"));

var Effect_Ref = _interopRequireWildcard(require("../Effect.Ref/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var bind =
/* #__PURE__ */
Control_Bind.bind(Effect_Aff.bindAff);
var liftEffect =
/* #__PURE__ */
Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
var discard =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff);
var pure =
/* #__PURE__ */
Control_Applicative.pure(Effect_Aff.applicativeAff);
var $$void =
/* #__PURE__ */
Data_Functor["void"](Effect_Aff.functorAff);
var example =
/* #__PURE__ */
bind(
/* #__PURE__ */
liftEffect(
/* #__PURE__ */
Effect_Ref["new"]("")))(function (ref) {
  var action = function action(s) {
    return discard(Effect_Aff.delay(100.0))(function () {
      return bind(liftEffect(Effect_Ref.modify(function (v) {
        return v + s;
      })(ref)))(function () {
        return pure(s);
      });
    });
  };

  return bind(Effect_Aff.bracket(action("1"))(function (s) {
    return $$void(action(s + "3"));
  })(function (s) {
    return action(s + "2");
  }))(function () {
    return bind(liftEffect(Effect_Ref.read(ref)))(function (value) {
      return liftEffect(Effect_Console.log(value));
    });
  });
});
exports.example = example;
},{"../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Effect.Aff/index.js":"../output/Effect.Aff/index.js","../Effect.Class/index.js":"../output/Effect.Class/index.js","../Effect.Console/index.js":"../output/Effect.Console/index.js","../Effect.Ref/index.js":"../output/Effect.Ref/index.js"}],"../output/Aff.Delay/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.example = void 0;

var Aff_Util = _interopRequireWildcard(require("../Aff.Util/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Effect_Aff = _interopRequireWildcard(require("../Effect.Aff/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var discard =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff);
var example =
/* #__PURE__ */
discard(
/* #__PURE__ */
Aff_Util.affLog("Before Delay"))(function () {
  return discard(Effect_Aff.delay(500.0))(function () {
    return Aff_Util.affLog("After Delay");
  });
});
exports.example = example;
},{"../Aff.Util/index.js":"../output/Aff.Util/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Effect.Aff/index.js":"../output/Effect.Aff/index.js"}],"../output/Aff.Finally/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.example = void 0;

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Monad_Error_Class = _interopRequireWildcard(require("../Control.Monad.Error.Class/index.js"));

var Data_Either = _interopRequireWildcard(require("../Data.Either/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Effect_Aff = _interopRequireWildcard(require("../Effect.Aff/index.js"));

var Effect_Console = _interopRequireWildcard(require("../Effect.Console/index.js"));

var Effect_Exception = _interopRequireWildcard(require("../Effect.Exception/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var mempty =
/* #__PURE__ */
Data_Monoid.mempty(Effect_Aff.monoidCanceler);
var pure1 =
/* #__PURE__ */
Control_Applicative.pure(Effect_Aff.applicativeAff);

var example =
/* #__PURE__ */
function () {
  var finalizer = Effect_Aff.makeAff(function (callback) {
    return function __do() {
      Effect_Console.log("finalize")();
      callback(new Data_Either.Right(Data_Unit.unit))();
      return mempty;
    };
  });
  var a = Effect_Aff.makeAff(function (callback) {
    return function __do() {
      Effect_Console.log("call failed fn")();
      callback(new Data_Either.Left(Effect_Exception.error("fail")))();
      return mempty;
    };
  });
  return Control_Bind.bind(Effect_Aff.bindAff)(Control_Monad_Error_Class["try"](Effect_Aff.monadErrorAff)(Effect_Aff["finally"](finalizer)(a)))(function () {
    return pure1(Data_Unit.unit);
  });
}();

exports.example = example;
},{"../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Monad.Error.Class/index.js":"../output/Control.Monad.Error.Class/index.js","../Data.Either/index.js":"../output/Data.Either/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Effect.Aff/index.js":"../output/Effect.Aff/index.js","../Effect.Console/index.js":"../output/Effect.Console/index.js","../Effect.Exception/index.js":"../output/Effect.Exception/index.js"}],"../output/Aff.Fork/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.example = void 0;

var Aff_Util = _interopRequireWildcard(require("../Aff.Util/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Data_Traversable = _interopRequireWildcard(require("../Data.Traversable/index.js"));

var Effect_Aff = _interopRequireWildcard(require("../Effect.Aff/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var discard =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff);
var bind =
/* #__PURE__ */
Control_Bind.bind(Effect_Aff.bindAff);
var traverse =
/* #__PURE__ */
Data_Traversable.traverse(Data_Traversable.traversableArray)(Effect_Aff.applicativeAff);
var example =
/* #__PURE__ */
discard(
/* #__PURE__ */
Aff_Util.affLog("Parent Start"))(function () {
  return bind(Effect_Aff.forkAff(discard(Effect_Aff.delay(500.0))(function () {
    return Aff_Util.affLog("Call Child1");
  })))(function (a) {
    return bind(Effect_Aff.forkAff(Aff_Util.affLog("Call Child2")))(function (b) {
      return bind(Effect_Aff.forkAff(discard(Effect_Aff.delay(200.0))(function () {
        return Aff_Util.affLog("Call Child3");
      })))(function (c) {
        return bind(traverse(Effect_Aff.joinFiber)([a, b, c]))(function () {
          return Aff_Util.affLog("Parent End");
        });
      });
    });
  });
});
exports.example = example;
},{"../Aff.Util/index.js":"../output/Aff.Util/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Data.Traversable/index.js":"../output/Data.Traversable/index.js","../Effect.Aff/index.js":"../output/Effect.Aff/index.js"}],"../output/Aff.Invincible/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.example = void 0;

var Aff_Util = _interopRequireWildcard(require("../Aff.Util/index.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Effect_Aff = _interopRequireWildcard(require("../Effect.Aff/index.js"));

var Effect_Exception = _interopRequireWildcard(require("../Effect.Exception/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

var bind =
/* #__PURE__ */
Control_Bind.bind(Effect_Aff.bindAff);
var discard =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff);
var pure =
/* #__PURE__ */
Control_Applicative.pure(Effect_Aff.applicativeAff);
var example =
/* #__PURE__ */
bind(
/* #__PURE__ */
Effect_Aff.forkAff(
/* #__PURE__ */
Effect_Aff.invincible(
/* #__PURE__ */
discard(
/* #__PURE__ */
Aff_Util.affLog("invincible fun start."))(function () {
  return discard(Effect_Aff.delay(200.0))(function () {
    return Aff_Util.affLog("invincible fun end.");
  });
}))))(function (a) {
  return bind(Effect_Aff.killFiber(Effect_Exception.error("kill"))(a))(function () {
    return pure(Data_Unit.unit);
  });
});
exports.example = example;
},{"../Aff.Util/index.js":"../output/Aff.Util/index.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Effect.Aff/index.js":"../output/Effect.Aff/index.js","../Effect.Exception/index.js":"../output/Effect.Exception/index.js"}],"../output/Aff.Kill/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.example = void 0;

var Aff_Util = _interopRequireWildcard(require("../Aff.Util/index.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Monad_Error_Class = _interopRequireWildcard(require("../Control.Monad.Error.Class/index.js"));

var Data_Bifunctor = _interopRequireWildcard(require("../Data.Bifunctor/index.js"));

var Data_Either = _interopRequireWildcard(require("../Data.Either/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Effect = _interopRequireWildcard(require("../Effect/index.js"));

var Effect_Aff = _interopRequireWildcard(require("../Effect.Aff/index.js"));

var Effect_Exception = _interopRequireWildcard(require("../Effect.Exception/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var bind =
/* #__PURE__ */
Control_Bind.bind(Effect_Aff.bindAff);
var discard =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff);
var pure =
/* #__PURE__ */
Control_Applicative.pure(Effect.applicativeEffect);
var $$try =
/* #__PURE__ */
Control_Monad_Error_Class["try"](Effect_Aff.monadErrorAff);
var lmap =
/* #__PURE__ */
Data_Bifunctor.lmap(Data_Bifunctor.bifunctorEither);
var pure1 =
/* #__PURE__ */
Control_Applicative.pure(Effect_Aff.applicativeAff);
var example =
/* #__PURE__ */
bind(
/* #__PURE__ */
Effect_Aff.forkAff(
/* #__PURE__ */
discard(
/* #__PURE__ */
Aff_Util.affLog("child start."))(function () {
  return bind(Effect_Aff.makeAff(function (v) {
    return pure(function (v1) {
      return Aff_Util.affLog("Cancelled.");
    });
  }))(function () {
    return Aff_Util.affLog("child end.");
  });
})))(function (fiber) {
  return discard(Effect_Aff.delay(50.0))(function () {
    return discard(Effect_Aff.killFiber(Effect_Exception.error("Nope"))(fiber))(function () {
      return bind($$try(Effect_Aff.joinFiber(fiber)))(function (res) {
        var v = lmap(Effect_Exception.message)(res);

        if (v instanceof Data_Either.Left) {
          return Aff_Util.affLog(v.value0);
        }

        ;

        if (v instanceof Data_Either.Right) {
          return pure1(Data_Unit.unit);
        }

        ;
        throw new Error("Failed pattern match at Aff.Kill (line 27, column 3 - line 29, column 25): " + [v.constructor.name]);
      });
    });
  });
});
exports.example = example;
},{"../Aff.Util/index.js":"../output/Aff.Util/index.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Monad.Error.Class/index.js":"../output/Control.Monad.Error.Class/index.js","../Data.Bifunctor/index.js":"../output/Data.Bifunctor/index.js","../Data.Either/index.js":"../output/Data.Either/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Effect/index.js":"../output/Effect/index.js","../Effect.Aff/index.js":"../output/Effect.Aff/index.js","../Effect.Exception/index.js":"../output/Effect.Exception/index.js"}],"../output/Aff.LaunchAff/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.example = void 0;

var Aff_Util = _interopRequireWildcard(require("../Aff.Util/index.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Effect_Aff = _interopRequireWildcard(require("../Effect.Aff/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var discard =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit);
var discard1 =
/* #__PURE__ */
discard(Effect_Aff.bindAff);
var pure =
/* #__PURE__ */
Control_Applicative.pure(Effect_Aff.applicativeAff);
var bindFlipped =
/* #__PURE__ */
Control_Bind.bindFlipped(Effect_Aff.bindAff);

var example = function __do() {
  var suspendedFiber = Effect_Aff.launchSuspendedAff(discard1(Aff_Util.affLog("launchSuspendedAff"))(function () {
    return pure("launchSuspendedAff result");
  }))();
  var fiber = Effect_Aff.launchAff(discard1(Aff_Util.affLog("launchAff"))(function () {
    return pure("launchAff result");
  }))();
  Effect_Aff.launchAff_(Aff_Util.affLog("launchAff_"))();
  Effect_Aff.launchAff_(bindFlipped(Aff_Util.affLog)(Effect_Aff.joinFiber(fiber)))();
  return Effect_Aff.launchAff_(bindFlipped(Aff_Util.affLog)(Effect_Aff.joinFiber(suspendedFiber)))();
};

exports.example = example;
},{"../Aff.Util/index.js":"../output/Aff.Util/index.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Effect.Aff/index.js":"../output/Effect.Aff/index.js"}],"../output/Aff.MakeAff/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.example = void 0;

var Aff_Util = _interopRequireWildcard(require("../Aff.Util/index.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Data_Either = _interopRequireWildcard(require("../Data.Either/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Effect_Aff = _interopRequireWildcard(require("../Effect.Aff/index.js"));

var Effect_Console = _interopRequireWildcard(require("../Effect.Console/index.js"));

var Effect_Exception = _interopRequireWildcard(require("../Effect.Exception/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var bind =
/* #__PURE__ */
Control_Bind.bind(Effect_Aff.bindAff);
var discard =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit);
var discard2 =
/* #__PURE__ */
discard(Effect_Aff.bindAff);
var pure1 =
/* #__PURE__ */
Control_Applicative.pure(Effect_Aff.applicativeAff);
var example =
/* #__PURE__ */
bind(
/* #__PURE__ */
Effect_Aff.forkAff(
/* #__PURE__ */
bind(
/* #__PURE__ */
Effect_Aff.makeAff(function (callback) {
  return function __do() {
    Effect_Console.log("use effect 1.")();
    callback(new Data_Either.Right("Done 1"))();
    return Effect_Aff.nonCanceler;
  };
}))(function (a) {
  return Aff_Util.affLog(a);
})))(function (fiber1) {
  return discard2(Effect_Aff.killFiber(Effect_Exception.error("cancel"))(fiber1))(function () {
    return bind(Effect_Aff.forkAff(bind(Effect_Aff.makeAff(function (callback) {
      return function __do() {
        Effect_Console.log("use effect 2.")();
        callback(new Data_Either.Right("Done 2"))();
        return function (e) {
          return Aff_Util.affLog(Effect_Exception.message(e));
        };
      };
    }))(function (b) {
      return Aff_Util.affLog(b);
    })))(function (fiber2) {
      return discard2(Effect_Aff.killFiber(Effect_Exception.error("cancel"))(fiber2))(function () {
        return pure1(Data_Unit.unit);
      });
    });
  });
});
exports.example = example;
},{"../Aff.Util/index.js":"../output/Aff.Util/index.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Data.Either/index.js":"../output/Data.Either/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Effect.Aff/index.js":"../output/Effect.Aff/index.js","../Effect.Console/index.js":"../output/Effect.Console/index.js","../Effect.Exception/index.js":"../output/Effect.Exception/index.js"}],"../output/Aff.RunAff/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.example = void 0;

var Aff_Util = _interopRequireWildcard(require("../Aff.Util/index.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Data_Either = _interopRequireWildcard(require("../Data.Either/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Effect_Aff = _interopRequireWildcard(require("../Effect.Aff/index.js"));

var Effect_Class = _interopRequireWildcard(require("../Effect.Class/index.js"));

var Effect_Class_Console = _interopRequireWildcard(require("../Effect.Class.Console/index.js"));

var Effect_Exception = _interopRequireWildcard(require("../Effect.Exception/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var log =
/* #__PURE__ */
Effect_Class_Console.log(Effect_Class.monadEffectEffect);
var discard =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit);
var discard1 =
/* #__PURE__ */
discard(Effect_Aff.bindAff);
var pure =
/* #__PURE__ */
Control_Applicative.pure(Effect_Aff.applicativeAff);

var example = function __do() {
  var suspendedFiber = Effect_Aff.runSuspendedAff(Data_Either.either(function ($8) {
    return log(Effect_Exception.message($8));
  })(log))(discard1(Aff_Util.affLog("runSuspendedAff"))(function () {
    return pure("runSuspendedAff result");
  }))();
  var fiber = Effect_Aff.runAff(Data_Either.either(function ($9) {
    return log(Effect_Exception.message($9));
  })(log))(discard1(Aff_Util.affLog("runAff"))(function () {
    return pure("runAff result");
  }))();
  Effect_Aff.runAff_(Data_Either.either(function ($10) {
    return log(Effect_Exception.message($10));
  })(log))(discard1(Aff_Util.affLog("runAff_"))(function () {
    return pure("runAff_ result");
  }))();
  Effect_Aff.launchAff_(Effect_Aff.joinFiber(fiber))();
  Effect_Aff.launchAff_(Effect_Aff.joinFiber(suspendedFiber))();
  return Data_Unit.unit;
};

exports.example = example;
},{"../Aff.Util/index.js":"../output/Aff.Util/index.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Data.Either/index.js":"../output/Data.Either/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Effect.Aff/index.js":"../output/Effect.Aff/index.js","../Effect.Class/index.js":"../output/Effect.Class/index.js","../Effect.Class.Console/index.js":"../output/Effect.Class.Console/index.js","../Effect.Exception/index.js":"../output/Effect.Exception/index.js"}],"../output/Aff.Supervise/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.example = void 0;

var Aff_Util = _interopRequireWildcard(require("../Aff.Util/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Effect_Aff = _interopRequireWildcard(require("../Effect.Aff/index.js"));

var Effect_Class = _interopRequireWildcard(require("../Effect.Class/index.js"));

var Effect_Exception = _interopRequireWildcard(require("../Effect.Exception/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var discard =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff);
var bind =
/* #__PURE__ */
Control_Bind.bind(Effect_Aff.bindAff);
var liftEffect =
/* #__PURE__ */
Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
var example =
/* #__PURE__ */
discard(
/* #__PURE__ */
Aff_Util.affLog("Parent process start."))(function () {
  return discard(Effect_Aff.supervise(bind(Effect_Aff.forkAff(discard(Effect_Aff.delay(3000.0))(function () {
    return liftEffect(Effect_Exception["throw"]("cancelled"));
  })))(function () {
    return bind(Effect_Aff.forkAff(discard(Effect_Aff.delay(10.0))(function () {
      return Aff_Util.affLog("child2");
    })))(function () {
      return discard(Effect_Aff.delay(100.0))(function () {
        return Aff_Util.affLog("done");
      });
    });
  })))(function () {
    return Aff_Util.affLog("Parent process end.");
  });
});
exports.example = example;
},{"../Aff.Util/index.js":"../output/Aff.Util/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Effect.Aff/index.js":"../output/Effect.Aff/index.js","../Effect.Class/index.js":"../output/Effect.Class/index.js","../Effect.Exception/index.js":"../output/Effect.Exception/index.js"}],"../output/Aff.Suspend/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.example = void 0;

var Aff_Util = _interopRequireWildcard(require("../Aff.Util/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Effect_Aff = _interopRequireWildcard(require("../Effect.Aff/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var bind =
/* #__PURE__ */
Control_Bind.bind(Effect_Aff.bindAff);
var discard =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff);
var example =
/* #__PURE__ */
bind(
/* #__PURE__ */
Effect_Aff.forkAff(
/* #__PURE__ */
Aff_Util.affLog("A")))(function (f1) {
  return bind(Effect_Aff.suspendAff(Aff_Util.affLog("B")))(function (f2) {
    return discard(Aff_Util.affLog("C"))(function () {
      return discard(Effect_Aff.delay(300.0))(function () {
        return discard(Aff_Util.affLog("D"))(function () {
          return discard(Effect_Aff.joinFiber(f1))(function () {
            return Effect_Aff.joinFiber(f2);
          });
        });
      });
    });
  });
});
exports.example = example;
},{"../Aff.Util/index.js":"../output/Aff.Util/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Effect.Aff/index.js":"../output/Effect.Aff/index.js"}],"../output/Aff.Main/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.main = exports.executeEff = exports.execute = void 0;

var Aff_Apathize = _interopRequireWildcard(require("../Aff.Apathize/index.js"));

var Aff_Attempt = _interopRequireWildcard(require("../Aff.Attempt/index.js"));

var Aff_Bracket = _interopRequireWildcard(require("../Aff.Bracket/index.js"));

var Aff_Delay = _interopRequireWildcard(require("../Aff.Delay/index.js"));

var Aff_Finally = _interopRequireWildcard(require("../Aff.Finally/index.js"));

var Aff_Fork = _interopRequireWildcard(require("../Aff.Fork/index.js"));

var Aff_Invincible = _interopRequireWildcard(require("../Aff.Invincible/index.js"));

var Aff_Kill = _interopRequireWildcard(require("../Aff.Kill/index.js"));

var Aff_LaunchAff = _interopRequireWildcard(require("../Aff.LaunchAff/index.js"));

var Aff_MakeAff = _interopRequireWildcard(require("../Aff.MakeAff/index.js"));

var Aff_RunAff = _interopRequireWildcard(require("../Aff.RunAff/index.js"));

var Aff_Supervise = _interopRequireWildcard(require("../Aff.Supervise/index.js"));

var Aff_Suspend = _interopRequireWildcard(require("../Aff.Suspend/index.js"));

var Aff_Util = _interopRequireWildcard(require("../Aff.Util/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Effect_Aff = _interopRequireWildcard(require("../Effect.Aff/index.js"));

var Effect_Console = _interopRequireWildcard(require("../Effect.Console/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var discard =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit);
var discard2 =
/* #__PURE__ */
discard(Effect_Aff.bindAff);

var executeEff = function executeEff(a) {
  return function (title) {
    return function __do() {
      Effect_Console.log("\x0a[" + (title + " example start]"))();
      a();
      return Effect_Console.log("[" + (title + " example end]"))();
    };
  };
};

exports.executeEff = executeEff;

var execute = function execute(a) {
  return function (title) {
    return discard2(Aff_Util.affLog("\x0a[" + (title + " example start]")))(function () {
      return discard2(a)(function () {
        return Aff_Util.affLog("[" + (title + " example end]"));
      });
    });
  };
};

exports.execute = execute;

var main = function __do() {
  executeEff(Aff_LaunchAff.example)("launchAff")();
  executeEff(Aff_RunAff.example)("runAff")();
  return Effect_Aff.launchAff_(discard2(execute(Aff_Delay.example)("delay"))(function () {
    return discard2(execute(Aff_Fork.example)("forkAff"))(function () {
      return discard2(execute(Aff_Suspend.example)("suspend"))(function () {
        return discard2(execute(Aff_Bracket.example)("bracket"))(function () {
          return discard2(execute(Aff_Supervise.example)("supervise"))(function () {
            return discard2(execute(Aff_Kill.example)("killFiber"))(function () {
              return discard2(execute(Aff_Attempt.example)("attempt"))(function () {
                return discard2(execute(Aff_Apathize.example)("apathize"))(function () {
                  return discard2(execute(Aff_Finally.example)("finally"))(function () {
                    return discard2(execute(Aff_MakeAff.example)("makeAff"))(function () {
                      return execute(Aff_Invincible.example)("invincible");
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  }))();
};

exports.main = main;
},{"../Aff.Apathize/index.js":"../output/Aff.Apathize/index.js","../Aff.Attempt/index.js":"../output/Aff.Attempt/index.js","../Aff.Bracket/index.js":"../output/Aff.Bracket/index.js","../Aff.Delay/index.js":"../output/Aff.Delay/index.js","../Aff.Finally/index.js":"../output/Aff.Finally/index.js","../Aff.Fork/index.js":"../output/Aff.Fork/index.js","../Aff.Invincible/index.js":"../output/Aff.Invincible/index.js","../Aff.Kill/index.js":"../output/Aff.Kill/index.js","../Aff.LaunchAff/index.js":"../output/Aff.LaunchAff/index.js","../Aff.MakeAff/index.js":"../output/Aff.MakeAff/index.js","../Aff.RunAff/index.js":"../output/Aff.RunAff/index.js","../Aff.Supervise/index.js":"../output/Aff.Supervise/index.js","../Aff.Suspend/index.js":"../output/Aff.Suspend/index.js","../Aff.Util/index.js":"../output/Aff.Util/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Effect.Aff/index.js":"../output/Effect.Aff/index.js","../Effect.Console/index.js":"../output/Effect.Console/index.js"}],"../output/Pattern.FourLayer.APIWithInfrastructure/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.runApp = exports.monadEffect = exports.monadAsk = exports.monadAppM = exports.logToScreenAppM = exports.getUserNameAppM = exports.functorTestM = exports.bindAppM = exports.applyAppM = exports.applicativeAppM = exports.AppM = void 0;

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Monad_Reader_Class = _interopRequireWildcard(require("../Control.Monad.Reader.Class/index.js"));

var Control_Monad_Reader_Trans = _interopRequireWildcard(require("../Control.Monad.Reader.Trans/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

var Effect = _interopRequireWildcard(require("../Effect/index.js"));

var Effect_Class = _interopRequireWildcard(require("../Effect.Class/index.js"));

var Effect_Console = _interopRequireWildcard(require("../Effect.Console/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var pure =
/* #__PURE__ */
Control_Applicative.pure(Effect.applicativeEffect);
var show =
/* #__PURE__ */
Data_Show.show(Data_Show.showInt);

var AppM = function AppM(x) {
  return x;
};

exports.AppM = AppM;
var applicativeAppM =
/* #__PURE__ */
Control_Monad_Reader_Trans.applicativeReaderT(Effect.applicativeEffect);
exports.applicativeAppM = applicativeAppM;

var runApp = function runApp(v) {
  return function (env) {
    return Control_Monad_Reader_Trans.runReaderT(v)(env);
  };
};

exports.runApp = runApp;
var monadEffect =
/* #__PURE__ */
Control_Monad_Reader_Trans.monadEffectReader(Effect_Class.monadEffectEffect);
exports.monadEffect = monadEffect;
var liftEffect =
/* #__PURE__ */
Effect_Class.liftEffect(monadEffect);
var monadAsk =
/* #__PURE__ */
Control_Monad_Reader_Trans.monadAskReaderT(Effect.monadEffect);
exports.monadAsk = monadAsk;
var monadAppM =
/* #__PURE__ */
Control_Monad_Reader_Trans.monadReaderT(Effect.monadEffect);
exports.monadAppM = monadAppM;
var logToScreenAppM = {
  log: function log($20) {
    return liftEffect(Effect_Console.log($20));
  },
  Monad0: function Monad0() {
    return monadAppM;
  }
};
exports.logToScreenAppM = logToScreenAppM;
var functorTestM =
/* #__PURE__ */
Control_Monad_Reader_Trans.functorReaderT(Effect.functorEffect);
exports.functorTestM = functorTestM;
var bindAppM =
/* #__PURE__ */
Control_Monad_Reader_Trans.bindReaderT(Effect.bindEffect);
exports.bindAppM = bindAppM;
var getUserNameAppM = {
  getUserName:
  /* #__PURE__ */
  Control_Bind.bind(bindAppM)(
  /* #__PURE__ */
  Control_Monad_Reader_Class.ask(monadAsk))(function (env) {
    return liftEffect(pure("some name " + show(env.someValue)));
  }),
  Monad0: function Monad0() {
    return monadAppM;
  }
};
exports.getUserNameAppM = getUserNameAppM;
var applyAppM =
/* #__PURE__ */
Control_Monad_Reader_Trans.applyReaderT(Effect.applyEffect);
exports.applyAppM = applyAppM;
},{"../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Monad.Reader.Class/index.js":"../output/Control.Monad.Reader.Class/index.js","../Control.Monad.Reader.Trans/index.js":"../output/Control.Monad.Reader.Trans/index.js","../Data.Show/index.js":"../output/Data.Show/index.js","../Effect/index.js":"../output/Effect/index.js","../Effect.Class/index.js":"../output/Effect.Class/index.js","../Effect.Console/index.js":"../output/Effect.Console/index.js"}],"../output/Pattern.FourLayer.Core/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.getName = exports.Name = void 0;

// Generated by purs version 0.15.10
var Name = function Name(x) {
  return x;
};

exports.Name = Name;

var getName = function getName(v) {
  return v;
};

exports.getName = getName;
},{}],"../output/Pattern.FourLayer.Domain/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.program = exports.log = exports.getUserName = void 0;

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Pattern_FourLayer_Core = _interopRequireWildcard(require("../Pattern.FourLayer.Core/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var discard =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit);

var log = function log(dict) {
  return dict.log;
};

exports.log = log;

var getUserName = function getUserName(dict) {
  return dict.getUserName;
};

exports.getUserName = getUserName;

var program = function program(dictLogToScreen) {
  var log1 = log(dictLogToScreen);
  return function (dictGetUserName) {
    var Bind1 = dictGetUserName.Monad0().Bind1();
    var bind = Control_Bind.bind(Bind1);
    var getUserName1 = getUserName(dictGetUserName);
    return discard(Bind1)(log1("What is your name?"))(function () {
      return bind(getUserName1)(function (name) {
        return log1("You name is " + Pattern_FourLayer_Core.getName(name));
      });
    });
  };
};

exports.program = program;
},{"../Control.Bind/index.js":"../output/Control.Bind/index.js","../Pattern.FourLayer.Core/index.js":"../output/Pattern.FourLayer.Core/index.js"}],"../output/Pattern.FourLayer.Main/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.main = void 0;

var Pattern_FourLayer_APIWithInfrastructure = _interopRequireWildcard(require("../Pattern.FourLayer.APIWithInfrastructure/index.js"));

var Pattern_FourLayer_Domain = _interopRequireWildcard(require("../Pattern.FourLayer.Domain/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var main =
/* #__PURE__ */
function () {
  var globalEnvironmentInfo = {
    someValue: 100
  };
  return Pattern_FourLayer_APIWithInfrastructure.runApp(Pattern_FourLayer_Domain.program(Pattern_FourLayer_APIWithInfrastructure.logToScreenAppM)(Pattern_FourLayer_APIWithInfrastructure.getUserNameAppM))(globalEnvironmentInfo);
}();

exports.main = main;
},{"../Pattern.FourLayer.APIWithInfrastructure/index.js":"../output/Pattern.FourLayer.APIWithInfrastructure/index.js","../Pattern.FourLayer.Domain/index.js":"../output/Pattern.FourLayer.Domain/index.js"}],"../output/Control.Monad.State.Trans/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.functorStateT = exports.execStateT = exports.evalStateT = exports.bindStateT = exports.applyStateT = exports.applicativeStateT = exports.alternativeStateT = exports.altStateT = exports.StateT = void 0;
Object.defineProperty(exports, "get", {
  enumerable: true,
  get: function () {
    return Control_Monad_State_Class.get;
  }
});
Object.defineProperty(exports, "gets", {
  enumerable: true,
  get: function () {
    return Control_Monad_State_Class.gets;
  }
});
exports.lazyStateT = void 0;
Object.defineProperty(exports, "lift", {
  enumerable: true,
  get: function () {
    return Control_Monad_Trans_Class.lift;
  }
});
exports.mapStateT = void 0;
Object.defineProperty(exports, "modify", {
  enumerable: true,
  get: function () {
    return Control_Monad_State_Class.modify;
  }
});
Object.defineProperty(exports, "modify_", {
  enumerable: true,
  get: function () {
    return Control_Monad_State_Class.modify_;
  }
});
exports.plusStateT = exports.newtypeStateT = exports.monoidStateT = exports.monadWriterStateT = exports.monadTransStateT = exports.monadThrowStateT = exports.monadTellStateT = exports.monadStateT = exports.monadStateStateT = exports.monadRecStateT = exports.monadReaderStateT = exports.monadPlusStateT = exports.monadErrorStateT = exports.monadEffectState = exports.monadContStateT = exports.monadAskStateT = void 0;
Object.defineProperty(exports, "put", {
  enumerable: true,
  get: function () {
    return Control_Monad_State_Class.put;
  }
});
exports.semigroupStateT = exports.runStateT = void 0;
Object.defineProperty(exports, "state", {
  enumerable: true,
  get: function () {
    return Control_Monad_State_Class.state;
  }
});
exports.withStateT = void 0;

var Control_Alt = _interopRequireWildcard(require("../Control.Alt/index.js"));

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Apply = _interopRequireWildcard(require("../Control.Apply/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Monad = _interopRequireWildcard(require("../Control.Monad/index.js"));

var Control_Monad_Cont_Class = _interopRequireWildcard(require("../Control.Monad.Cont.Class/index.js"));

var Control_Monad_Error_Class = _interopRequireWildcard(require("../Control.Monad.Error.Class/index.js"));

var Control_Monad_Reader_Class = _interopRequireWildcard(require("../Control.Monad.Reader.Class/index.js"));

var Control_Monad_Rec_Class = _interopRequireWildcard(require("../Control.Monad.Rec.Class/index.js"));

var Control_Monad_State_Class = _interopRequireWildcard(require("../Control.Monad.State.Class/index.js"));

var Control_Monad_Trans_Class = _interopRequireWildcard(require("../Control.Monad.Trans.Class/index.js"));

var Control_Monad_Writer_Class = _interopRequireWildcard(require("../Control.Monad.Writer.Class/index.js"));

var Control_Plus = _interopRequireWildcard(require("../Control.Plus/index.js"));

var Data_Functor = _interopRequireWildcard(require("../Data.Functor/index.js"));

var Data_Monoid = _interopRequireWildcard(require("../Data.Monoid/index.js"));

var Data_Semigroup = _interopRequireWildcard(require("../Data.Semigroup/index.js"));

var Data_Tuple = _interopRequireWildcard(require("../Data.Tuple/index.js"));

var Data_Unit = _interopRequireWildcard(require("../Data.Unit/index.js"));

var Effect_Class = _interopRequireWildcard(require("../Effect.Class/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var StateT = function StateT(x) {
  return x;
};

exports.StateT = StateT;

var withStateT = function withStateT(f) {
  return function (v) {
    return function ($193) {
      return v(f($193));
    };
  };
};

exports.withStateT = withStateT;

var runStateT = function runStateT(v) {
  return v;
};

exports.runStateT = runStateT;
var newtypeStateT = {
  Coercible0: function Coercible0() {
    return undefined;
  }
};
exports.newtypeStateT = newtypeStateT;
var monadTransStateT = {
  lift: function lift(dictMonad) {
    var bind = Control_Bind.bind(dictMonad.Bind1());
    var pure = Control_Applicative.pure(dictMonad.Applicative0());
    return function (m) {
      return function (s) {
        return bind(m)(function (x) {
          return pure(new Data_Tuple.Tuple(x, s));
        });
      };
    };
  }
};
exports.monadTransStateT = monadTransStateT;
var lift =
/* #__PURE__ */
Control_Monad_Trans_Class.lift(monadTransStateT);

var mapStateT = function mapStateT(f) {
  return function (v) {
    return function ($194) {
      return f(v($194));
    };
  };
};

exports.mapStateT = mapStateT;
var lazyStateT = {
  defer: function defer(f) {
    return function (s) {
      var v = f(Data_Unit.unit);
      return v(s);
    };
  }
};
exports.lazyStateT = lazyStateT;

var functorStateT = function functorStateT(dictFunctor) {
  var _map = Data_Functor.map(dictFunctor);

  return {
    map: function map(f) {
      return function (v) {
        return function (s) {
          return _map(function (v1) {
            return new Data_Tuple.Tuple(f(v1.value0), v1.value1);
          })(v(s));
        };
      };
    }
  };
};

exports.functorStateT = functorStateT;

var execStateT = function execStateT(dictFunctor) {
  var map = Data_Functor.map(dictFunctor);
  return function (v) {
    return function (s) {
      return map(Data_Tuple.snd)(v(s));
    };
  };
};

exports.execStateT = execStateT;

var evalStateT = function evalStateT(dictFunctor) {
  var map = Data_Functor.map(dictFunctor);
  return function (v) {
    return function (s) {
      return map(Data_Tuple.fst)(v(s));
    };
  };
};

exports.evalStateT = evalStateT;

var monadStateT = function monadStateT(dictMonad) {
  return {
    Applicative0: function Applicative0() {
      return applicativeStateT(dictMonad);
    },
    Bind1: function Bind1() {
      return bindStateT(dictMonad);
    }
  };
};

exports.monadStateT = monadStateT;

var bindStateT = function bindStateT(dictMonad) {
  var _bind = Control_Bind.bind(dictMonad.Bind1());

  return {
    bind: function bind(v) {
      return function (f) {
        return function (s) {
          return _bind(v(s))(function (v1) {
            var v3 = f(v1.value0);
            return v3(v1.value1);
          });
        };
      };
    },
    Apply0: function Apply0() {
      return applyStateT(dictMonad);
    }
  };
};

exports.bindStateT = bindStateT;

var applyStateT = function applyStateT(dictMonad) {
  var functorStateT1 = functorStateT(dictMonad.Bind1().Apply0().Functor0());
  return {
    apply: Control_Monad.ap(monadStateT(dictMonad)),
    Functor0: function Functor0() {
      return functorStateT1;
    }
  };
};

exports.applyStateT = applyStateT;

var applicativeStateT = function applicativeStateT(dictMonad) {
  var _pure = Control_Applicative.pure(dictMonad.Applicative0());

  return {
    pure: function pure(a) {
      return function (s) {
        return _pure(new Data_Tuple.Tuple(a, s));
      };
    },
    Apply0: function Apply0() {
      return applyStateT(dictMonad);
    }
  };
};

exports.applicativeStateT = applicativeStateT;

var semigroupStateT = function semigroupStateT(dictMonad) {
  var lift2 = Control_Apply.lift2(applyStateT(dictMonad));
  return function (dictSemigroup) {
    return {
      append: lift2(Data_Semigroup.append(dictSemigroup))
    };
  };
};

exports.semigroupStateT = semigroupStateT;

var monadAskStateT = function monadAskStateT(dictMonadAsk) {
  var Monad0 = dictMonadAsk.Monad0();
  var monadStateT1 = monadStateT(Monad0);
  return {
    ask: lift(Monad0)(Control_Monad_Reader_Class.ask(dictMonadAsk)),
    Monad0: function Monad0() {
      return monadStateT1;
    }
  };
};

exports.monadAskStateT = monadAskStateT;

var monadReaderStateT = function monadReaderStateT(dictMonadReader) {
  var monadAskStateT1 = monadAskStateT(dictMonadReader.MonadAsk0());
  return {
    local: function () {
      var $195 = Control_Monad_Reader_Class.local(dictMonadReader);
      return function ($196) {
        return mapStateT($195($196));
      };
    }(),
    MonadAsk0: function MonadAsk0() {
      return monadAskStateT1;
    }
  };
};

exports.monadReaderStateT = monadReaderStateT;

var monadContStateT = function monadContStateT(dictMonadCont) {
  var _callCC = Control_Monad_Cont_Class.callCC(dictMonadCont);

  var monadStateT1 = monadStateT(dictMonadCont.Monad0());
  return {
    callCC: function callCC(f) {
      return function (s) {
        return _callCC(function (c) {
          var v = f(function (a) {
            return function (s$prime) {
              return c(new Data_Tuple.Tuple(a, s$prime));
            };
          });
          return v(s);
        });
      };
    },
    Monad0: function Monad0() {
      return monadStateT1;
    }
  };
};

exports.monadContStateT = monadContStateT;

var monadEffectState = function monadEffectState(dictMonadEffect) {
  var Monad0 = dictMonadEffect.Monad0();
  var monadStateT1 = monadStateT(Monad0);
  return {
    liftEffect: function () {
      var $197 = lift(Monad0);
      var $198 = Effect_Class.liftEffect(dictMonadEffect);
      return function ($199) {
        return $197($198($199));
      };
    }(),
    Monad0: function Monad0() {
      return monadStateT1;
    }
  };
};

exports.monadEffectState = monadEffectState;

var monadRecStateT = function monadRecStateT(dictMonadRec) {
  var Monad0 = dictMonadRec.Monad0();
  var bind = Control_Bind.bind(Monad0.Bind1());
  var pure = Control_Applicative.pure(Monad0.Applicative0());

  var _tailRecM = Control_Monad_Rec_Class.tailRecM(dictMonadRec);

  var monadStateT1 = monadStateT(Monad0);
  return {
    tailRecM: function tailRecM(f) {
      return function (a) {
        var f$prime = function f$prime(v) {
          var v1 = f(v.value0);
          return bind(v1(v.value1))(function (v2) {
            return pure(function () {
              if (v2.value0 instanceof Control_Monad_Rec_Class.Loop) {
                return new Control_Monad_Rec_Class.Loop(new Data_Tuple.Tuple(v2.value0.value0, v2.value1));
              }

              ;

              if (v2.value0 instanceof Control_Monad_Rec_Class.Done) {
                return new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(v2.value0.value0, v2.value1));
              }

              ;
              throw new Error("Failed pattern match at Control.Monad.State.Trans (line 87, column 16 - line 89, column 40): " + [v2.value0.constructor.name]);
            }());
          });
        };

        return function (s) {
          return _tailRecM(f$prime)(new Data_Tuple.Tuple(a, s));
        };
      };
    },
    Monad0: function Monad0() {
      return monadStateT1;
    }
  };
};

exports.monadRecStateT = monadRecStateT;

var monadStateStateT = function monadStateStateT(dictMonad) {
  var pure = Control_Applicative.pure(dictMonad.Applicative0());
  var monadStateT1 = monadStateT(dictMonad);
  return {
    state: function state(f) {
      return function ($200) {
        return pure(f($200));
      };
    },
    Monad0: function Monad0() {
      return monadStateT1;
    }
  };
};

exports.monadStateStateT = monadStateStateT;

var monadTellStateT = function monadTellStateT(dictMonadTell) {
  var Monad1 = dictMonadTell.Monad1();

  var _Semigroup = dictMonadTell.Semigroup0();

  var monadStateT1 = monadStateT(Monad1);
  return {
    tell: function () {
      var $201 = lift(Monad1);
      var $202 = Control_Monad_Writer_Class.tell(dictMonadTell);
      return function ($203) {
        return $201($202($203));
      };
    }(),
    Semigroup0: function Semigroup0() {
      return _Semigroup;
    },
    Monad1: function Monad1() {
      return monadStateT1;
    }
  };
};

exports.monadTellStateT = monadTellStateT;

var monadWriterStateT = function monadWriterStateT(dictMonadWriter) {
  var MonadTell1 = dictMonadWriter.MonadTell1();
  var Monad1 = MonadTell1.Monad1();
  var bind = Control_Bind.bind(Monad1.Bind1());

  var _listen = Control_Monad_Writer_Class.listen(dictMonadWriter);

  var pure = Control_Applicative.pure(Monad1.Applicative0());

  var _pass = Control_Monad_Writer_Class.pass(dictMonadWriter);

  var _Monoid = dictMonadWriter.Monoid0();

  var monadTellStateT1 = monadTellStateT(MonadTell1);
  return {
    listen: function listen(m) {
      return function (s) {
        return bind(_listen(m(s)))(function (v) {
          return pure(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value0, v.value1), v.value0.value1));
        });
      };
    },
    pass: function pass(m) {
      return function (s) {
        return _pass(bind(m(s))(function (v) {
          return pure(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value0, v.value1), v.value0.value1));
        }));
      };
    },
    Monoid0: function Monoid0() {
      return _Monoid;
    },
    MonadTell1: function MonadTell1() {
      return monadTellStateT1;
    }
  };
};

exports.monadWriterStateT = monadWriterStateT;

var monadThrowStateT = function monadThrowStateT(dictMonadThrow) {
  var Monad0 = dictMonadThrow.Monad0();
  var lift1 = lift(Monad0);

  var _throwError = Control_Monad_Error_Class.throwError(dictMonadThrow);

  var monadStateT1 = monadStateT(Monad0);
  return {
    throwError: function throwError(e) {
      return lift1(_throwError(e));
    },
    Monad0: function Monad0() {
      return monadStateT1;
    }
  };
};

exports.monadThrowStateT = monadThrowStateT;

var monadErrorStateT = function monadErrorStateT(dictMonadError) {
  var _catchError = Control_Monad_Error_Class.catchError(dictMonadError);

  var monadThrowStateT1 = monadThrowStateT(dictMonadError.MonadThrow0());
  return {
    catchError: function catchError(v) {
      return function (h) {
        return function (s) {
          return _catchError(v(s))(function (e) {
            var v1 = h(e);
            return v1(s);
          });
        };
      };
    },
    MonadThrow0: function MonadThrow0() {
      return monadThrowStateT1;
    }
  };
};

exports.monadErrorStateT = monadErrorStateT;

var monoidStateT = function monoidStateT(dictMonad) {
  var pure = Control_Applicative.pure(applicativeStateT(dictMonad));
  var semigroupStateT1 = semigroupStateT(dictMonad);
  return function (dictMonoid) {
    var semigroupStateT2 = semigroupStateT1(dictMonoid.Semigroup0());
    return {
      mempty: pure(Data_Monoid.mempty(dictMonoid)),
      Semigroup0: function Semigroup0() {
        return semigroupStateT2;
      }
    };
  };
};

exports.monoidStateT = monoidStateT;

var altStateT = function altStateT(dictMonad) {
  return function (dictAlt) {
    var _alt = Control_Alt.alt(dictAlt);

    var functorStateT1 = functorStateT(dictAlt.Functor0());
    return {
      alt: function alt(v) {
        return function (v1) {
          return function (s) {
            return _alt(v(s))(v1(s));
          };
        };
      },
      Functor0: function Functor0() {
        return functorStateT1;
      }
    };
  };
};

exports.altStateT = altStateT;

var plusStateT = function plusStateT(dictMonad) {
  var altStateT1 = altStateT(dictMonad);
  return function (dictPlus) {
    var _empty = Control_Plus.empty(dictPlus);

    var altStateT2 = altStateT1(dictPlus.Alt0());
    return {
      empty: function empty(v) {
        return _empty;
      },
      Alt0: function Alt0() {
        return altStateT2;
      }
    };
  };
};

exports.plusStateT = plusStateT;

var alternativeStateT = function alternativeStateT(dictMonad) {
  var applicativeStateT1 = applicativeStateT(dictMonad);
  var plusStateT1 = plusStateT(dictMonad);
  return function (dictAlternative) {
    var plusStateT2 = plusStateT1(dictAlternative.Plus1());
    return {
      Applicative0: function Applicative0() {
        return applicativeStateT1;
      },
      Plus1: function Plus1() {
        return plusStateT2;
      }
    };
  };
};

exports.alternativeStateT = alternativeStateT;

var monadPlusStateT = function monadPlusStateT(dictMonadPlus) {
  var Monad0 = dictMonadPlus.Monad0();
  var monadStateT1 = monadStateT(Monad0);
  var alternativeStateT1 = alternativeStateT(Monad0)(dictMonadPlus.Alternative1());
  return {
    Monad0: function Monad0() {
      return monadStateT1;
    },
    Alternative1: function Alternative1() {
      return alternativeStateT1;
    }
  };
};

exports.monadPlusStateT = monadPlusStateT;
},{"../Control.Alt/index.js":"../output/Control.Alt/index.js","../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Apply/index.js":"../output/Control.Apply/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Monad/index.js":"../output/Control.Monad/index.js","../Control.Monad.Cont.Class/index.js":"../output/Control.Monad.Cont.Class/index.js","../Control.Monad.Error.Class/index.js":"../output/Control.Monad.Error.Class/index.js","../Control.Monad.Reader.Class/index.js":"../output/Control.Monad.Reader.Class/index.js","../Control.Monad.Rec.Class/index.js":"../output/Control.Monad.Rec.Class/index.js","../Control.Monad.State.Class/index.js":"../output/Control.Monad.State.Class/index.js","../Control.Monad.Trans.Class/index.js":"../output/Control.Monad.Trans.Class/index.js","../Control.Monad.Writer.Class/index.js":"../output/Control.Monad.Writer.Class/index.js","../Control.Plus/index.js":"../output/Control.Plus/index.js","../Data.Functor/index.js":"../output/Data.Functor/index.js","../Data.Monoid/index.js":"../output/Data.Monoid/index.js","../Data.Semigroup/index.js":"../output/Data.Semigroup/index.js","../Data.Tuple/index.js":"../output/Data.Tuple/index.js","../Data.Unit/index.js":"../output/Data.Unit/index.js","../Effect.Class/index.js":"../output/Effect.Class/index.js"}],"../output/Pattern.ReaderT.ReaderT/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.monadBalanceStateTInt = exports.monadBalanceReaderT = exports.modifyBalance = exports.modify = exports.main = exports.logSomething = exports.hasLogStringFunctionEffec = exports.hasLogEnv = exports.hasBalanceRefInt = exports.hasBalanceEnv = exports.getLog = exports.getBalance = exports.Env = void 0;

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Category = _interopRequireWildcard(require("../Control.Category/index.js"));

var Control_Monad_Reader_Class = _interopRequireWildcard(require("../Control.Monad.Reader.Class/index.js"));

var Control_Monad_Reader_Trans = _interopRequireWildcard(require("../Control.Monad.Reader.Trans/index.js"));

var Control_Monad_State_Class = _interopRequireWildcard(require("../Control.Monad.State.Class/index.js"));

var Control_Monad_State_Trans = _interopRequireWildcard(require("../Control.Monad.State.Trans/index.js"));

var Effect = _interopRequireWildcard(require("../Effect/index.js"));

var Effect_Class = _interopRequireWildcard(require("../Effect.Class/index.js"));

var Effect_Console = _interopRequireWildcard(require("../Effect.Console/index.js"));

var Effect_Ref = _interopRequireWildcard(require("../Effect.Ref/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var identity =
/* #__PURE__ */
Control_Category.identity(Control_Category.categoryFn);

var Env =
/* #__PURE__ */
function () {
  function Env(value0) {
    this.value0 = value0;
  }

  ;

  Env.create = function (value0) {
    return new Env(value0);
  };

  return Env;
}();

exports.Env = Env;

var monadBalanceStateTInt = function monadBalanceStateTInt(dictMonad) {
  var monadStateT = Control_Monad_State_Trans.monadStateT(dictMonad);
  return {
    modifyBalance: Control_Monad_State_Class.modify_(Control_Monad_State_Trans.monadStateStateT(dictMonad)),
    Monad0: function Monad0() {
      return monadStateT;
    }
  };
};

exports.monadBalanceStateTInt = monadBalanceStateTInt;
var hasLogStringFunctionEffec = {
  getLog: identity
};
exports.hasLogStringFunctionEffec = hasLogStringFunctionEffec;
var hasLogEnv = {
  getLog: function getLog(v) {
    return v.value0.envLog;
  }
};
exports.hasLogEnv = hasLogEnv;
var hasBalanceRefInt = {
  getBalance: identity
};
exports.hasBalanceRefInt = hasBalanceRefInt;
var hasBalanceEnv = {
  getBalance: function getBalance(v) {
    return v.value0.envBalance;
  }
};
exports.hasBalanceEnv = hasBalanceEnv;

var modifyBalance = function modifyBalance(dict) {
  return dict.modifyBalance;
};

exports.modifyBalance = modifyBalance;

var modify = function modify(dictMonadBalance) {
  var modifyBalance1 = modifyBalance(dictMonadBalance);
  return function (f) {
    return modifyBalance1(f);
  };
};

exports.modify = modify;

var getLog = function getLog(dict) {
  return dict.getLog;
};

exports.getLog = getLog;

var logSomething = function logSomething(dictHasLog) {
  var getLog1 = getLog(dictHasLog);
  return function (dictMonadReader) {
    var ask = Control_Monad_Reader_Class.ask(dictMonadReader.MonadAsk0());
    return function (dictMonadEffect) {
      var bind = Control_Bind.bind(dictMonadEffect.Monad0().Bind1());
      var liftEffect = Effect_Class.liftEffect(dictMonadEffect);
      return function (msg) {
        return bind(ask)(function (env) {
          return liftEffect(getLog1(env)(msg));
        });
      };
    };
  };
};

exports.logSomething = logSomething;
var main =
/* #__PURE__ */
Control_Monad_Reader_Trans.runReaderT(
/* #__PURE__ */
logSomething(hasLogStringFunctionEffec)(
/* #__PURE__ */
Control_Monad_Reader_Trans.monadReaderReaderT(Effect.monadEffect))(
/* #__PURE__ */
Control_Monad_Reader_Trans.monadEffectReader(Effect_Class.monadEffectEffect))("message"))(Effect_Console.log);
exports.main = main;

var getBalance = function getBalance(dict) {
  return dict.getBalance;
};

exports.getBalance = getBalance;

var monadBalanceReaderT = function monadBalanceReaderT(dictHasBalance) {
  var getBalance1 = getBalance(dictHasBalance);
  return function (dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    var bind = Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Monad0.Bind1()));
    var ask = Control_Monad_Reader_Class.ask(Control_Monad_Reader_Trans.monadAskReaderT(Monad0));
    var liftEffect = Effect_Class.liftEffect(Control_Monad_Reader_Trans.monadEffectReader(dictMonadEffect));
    var monadReaderT = Control_Monad_Reader_Trans.monadReaderT(Monad0);
    return {
      modifyBalance: function modifyBalance(f) {
        return bind(ask)(function (env) {
          return liftEffect(Effect_Ref.modify_(f)(getBalance1(env)));
        });
      },
      Monad0: function Monad0() {
        return monadReaderT;
      }
    };
  };
};

exports.monadBalanceReaderT = monadBalanceReaderT;
},{"../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Category/index.js":"../output/Control.Category/index.js","../Control.Monad.Reader.Class/index.js":"../output/Control.Monad.Reader.Class/index.js","../Control.Monad.Reader.Trans/index.js":"../output/Control.Monad.Reader.Trans/index.js","../Control.Monad.State.Class/index.js":"../output/Control.Monad.State.Class/index.js","../Control.Monad.State.Trans/index.js":"../output/Control.Monad.State.Trans/index.js","../Effect/index.js":"../output/Effect/index.js","../Effect.Class/index.js":"../output/Effect.Class/index.js","../Effect.Console/index.js":"../output/Effect.Console/index.js","../Effect.Ref/index.js":"../output/Effect.Ref/index.js"}],"../output/Pattern.ThreeLayer.Layer1/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.runApp = exports.monadEffect = exports.monadAsk = exports.monadAppM = exports.logToScreenAppM = exports.getUserNameAppM = exports.functorTestM = exports.bindAppM = exports.applyAppM = exports.applicativeAppM = exports.AppM = void 0;

var Control_Applicative = _interopRequireWildcard(require("../Control.Applicative/index.js"));

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Control_Monad_Reader_Class = _interopRequireWildcard(require("../Control.Monad.Reader.Class/index.js"));

var Control_Monad_Reader_Trans = _interopRequireWildcard(require("../Control.Monad.Reader.Trans/index.js"));

var Data_Show = _interopRequireWildcard(require("../Data.Show/index.js"));

var Effect = _interopRequireWildcard(require("../Effect/index.js"));

var Effect_Class = _interopRequireWildcard(require("../Effect.Class/index.js"));

var Effect_Console = _interopRequireWildcard(require("../Effect.Console/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var pure =
/* #__PURE__ */
Control_Applicative.pure(Effect.applicativeEffect);
var show =
/* #__PURE__ */
Data_Show.show(Data_Show.showInt);

var AppM = function AppM(x) {
  return x;
};

exports.AppM = AppM;
var applicativeAppM =
/* #__PURE__ */
Control_Monad_Reader_Trans.applicativeReaderT(Effect.applicativeEffect);
exports.applicativeAppM = applicativeAppM;

var runApp = function runApp(v) {
  return function (env) {
    return Control_Monad_Reader_Trans.runReaderT(v)(env);
  };
};

exports.runApp = runApp;
var monadEffect =
/* #__PURE__ */
Control_Monad_Reader_Trans.monadEffectReader(Effect_Class.monadEffectEffect);
exports.monadEffect = monadEffect;
var liftEffect =
/* #__PURE__ */
Effect_Class.liftEffect(monadEffect);
var monadAsk =
/* #__PURE__ */
Control_Monad_Reader_Trans.monadAskReaderT(Effect.monadEffect);
exports.monadAsk = monadAsk;
var monadAppM =
/* #__PURE__ */
Control_Monad_Reader_Trans.monadReaderT(Effect.monadEffect);
exports.monadAppM = monadAppM;
var logToScreenAppM = {
  log: function log($20) {
    return liftEffect(Effect_Console.log($20));
  },
  Monad0: function Monad0() {
    return monadAppM;
  }
};
exports.logToScreenAppM = logToScreenAppM;
var functorTestM =
/* #__PURE__ */
Control_Monad_Reader_Trans.functorReaderT(Effect.functorEffect);
exports.functorTestM = functorTestM;
var bindAppM =
/* #__PURE__ */
Control_Monad_Reader_Trans.bindReaderT(Effect.bindEffect);
exports.bindAppM = bindAppM;
var getUserNameAppM = {
  getUserName:
  /* #__PURE__ */
  Control_Bind.bind(bindAppM)(
  /* #__PURE__ */
  Control_Monad_Reader_Class.ask(monadAsk))(function (env) {
    return liftEffect(pure("some name " + show(env.someValue)));
  }),
  Monad0: function Monad0() {
    return monadAppM;
  }
};
exports.getUserNameAppM = getUserNameAppM;
var applyAppM =
/* #__PURE__ */
Control_Monad_Reader_Trans.applyReaderT(Effect.applyEffect);
exports.applyAppM = applyAppM;
},{"../Control.Applicative/index.js":"../output/Control.Applicative/index.js","../Control.Bind/index.js":"../output/Control.Bind/index.js","../Control.Monad.Reader.Class/index.js":"../output/Control.Monad.Reader.Class/index.js","../Control.Monad.Reader.Trans/index.js":"../output/Control.Monad.Reader.Trans/index.js","../Data.Show/index.js":"../output/Data.Show/index.js","../Effect/index.js":"../output/Effect/index.js","../Effect.Class/index.js":"../output/Effect.Class/index.js","../Effect.Console/index.js":"../output/Effect.Console/index.js"}],"../output/Pattern.ThreeLayer.Layer3/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.getName = exports.Name = void 0;

// Generated by purs version 0.15.10
var Name = function Name(x) {
  return x;
};

exports.Name = Name;

var getName = function getName(v) {
  return v;
};

exports.getName = getName;
},{}],"../output/Pattern.ThreeLayer.Layer2/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.program = exports.log = exports.getUserName = void 0;

var Control_Bind = _interopRequireWildcard(require("../Control.Bind/index.js"));

var Pattern_ThreeLayer_Layer3 = _interopRequireWildcard(require("../Pattern.ThreeLayer.Layer3/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var discard =
/* #__PURE__ */
Control_Bind.discard(Control_Bind.discardUnit);

var log = function log(dict) {
  return dict.log;
};

exports.log = log;

var getUserName = function getUserName(dict) {
  return dict.getUserName;
};

exports.getUserName = getUserName;

var program = function program(dictLogToScreen) {
  var log1 = log(dictLogToScreen);
  return function (dictGetUserName) {
    var Bind1 = dictGetUserName.Monad0().Bind1();
    var bind = Control_Bind.bind(Bind1);
    var getUserName1 = getUserName(dictGetUserName);
    return discard(Bind1)(log1("What is your name?"))(function () {
      return bind(getUserName1)(function (name) {
        return log1("You name is " + Pattern_ThreeLayer_Layer3.getName(name));
      });
    });
  };
};

exports.program = program;
},{"../Control.Bind/index.js":"../output/Control.Bind/index.js","../Pattern.ThreeLayer.Layer3/index.js":"../output/Pattern.ThreeLayer.Layer3/index.js"}],"../output/Pattern.ThreeLayer.Main/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.main = void 0;

var Pattern_ThreeLayer_Layer1 = _interopRequireWildcard(require("../Pattern.ThreeLayer.Layer1/index.js"));

var Pattern_ThreeLayer_Layer2 = _interopRequireWildcard(require("../Pattern.ThreeLayer.Layer2/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var main =
/* #__PURE__ */
function () {
  var globalEnvironmentInfo = {
    someValue: 1000
  };
  return Pattern_ThreeLayer_Layer1.runApp(Pattern_ThreeLayer_Layer2.program(Pattern_ThreeLayer_Layer1.logToScreenAppM)(Pattern_ThreeLayer_Layer1.getUserNameAppM))(globalEnvironmentInfo);
}();

exports.main = main;
},{"../Pattern.ThreeLayer.Layer1/index.js":"../output/Pattern.ThreeLayer.Layer1/index.js","../Pattern.ThreeLayer.Layer2/index.js":"../output/Pattern.ThreeLayer.Layer2/index.js"}],"../output/Main/index.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.main = void 0;

var Aff_Main = _interopRequireWildcard(require("../Aff.Main/index.js"));

var Pattern_FourLayer_Main = _interopRequireWildcard(require("../Pattern.FourLayer.Main/index.js"));

var Pattern_ReaderT_ReaderT = _interopRequireWildcard(require("../Pattern.ReaderT.ReaderT/index.js"));

var Pattern_ThreeLayer_Main = _interopRequireWildcard(require("../Pattern.ThreeLayer.Main/index.js"));

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function (nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || typeof obj !== "object" && typeof obj !== "function") { return { default: obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Generated by purs version 0.15.10
var main = function __do() {
  Pattern_ReaderT_ReaderT.main();
  Pattern_ThreeLayer_Main.main();
  Pattern_FourLayer_Main.main();
  return Aff_Main.main();
};

exports.main = main;
},{"../Aff.Main/index.js":"../output/Aff.Main/index.js","../Pattern.FourLayer.Main/index.js":"../output/Pattern.FourLayer.Main/index.js","../Pattern.ReaderT.ReaderT/index.js":"../output/Pattern.ReaderT.ReaderT/index.js","../Pattern.ThreeLayer.Main/index.js":"../output/Pattern.ThreeLayer.Main/index.js"}],"index.js":[function(require,module,exports) {
require("../output/Main/index.js").main();
},{"../output/Main/index.js":"../output/Main/index.js"}],"../node_modules/parcel/src/builtins/hmr-runtime.js":[function(require,module,exports) {
var global = arguments[3];
var OVERLAY_ID = '__parcel__error__overlay__';
var OldModule = module.bundle.Module;

function Module(moduleName) {
  OldModule.call(this, moduleName);
  this.hot = {
    data: module.bundle.hotData,
    _acceptCallbacks: [],
    _disposeCallbacks: [],
    accept: function (fn) {
      this._acceptCallbacks.push(fn || function () {});
    },
    dispose: function (fn) {
      this._disposeCallbacks.push(fn);
    }
  };
  module.bundle.hotData = null;
}

module.bundle.Module = Module;
var checkedAssets, assetsToAccept;
var parent = module.bundle.parent;

if ((!parent || !parent.isParcelRequire) && typeof WebSocket !== 'undefined') {
  var hostname = "" || location.hostname;
  var protocol = location.protocol === 'https:' ? 'wss' : 'ws';
  var ws = new WebSocket(protocol + '://' + hostname + ':' + "51021" + '/');

  ws.onmessage = function (event) {
    checkedAssets = {};
    assetsToAccept = [];
    var data = JSON.parse(event.data);

    if (data.type === 'update') {
      var handled = false;
      data.assets.forEach(function (asset) {
        if (!asset.isNew) {
          var didAccept = hmrAcceptCheck(global.parcelRequire, asset.id);

          if (didAccept) {
            handled = true;
          }
        }
      }); // Enable HMR for CSS by default.

      handled = handled || data.assets.every(function (asset) {
        return asset.type === 'css' && asset.generated.js;
      });

      if (handled) {
        console.clear();
        data.assets.forEach(function (asset) {
          hmrApply(global.parcelRequire, asset);
        });
        assetsToAccept.forEach(function (v) {
          hmrAcceptRun(v[0], v[1]);
        });
      } else {
        window.location.reload();
      }
    }

    if (data.type === 'reload') {
      ws.close();

      ws.onclose = function () {
        location.reload();
      };
    }

    if (data.type === 'error-resolved') {
      console.log('[parcel] ✨ Error resolved');
      removeErrorOverlay();
    }

    if (data.type === 'error') {
      console.error('[parcel] 🚨  ' + data.error.message + '\n' + data.error.stack);
      removeErrorOverlay();
      var overlay = createErrorOverlay(data);
      document.body.appendChild(overlay);
    }
  };
}

function removeErrorOverlay() {
  var overlay = document.getElementById(OVERLAY_ID);

  if (overlay) {
    overlay.remove();
  }
}

function createErrorOverlay(data) {
  var overlay = document.createElement('div');
  overlay.id = OVERLAY_ID; // html encode message and stack trace

  var message = document.createElement('div');
  var stackTrace = document.createElement('pre');
  message.innerText = data.error.message;
  stackTrace.innerText = data.error.stack;
  overlay.innerHTML = '<div style="background: black; font-size: 16px; color: white; position: fixed; height: 100%; width: 100%; top: 0px; left: 0px; padding: 30px; opacity: 0.85; font-family: Menlo, Consolas, monospace; z-index: 9999;">' + '<span style="background: red; padding: 2px 4px; border-radius: 2px;">ERROR</span>' + '<span style="top: 2px; margin-left: 5px; position: relative;">🚨</span>' + '<div style="font-size: 18px; font-weight: bold; margin-top: 20px;">' + message.innerHTML + '</div>' + '<pre>' + stackTrace.innerHTML + '</pre>' + '</div>';
  return overlay;
}

function getParents(bundle, id) {
  var modules = bundle.modules;

  if (!modules) {
    return [];
  }

  var parents = [];
  var k, d, dep;

  for (k in modules) {
    for (d in modules[k][1]) {
      dep = modules[k][1][d];

      if (dep === id || Array.isArray(dep) && dep[dep.length - 1] === id) {
        parents.push(k);
      }
    }
  }

  if (bundle.parent) {
    parents = parents.concat(getParents(bundle.parent, id));
  }

  return parents;
}

function hmrApply(bundle, asset) {
  var modules = bundle.modules;

  if (!modules) {
    return;
  }

  if (modules[asset.id] || !bundle.parent) {
    var fn = new Function('require', 'module', 'exports', asset.generated.js);
    asset.isNew = !modules[asset.id];
    modules[asset.id] = [fn, asset.deps];
  } else if (bundle.parent) {
    hmrApply(bundle.parent, asset);
  }
}

function hmrAcceptCheck(bundle, id) {
  var modules = bundle.modules;

  if (!modules) {
    return;
  }

  if (!modules[id] && bundle.parent) {
    return hmrAcceptCheck(bundle.parent, id);
  }

  if (checkedAssets[id]) {
    return;
  }

  checkedAssets[id] = true;
  var cached = bundle.cache[id];
  assetsToAccept.push([bundle, id]);

  if (cached && cached.hot && cached.hot._acceptCallbacks.length) {
    return true;
  }

  return getParents(global.parcelRequire, id).some(function (id) {
    return hmrAcceptCheck(global.parcelRequire, id);
  });
}

function hmrAcceptRun(bundle, id) {
  var cached = bundle.cache[id];
  bundle.hotData = {};

  if (cached) {
    cached.hot.data = bundle.hotData;
  }

  if (cached && cached.hot && cached.hot._disposeCallbacks.length) {
    cached.hot._disposeCallbacks.forEach(function (cb) {
      cb(bundle.hotData);
    });
  }

  delete bundle.cache[id];
  bundle(id);
  cached = bundle.cache[id];

  if (cached && cached.hot && cached.hot._acceptCallbacks.length) {
    cached.hot._acceptCallbacks.forEach(function (cb) {
      cb();
    });

    return true;
  }
}
},{}]},{},["../node_modules/parcel/src/builtins/hmr-runtime.js","index.js"], null)
//# sourceMappingURL=/dev.e31bb0bc.js.map