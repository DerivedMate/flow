import { trampoline } from "./utils.mjs"
import {Fifo} from "./utils.mjs"
//@flow
/**@signature a -> a */
const id = x => x

/**@signature a -> a -> a -> ... */
const ignore = _ => ignore

/**@signature a -> b -> (a -> c) -> () */
const just = a => _ => cb => cb(a)

class Arg {
  name = ""
  serializer = id
  val = undefined

  constructor(name, serializer) {
    this.name = name
    this.serializer = serializer
  }

  assign(val) {
    this.val = val
    return this
  }

  get() {
    return this.val
  }
}

class CPS {
  vars = {}

  constructor(v) {
    this.v = v.bind(this)
  }

  static pure(v) {
    if (v instanceof CPS) {
      return v
    }
    return new CPS(v)
  }

  static empty() {
    return CPS.pure(cb => cb(void 0))
  }


  /**
   * @param {CPS} f 
   * @param {Arg[]} f_args
   */
  static func(f, f_args) {
    f.arg_length = f_args.length

    const F = function (...args) {
      f.define(args.map((v, i) => i < f_args.length && f_args[i].assign(v)))
      return cb => {
        f.e(cb)
      }
    }

    /*--------------------------------:
        Define function's length
        since variadic functions 
        have length 0.
        Required for `CPS.prototype.gather`
    :--------------------------------*/
    Object.defineProperty(F, "length", {
      value: f_args.length
    })

    return F
  }

  /**
   * @param {Function} f 
   * @param {CPS} prev 
   */
  static evolve(f, prev) {
    const c = CPS.pure(f)
    c.vars = prev.vars

    return c
  }

  /**
   * @param {Arg[]} defs 
   * @returns {CPS}
   */
  define(defs) {
    defs.forEach(d => this.vars[d.name] = d)
    return this
  }

  lookup(v_name) {
    return this.vars[v_name]?.val
  }

  unpack() {
    return this.v
  }

  chain(f) {
    const F = this.unpack()

    return CPS.evolve(cb =>
      F((...args) => f.bind(this)(...args)(cb)),
      this
    )
  }

  fmap(f) {
    const F = this.unpack()

    return CPS.evolve(cb =>
      F((...args) => cb(f.bind(this)(...args))),
      this
    )
  }

  /**
   * @param {(args: any[]) => (cb: Function) => any} f 
   */
  keep(f) {
    const F = this.unpack()
    return CPS.evolve(cb => F((...args) => {
      f(...args)(r => r ? cb(...args) : {})
    }), this)
  }

  /**
   * @param {(args: any[]) => (cb: Function) => any} f 
   */
  gen(f) {
    const F = this.unpack()

    return CPS.evolve(cb => F((...args) => {
      let decider_flag = true
      const decider = ([next, out, cnt]) => {
        decider_flag = decider_flag && cnt
        if (!decider_flag) return

        cb(...out)
        f(...(Object.getOwnPropertySymbols(next).includes(Symbol.iterator)
          ? next
          : [next]))(decider)
      }

      f(...args)(decider)
    }), this)
  }

  gather(...fs) {
    const F = this.unpack()

    return CPS.evolve(cb => F((...args) => {
      let acc = new Array(fs.length).fill(0).map(() => new Fifo(255))
      const update = i => (...v) => {
        acc[i].append(v)
        if (acc.reduce((r, a) => r && !a.is_empty(), true)) {
          const out = acc.flatMap(a => a.pop())
          cb(...out)
        }
      }

      fs.reduce((args, f, i) => {
        f.bind(this)(...args.slice(0, f.length))(update(i))
        return args.slice(f.length)
      }, args)
    }), this)
  }

  e(cb) {
    this.unpack()(cb)
    return CPS.empty()
  }
}


/*--------------------------------:
    { ( 5; 10 ) } => { a(Int), b(Int) = + a b } 
                  => { <~ Int }
:--------------------------------*/
/*
CPS.empty()
  .gather(just(5), just(10))
  .chain(CPS.func(
    CPS.empty()
      .fmap(function () {
        return this.lookup("a") + this.lookup("b")
      }),
    [new Arg("a", Number), new Arg("b", Number)]
  ))
  .fmap(console.log)
  .e(id)
  */


/*--------------------------------:
  { ( 12; 11 ) } => filter { a(Int), b(Int) = && > a 10 > b 10 }
                 => { a(Int), b(Int) = + a b }
                 => { <~ Int }
  { 5 } => { <~ Int }
:--------------------------------*/


/*--------------------------------:
    {~inc: a(Int) = + a 1}
    {~dec: a(Int) = - a 1}
    { (2; 2) } => { (~inc; ~dec) }
               => { a(Int), b(Int) = [ a, b ] }
               => { <~ List<Int> }
:--------------------------------*/

/**
 * @todo Rewrite filter & gen to Func
 * @todo Rewrite Func to a separate entity (if possible)
 * @todo Rewrite Func to morph
 */

/*--------------------------------:
    {~inc: a(Int) = + a 1}
    {~dec: a(Int) = - a 1}
    {( 2; 2 )} => {( ~inc; ~dec )}
               => { <~ Int }
:--------------------------------*/

; {
  const $func_inc =
    CPS.func(
      CPS.empty()
        .fmap(function () {
          return this.lookup("a") + 1
        }),
      [new Arg("a", Number)]
    )

  const $func_dec =
    CPS.func(
      CPS.empty()
        .fmap(function () {
          return this.lookup("a") - 1
        }),
      [new Arg("a", Number)]
    )

  CPS.empty()
    .gather(just(2), just(2))
    .gather($func_inc, $func_dec)
    .fmap(console.log)
  // .e(id)
}


; {
  const $func_is_even =
    CPS.func(
      CPS.empty()
        .fmap(function () {
          return this.lookup("a") % 2 === 0
        })
      , [new Arg("a", Number)]
    )

  CPS.empty()
    .chain((..._) => cb => {
      new Array(10).fill(0).map((_, i) => i).forEach(a => cb(a))
    })
    .keep($func_is_even)
    .fmap(console.log)
    // .e(id)
}

; {
  const $func_gen_nums =
    CPS.func(
      CPS.empty()
         .fmap(function () {
           if (this.lookup("a") > 0)
            return [ this.lookup("a") - 1, [ this.lookup("a") ], true ]
          return [ [ 0 ], [ 0 ], false ]
         })
      , [new Arg("a", Number)]
    )

  CPS.empty()
     .gather(just(10000))
     .gen($func_gen_nums)
     .fmap(console.log)
     .e(id)
}

const f = trampoline(function _f(n) {
  if (n <= 1) return 1
  return n * _f(n - 1)
})

// console.dir(f(100))