//@flow
/**@signature a -> a */
const id = x => x

/**@signature a -> a -> a -> ... */
const ignore = _ => ignore

/**@signature a -> b -> (a -> c) -> () */
const just = a => _ => cb => cb(a)

class Fifo {
  /**@typedef T */
  /**@type {Number} */
  start = 0
  /**@type {Number} */
  end = 0
  /**@type {(any | undefined)[]} */
  store = []

  constructor(len = 0) {
    this.store = new Array(len).fill(undefined)
  }

  pop() {
    /*--------------------------------:
            Handle empty store
    :--------------------------------*/
    if (this.start === this.end)
      return undefined

    const out = this.store[this.start]
    this.store[this.start++] = undefined

    return out
  }

  /*--------------------------------:
          Append a new element 
          to the end
  :--------------------------------*/
  append(v) {
    this.store[this.end] = v
    this.end = (this.end + 1) % this.store.length
    return this
  }

  is_empty() {
    return this.store[this.start] === undefined
  }
}

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
    return function (...args) {
      f.define(args.map((v, i) => i < f_args.length && f_args[i].assign(v)))
      return cb => {
        f.e(cb)
      }
    }
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

  keep(pred) {
    const F = this.unpack()
    return CPS.evolve(cb =>
      F((...args) => pred(...args)
        ? cb(...args)
        : {}
      ), this)
  }

  gen(f) {
    const F = this.unpack()

    return CPS.evolve(cb =>
      F((...args) => {
        let a = args
        while (true) {
          const [next, out, cnt] = f.bind(this)(...a)
          if (!cnt) break
          cb(out)
          a = Object.getOwnPropertySymbols(next).includes(Symbol.iterator) ? next : [next]
        }
      }), this)
  }

  gather(...fs) {
    const F = this.unpack()

    return CPS.evolve(cb => F((...args) => {
      let acc = new Array(fs.length).fill(0).map(() => new Fifo(5))
      const update = i => (...v) => {
        acc[i].append(v)
        if (acc.reduce((r, a) => r && !a.is_empty(), true)) {
          const out = acc.flatMap(a => a.pop())
          cb(...out)
        }
      }

      fs.reduce((args, f, i) => {
        const non_variadic_args = args.slice(0, f.length)
        f.bind(this)(...(non_variadic_args.length > 0 ? non_variadic_args : args))(update(i))
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
    {~inc: a(Int) = + a 1}
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
    .e(id)
}