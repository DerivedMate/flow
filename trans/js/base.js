import { Fifo, ensureIter } from "./utils.js"

/**@signature a -> a */
const id = (x) => x

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
   * @param {Arg[]} fArgs
   */
  static func(f, fArgs) {
    f.arg_length = fArgs.length

    const F = function (...args) {
      f.define(
        args
          .filter((_, i) => i < fArgs.length)
          .map((v, i) => i < fArgs.length && fArgs[i].assign(v)))
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
      value: fArgs.length
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
    debugger
    defs.forEach(d => this.vars[d.name] = d)
    return this
  }

  lookup(vName) {
    return this.vars[vName]?.val
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
      F((...args) => cb(...ensureIter(f.bind(this)(...args)))),
      this
    )
  }

  condChain(c, f) {
    const F = this.unpack()

    return CPS.evolve(cb => {
      F((...args) => c.bind(this)(...args)(r => r ? f.bind(this)(...args)(cb) : cb(...args)))
    }, this)
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

    return CPS.evolve(cb => {
      function func(...args) {
        let deciderFlag = true
        let interval
        const stack = new Fifo(255).append(f(...args))

        const decider = ([next, out, cnt]) => {
          /*--------------------------------:
              Terminate generator
          :--------------------------------*/
          deciderFlag = deciderFlag && cnt
          if (!deciderFlag) return

          cb(...out)
          /*--------------------------------:
              Continue
          :--------------------------------*/
          const args = ensureIter(next)
          stack.append(f(...args))
        }

        const loop = () => {
          while (!stack.is_empty()) {
            const s = stack.pop()
            s && s(decider)
          }

          if (!deciderFlag) clearInterval(interval)
        }

        interval = setInterval(loop, 0)
      }

      Object.defineProperty(func, "length", {
        value: f.length
      })

      F(func)
    }, this)
  }

  gather(...fs) {
    const F = this.unpack()

    return CPS.evolve(cb => F((...args) => {
      const acc = new Array(fs.length).fill(0).map(() => new Fifo(255))
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

const readStdIo = serializer => () => cb => {
  const buf = new Uint8Array(1024)
  Deno.stdin.read(buf)
    .then(n => n || 0)
    .then(n =>
      new TextDecoder().decode(buf.subarray(0, n)).trim()
    )
    .then(serializer)
    .then(cb)
    .catch(console.error)
}

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
      new Array(100).fill(0).map((_, i) => i).forEach(a => cb(a))
    })
    .keep($func_is_even)
    .fmap(console.log)
  // .e(id)
}


/*--------------------------------:
    {~genNums: a(Int) =
      > a 0 | ( - a 1; a; True  )
            | ( 0    ; 0; False )
    }
    {~keepAsking: =
      { ~> Int } => { a(Int) = ( (); a; True ) }
    }

    gen { ~keepAsking } => gen { ~genNums } => { <~ Int }
:--------------------------------*/

; {
  const $funcGenNums =
    CPS.func(
      CPS.empty()
        .fmap(function () {
          if (this.lookup("a") > 0)
            return [this.lookup("a") - 1, [this.lookup("a")], true]
          return [[0], [0], false]
        })
      , [new Arg("a", Number)]
    )

  const $funcKeepAsking =
    CPS.func(
      CPS.empty()
        .chain(readStdIo(Number))
        .fmap(a => [[], [a], true])
      , []
    )

  CPS.empty()
    // .gather(just(1000000))
    .gen($funcKeepAsking)
    .gen($funcGenNums)
    .fmap(console.log)
  // .e(id)
}

/*--------------------------------:
    {(~> Int; 1)} => { ~fact: a(Int), b(Int) = 
        > a 1 | {(- a 1 ; * a b)} => ~fact 
              | { b } 
      } => {<~ Int}
:--------------------------------*/

(() => {
  const $fact = f =>
    CPS.func(
      CPS.empty()
        .condChain(function () {
          return cb => cb(this.lookup("a") > 1)
        }, CPS.func(
          CPS.empty()
            .fmap(function () {
              const rv = [this.lookup("a") - 1, this.lookup("a") * this.lookup("b")]
              console.dir(rv)
              return rv
            })
            .chain(f)
          , [new Arg("a", Number), new Arg("b", Number)]
        ))
        .condChain(function () {
          return cb => cb(this.lookup("a") <= 1)
        }, function () {
          return cb => cb(this.lookup("b"))
        })
      , [new Arg("a", Number), new Arg("b", Number)]
    )

  CPS.empty()
    .gather(just(5), just(1))
    .chain($fact($fact))
    .fmap(console.log)
    .e(id)

  CPS.empty()
    .define([new Arg("flag", Boolean).assign(false)])
    .gather(just(3))
    .condChain(
      function () {
        return cb => cb(this.lookup("flag"))
      },
      function (a) {
        return cb => cb(a - 5)
      }
    )
    .fmap(console.log)
  // .e(id)
})()
