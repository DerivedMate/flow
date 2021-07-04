export class Fifo {
  start = 0
  end = 0
  store = []

  constructor(len = 0) {
    this.store = new Array(len).fill(undefined)
  }

  pop() {
    /*--------------------------------:
            Handle empty store
    :--------------------------------*/
    if (this.is_empty())
      return undefined

    const out = this.store[this.start]
    this.store[this.start] = undefined
    this.start = this.__iter_index(this.start)

    return out
  }

  /*--------------------------------:
          Append a new element 
          to the end
  :--------------------------------*/
  append(v) {
    this.store[this.end] = v
    this.end = this.__iter_index(this.end)
    return this
  }

  is_empty() {
    return this.store[this.start] === undefined
  }

  length() {
    return this.end - this.start + this.store.length * (+(this.start > this.end))
  }

  __iter_index(a) {
    return (a + 1) % this.store.length
  }
}

export const call = (f, ...args) => setTimeout(f, 0, ...args)
export const ensureIter = a => {
  try {
    return (Object.getOwnPropertySymbols(a).includes(Symbol.iterator)
      ? a
      : [a])
  } catch {
    return []
  }
}