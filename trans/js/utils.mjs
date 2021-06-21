export class Fifo {
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


export const trampoline = (f) => {
  return (...args) => {
    let r = f(...args)
    while (typeof r === "function") r = r()
    return r
  }
}

