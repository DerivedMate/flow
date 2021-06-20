from abc import abstractclassmethod
from collections import deque
from functools import reduce
from typing import List
# prev_return -> next -> void
# p a -> ( a -> b ) -> p b


class M():
    def __init__(self, a):
        self.val = a
        if isinstance(self.val, M):
            self.val = self.val.unpack()

    def __call__(self, *_, **__):
        ()

    @staticmethod
    def pure(a):
        return M(a)

    @staticmethod
    def empty():
        return M(id)

    def unpack(self):
        return self.val

    def chain(self, f):
        return M.pure(lambda cb: self.unpack()(lambda *a: f(*a)(cb)))

    def fmap(self, f):
        def g(cb): return self.unpack()(lambda *a: cb(f(*a)))
        return M.pure(g)

    def keep(self, pred):
        return M.pure(lambda cb: self.unpack()(lambda *args: cb(*args) if pred(*args) else None))

    def ignore(self, f):
        self.val = f
        return self

    def exec(self):
        return M.pure(self.unpack()(id))


def nums(n):
    return lambda cb: [cb(k) for k in range(n+1)]


def getInputs(question: str):
    def aux(cb):
        i = 0
        while True:
            cb((i, input(question)))
            i += 1

    return aux


def readStdIo(parser):
    return lambda cb: cb(parser(input()))


def printStdIo(parser):
    return lambda a: lambda cb: cb(print(parser(a)))


def gather(*fs):
    state = {
        "acc": [deque() for _ in fs]
    }

    def aux(cb):
        def make_solver(i):
            def solver(v):
                state["acc"][i].append(v)

                if all([len(queue) > 0 for queue in state["acc"]]):
                    cb(*[queue.popleft() for queue in state["acc"]])

            return solver

        [f(make_solver(i)) for i, f in enumerate(fs)]

    return aux


def just(a):
    return lambda cb: cb(a)


"""
  { (1; 2) } => { a(Int), b(Int) = + a b } => { <~ Int }
"""


"""
  { ( 12; 11 ) } => filter { a(Int), b(Int) = && > a 10 > b 10 } 
                 => { a(Int), b(Int) = + a b }
                 => { <~ Int }
  { 5 } => { <~ Int }
"""
"""
(M.empty()
 .ignore(gather(just(12), just(11)))
 .keep(lambda a, b: a > 10 and b > 10)
 .fmap(lambda a, b: a + b)
 .fmap(printStdIo(int))
 .exec()
 .ignore(just(5))
 .fmap(printStdIo(int))
 .exec()
 )
"""

"""
  { ~> Int } => { <~ Int }
"""
"""(M.empty()
 .ignore(readStdIo(int))
 .fmap(printStdIo(int))
 .exec()
 )"""

"""
  { <~ Int }
"""

