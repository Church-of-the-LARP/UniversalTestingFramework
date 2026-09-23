"""Implementation of service 'poc' (the proof-of-concept demo).

This file is yours: `utf gen python` creates it once and never overwrites it
unless --force is passed. Keep the parameter names, order and type
annotations in sync with examples/poc/poc.ml -- the wrapper server checks
them and refuses to start on drift.
"""


def total(xs: list[int]) -> int:
    return sum(xs)


def repeat(s: str, n: int) -> str:
    return s * n


def divide(a: int, b: int) -> int:
    if b == 0:
        raise ZeroDivisionError("division by zero")
    return a // b
