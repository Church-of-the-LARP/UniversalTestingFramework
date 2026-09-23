"""Bridged functions for service 'spec'.

This file was scaffolded by `utf gen python` and is yours to edit; the
generator never overwrites it unless --force is passed. Keep the parameter
names, order and type annotations in sync with the OCaml signature -- the
wrapper server (spec_server.py) validates the implementation at startup.
"""


def add(a: int, b: int) -> int:
    return a + b


def greet(s: str) -> bool:
    return len(s) > 0


def names(n: int) -> list[str]:
    return [str(i) for i in range(n)]


def scale(x: float, y: float) -> float:
    return x * y


def echo(s: str) -> str:
    return s


def pair_sum(arg1: int, arg2: int) -> int:
    """Positional parameters are named arg1, arg2, ... by the generator."""
    return arg1 + arg2


def squares(n: int) -> list[list[int]]:
    return [[i, i * i] for i in range(n)]


def explode(x: int) -> int:
    """Deliberately raises, to show how remote errors reach OCaml."""
    raise ValueError("boom")


def wrong_type(x: int) -> int:
    """Deliberately violates the signature, to show the typed boundary."""
    return "not an int"  # type: ignore[return-value]
