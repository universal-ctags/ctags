// Taken from https://openjdk.org/jeps/409

public sealed interface Shape permits Circle, Square, Rectangle { }

sealed class Base permits Derived { }

sealed interface Expr extends Node permits Constant, Plus { }

non-sealed interface Node { }
