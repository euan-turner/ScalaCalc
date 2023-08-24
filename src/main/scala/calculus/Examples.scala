package calculus

import calculus.BinaryOp._
import calculus.UnaryOp._

val environment: Env = List(("x", 4), ("y", 5))

val e1: Expression = BinaryApp(Mul, Value(5.0), Variable("x"))
val e2: Expression = BinaryApp(Add,
  BinaryApp(Add,
    BinaryApp(Mul, Variable("x"), Variable("x")),
    Variable("y")),
  UnaryApp(Neg, Value(7.0)))