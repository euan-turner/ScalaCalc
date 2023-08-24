package calculus

sealed trait Expression
case class Value(value: Double) extends Expression
case class Variable(id: String) extends Expression
case class UnaryApp(op: UnaryOp, exp: Expression) extends Expression
case class BinaryApp(op: BinaryOp, exp1: Expression, exp2: Expression) extends Expression

type Env = List[(String, Double)]

val environment: Env = List(("x", 4), ("y", 5))

val e1: Expression = BinaryApp(Mul, Value(5.0), Variable("x"))
val e2: Expression = BinaryApp(Add,
  BinaryApp(Add,
    BinaryApp(Mul, Variable("x"), Variable("x")),
    Variable("y")),
  UnaryApp(Neg, Value(7.0)))
