package calculus

import scala.math._

sealed trait UnaryOp
case object Neg extends UnaryOp
case object Sin extends UnaryOp
case object Cos extends UnaryOp
case object Log extends UnaryOp

sealed trait BinaryOp
case object Add extends BinaryOp
case object Mul extends BinaryOp
case object Div extends BinaryOp

sealed trait Expression
case class Value(value: Double) extends Expression
case class Variable(id: String) extends Expression
case class UnaryApp(op: UnaryOp, exp: Expression) extends Expression
case class BinaryApp(op: BinaryOp, exp1: Expression, exp2: Expression) extends Expression

type Env = List[(String, Double)]

def lookUp(id: String, env: Env): Double =
  val res = env.find((name, value) => name.equals(id)).head
  res._2

def getUnaryOp(op: UnaryOp): Double => Double = op match
  case Neg => (x: Double) => -x
  case Sin => sin
  case Cos => cos
  case Log => log

def getBinaryOp(op: BinaryOp): (Double, Double) => Double = op match
  case Add => (x: Double, y: Double) => x + y
  case Mul => (x: Double, y: Double) => x * y
  case Div => (x: Double, y: Double) => x / y

def showUnary(op: UnaryOp): String = op match
  case Neg => "-"
  case Sin => "sin"
  case Cos => "cos"
  case Log => "log"

def showBinary(op: BinaryOp): String = op match
  case Add => "+"
  case Mul => "*"
  case div => "/"

def showExp(e: Expression): String = e match
  case Value(v) => v.toString
  case Variable(id) => id
  case UnaryApp(op, exp) => showUnary(op) + "(" + showExp(exp) + ")"
  case BinaryApp(op, exp1, exp2) => showExp(exp1) + " " + showBinary(op) + " " + showExp(exp2)

def eval(e: Expression, env: Env): Double = e match
  case Value(v) => v
  case Variable(id) => lookUp(id, env)
  case UnaryApp(op, exp) => getUnaryOp(op)(eval(exp, env))
  case BinaryApp(op, exp1, exp2) => getBinaryOp(op)(eval(exp1, env), eval(exp2, env))

val environment: Env = List(("x", 4), ("y", 5))

val e1: Expression = BinaryApp(Mul, Value(5.0), Variable("x"))
val e2: Expression = BinaryApp(Add,
  BinaryApp(Add,
    BinaryApp(Mul, Variable("x"), Variable("x")),
    Variable("y")),
  UnaryApp(Neg, Value(7.0)))

@main def run() =
  println(showExp(e1))
  println(showExp(e2))
  println(eval(e1, environment))
  println(eval(e2, environment))
