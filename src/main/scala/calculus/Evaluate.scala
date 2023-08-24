package calculus

import scala.math.{cos, log, sin}

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

def eval(e: Expression, env: Env): Double = e match
  case Value(v) => v
  case Variable(id) => lookUp(id, env)
  case UnaryApp(op, exp) => getUnaryOp(op)(eval(exp, env))
  case BinaryApp(op, exp1, exp2) => getBinaryOp(op)(eval(exp1, env), eval(exp2, env))

