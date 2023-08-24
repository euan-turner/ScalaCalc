package calculus

import scala.math.{cos, log, sin}

enum UnaryOp(val op: Double => Double, val str: String):
  case Neg extends UnaryOp((x: Double) => -x, "-")
  case Sin extends UnaryOp(sin, "sin")
  case Cos extends UnaryOp(cos, "cos")
  case Log extends UnaryOp(log, "log")