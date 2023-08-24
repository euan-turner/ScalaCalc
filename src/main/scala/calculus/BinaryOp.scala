package calculus

enum BinaryOp(val op: (Double, Double) => Double, val str: String):
  case Add extends BinaryOp((x, y) => x + y, "+")
  case Mul extends BinaryOp((x, y) => x * y, "*")
  case Div extends BinaryOp((x, y) => x / y, "/")
