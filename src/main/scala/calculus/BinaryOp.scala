package calculus

sealed trait BinaryOp
case object Add extends BinaryOp
case object Mul extends BinaryOp
case object Div extends BinaryOp
