package calculus

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

