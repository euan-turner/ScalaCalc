package calculus

import UnaryOp._
import BinaryOp._

type Env = List[(String, Double)]

def lookUp(id: String, env: Env): Double =
  env.collectFirst { case (`id`, value) => value }.getOrElse(0.0)

sealed trait Expression:
  def eval(env: Env): Double
  def diff(v: String): Expression
  def simplify(): Expression

case class Value(value: Double) extends Expression:
  override def toString: String = s"$value"
  override def eval(env: Env): Double = value
  override def diff(v: String): Expression = Value(0)
  override def simplify(): Expression = this

case class Variable(id: String) extends Expression:
  override def toString: String = id
  override def eval(env: Env): Double = lookUp(id, env)
  override def diff(v: String): Expression = if (v.equals(id)) Value(1) else Value(0)
  override def simplify(): Expression = this

case class UnaryApp(op: UnaryOp, exp: Expression) extends Expression:
  override def toString: String = s"${op.str}($exp)"
  override def eval(env: Env): Double = op.op(exp.eval(env))
  override def diff(v: String): Expression = op match
    case Neg => UnaryApp(Neg, exp.diff(v))
    case Sin => BinaryApp(Mul, UnaryApp(Cos, exp), exp.diff(v))
    case Cos => UnaryApp(Neg, BinaryApp(Mul, UnaryApp(Sin, exp), exp.diff(v)))
    case Log => BinaryApp(Div, exp.diff(v), exp)
  override def simplify(): Expression = {
    val e = exp.simplify()
    op match
      case Neg => e match
        case Value(0) => Value(0)
        case UnaryApp(Neg, e2) => e2
        case _ => UnaryApp(op, e)
      case _ => UnaryApp(op, e)
  }

case class BinaryApp(op: BinaryOp, exp1: Expression, exp2: Expression) extends Expression:
  override def toString: String = s"$exp1 ${op.str} $exp2"
  override def eval(env: Env): Double = op.op(exp1.eval(env), exp2.eval(env))
  override def diff(v: String): Expression = op match
    case Add => BinaryApp(Add, exp1.diff(v), exp2.diff(v))
    case Mul => BinaryApp(Add,
      BinaryApp(Mul, exp1, exp2.diff(v)),
      BinaryApp(Mul, exp1.diff(v), exp2))
    case Div => BinaryApp(Div,
      BinaryApp(Add,
        BinaryApp(Mul, exp2, exp1.diff(v)),
        UnaryApp(Neg, BinaryApp(Mul, exp1, exp2.diff(v)))),
      BinaryApp(Mul, exp2, exp2)
    )
  override def simplify(): Expression = {
    val e1 = exp1.simplify()
    val e2 = exp2.simplify()
    val res = BinaryApp(op, e1, e2)

    (op, e1, e2) match {
      case (Add, _, Value(0)) => e1
      case (Add, Value(0), _) => e2
      case (Mul, _, Value(0)) | (Mul, Value(0), _) => Value(0)
      case (Mul, _, Value(1)) => e1
      case (Mul, Value(1), _) => e2
      case (Div, Value(0), _) => Value(0)
      case _ => res
    }
  }


