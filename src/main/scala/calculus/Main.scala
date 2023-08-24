package calculus

@main def run(): Unit =
  println(e1)
  println(e2)
  println(e1.eval(environment))
  println(e2.eval(environment))
  val e1dx = e1.diff("x")
  val e2dx = e2.diff("x")
  val e2dy = e2.diff("y")
  println(e1dx)
  println(e1dx.simplify())
  println(e2dx)
  println(e2dx.simplify())
  println(e2dy)
  println(e2dy.simplify())
