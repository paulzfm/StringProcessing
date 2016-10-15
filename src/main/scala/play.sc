import Program._
import StringUtils._

val sigma = Vector((Vector("(6/7)(4/5)(14/1)"), "6/7# 4/5# 14/1# "))

val input = sigma(0)._1
val output = sigma(0)._2

val learner = new Learner(sigma)

val p = learner.genTraceExpr(input, output)

p.check(output, input)