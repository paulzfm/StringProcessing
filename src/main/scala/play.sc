import ProgramSet._

val examples = List(
  (Vector("323-708-7700"), "323-708-7700"),
  (Vector("(425)-706-7709"), "425-706-7709"),
  (Vector("510.220.5586"), "510-220-5586"),
  (Vector("235 7654"), "425-235-7654"),
  (Vector("745-8139"), "425-745-8139")
)
val i1 = examples.head._1
val o1 = examples.head._2
val i2 = examples(1)._1
val o2 = examples(1)._2
val l = new Learner(examples)

val p1 = l.genTraceExpr(i1, o1)
p1.check(Atom(o1), Atom(i1))
val p2 = l.genTraceExpr(i2, o2)
p2.check(Atom(o2), Atom(i2))
val e = p1.intersect(p2)
e.check(Pair(Atom(o1), Atom(o2)), Pair(Atom(i1), Atom(i2)))