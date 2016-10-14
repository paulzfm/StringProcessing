import Program._

val l = new Learner(Vector((Vector("123-456"), "123")))

val s = "2003-23-03"


Pos(new RegularExpr(SpecialToken('-')), emptyRegex, CInt(2)).eval(s)
Pos(new RegularExpr(EndToken), emptyRegex, CInt(1)).eval(s)