/**
  * Created by paul on 9/29/16.
  */

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Program._

@RunWith(classOf[JUnitRunner])
class TestProgram extends FunSuite {

  val cases = Map(
    2 -> List(
      (Vector("BTR KRNL WK CORN 15Z"), "15Z"),
      (Vector("CAMP DRY DBL NDL 3.6 OZ"), "3.6 OZ"),
      (Vector("CHORE BOY HD SC SPNG 1 PK"), "1 PK"),
      (Vector("FRENCH WORCESTERSHIRE 5 Z"), "5 Z"),
      (Vector("O F TOMATO PASTE 6 OZ"), "6 OZ")
    ),
    3 -> List(
      (Vector("Company\\Code\\index.html"), "Company\\Code\\"),
      (Vector("Company\\Docs\\Spec\\specs.doc"), "Company\\Docs\\Spec\\")
    ),
    4 -> List(
      (Vector("International Business Machines"), "IBM"),
      (Vector("Principle Of Programming Languages"), "POPL"),
      (Vector("International Conference on Software Engineering"), "ICSE")
    ),
    5 -> List(
      (Vector("(6/7)(4/5)(14/1)"), "6/7# 4/5# 14/1# "),
      (Vector("49(28/11)(14/1)"), "28/11# 14/1# "),
      (Vector("()(28/11)(14/1)"), "28/11# 14/1# ")
    ),
    6 -> List(
      (Vector("   Oege     de       Moor        "), "Oege de Moor"),
      (Vector("Kathleen    Fisher      AT&T Labs"), "Kathleen Fisher AT&T Labs")
    ),
    7 -> List(
      (Vector("Alex", "Asst."), "Alex(Asst.)"),
      (Vector("Jim", "Manager"), "Jim(Manager)"),
      (Vector("Ryan", ""), ""),
      (Vector("", "Asst."), "")
    ),
    8 -> List(
      (Vector("01/21/2001"), "01"),
      (Vector("22.02.2002"), "02"),
      (Vector("2003-23-03"), "03")
    ),
    9 -> List(
      (Vector("Dr. Eran Yahav"), "Yahav, E."),
      (Vector("Prof. Kathleen S. Fisher"), "Fisher, K."),
      (Vector("Bill Gates, Sr."), "Gates, B."),
      (Vector("George Ciprian Necula"), "Necula, G."),
      (Vector("Ken McMillan, II"), "McMillan, K.")
    ),
    10 -> List(
      (Vector("323-708-7700"), "323-708-7700"),
      (Vector("(425)-706-7709"), "425-706-7709"),
      (Vector("510.220.5586"), "510-220-5586"),
      (Vector("235 7654"), "425-235-7654"),
      (Vector("745-8139"), "425-745-8139")
    ),
    11 -> List(
      (Vector("Otis", "Daniels"), "Otis, D."),
      (Vector("Kimberly", "Jones"), "Kimberly, J."),
      (Vector("Mary", "Leslie"), "Mary, L.")
    ),
    12 -> List(
      (Vector("Albania", "355"), "case 355: return “Albania”;"),
      (Vector("Algeria", "213"), "case 213: return “Algeria”;")
    ),
    13 -> List(
      (Vector("/um/people/sumitg/pictures/lake-tahoe/index.html", "192"), "192"),
      (Vector("/um/people/sumitg/index.html", "104"), "0"),
      (Vector("/um/people/sumitg/pubs/speed.html", "16"), "0"),
      (Vector("/um/people/sumitg/pubs/popl10 synthesis.pdf", "13"), "0"),
      (Vector("/um/people/sumitg/pictures/verona/index.html", "7"), "7"),
      (Vector("/um/people/sumitg/pictures/kerela/target21.html", "3"), "3")
    ),
    14 -> List(
      (Vector("Alpha 10 Beta 20 Charlie 30 Delta"), "10+20+30"),
      (Vector("POPL 9 CAV 7 PLDI 6 ESOP 4"), "9+7+6+4")
    )
  )

  val programs = Map(
    2 -> SubStr(
      0,
      Pos(
        emptyRegex,
        new RegularExpr(List(OneOrMore(Numeric))),
        CInt(1)),
      CPos(-1)),
    3 -> SubStr(
      0,
      CPos(0),
      Pos(
        new RegularExpr(List(SpecialToken('\\'))),
        emptyRegex,
        CInt(-1)
      )
    ),
    4 -> Loop(new TraceExpr(List(
      SubStr2(0, new RegularExpr(List(OneOrMore(UpperCases))), Linear(1))
    ))),
    5 -> Loop(new TraceExpr(List(
      SubStr(
        0,
        Pos(
          new RegularExpr(List(SpecialToken('('))),
          new RegularExpr(List(OneOrMore(Numeric), SpecialToken('/'))),
          Linear(1)
        ),
        Pos(
          new RegularExpr(List(SpecialToken('/'), OneOrMore(Numeric))),
          new RegularExpr(List(SpecialToken(')'))),
          Linear(1)
        )
      ),
      ConstStr("# ")
    ))),
    6 -> new TraceExpr(List(
      Loop(new TraceExpr(List(
        SubStr(
          0,
          Pos(
            emptyRegex,
            new RegularExpr(List(ExcludeOneOrMore(Whitespaces))),
            Linear(1)
          ),
          Pos(
            new RegularExpr(List(ExcludeOneOrMore(Whitespaces))),
            new RegularExpr(List(OneOrMore(Whitespaces), ExcludeOneOrMore(Whitespaces))),
            Linear(1)
          )
        ),
        ConstStr(" ")
      ))),
      SubStr2(0, new RegularExpr(List(ExcludeOneOrMore(Whitespaces))), CInt(-1))
    )),
    7 -> new StringProgram(List(
      (new Bool(List(
        new Conjunction(List(
          Match(0, new RegularExpr(List(OneOrMore(Letters)))),
          Match(1, new RegularExpr(List(OneOrMore(Letters))))
        ))
      )), new TraceExpr(List(
        InputStr(0),
        ConstStr("("),
        InputStr(1),
        ConstStr(")")
      ))),
      (new Bool(List(
        new Conjunction(List(NotMatch(0, new RegularExpr(List(OneOrMore(Letters)))))),
        new Conjunction(List(NotMatch(1, new RegularExpr(List(OneOrMore(Letters))))))
      )), emptyString)
    )),
    8 -> new StringProgram(List(
      (new Bool(List(
        new Conjunction(List(Match(0, new RegularExpr(List(SpecialToken('/'))))))
      )), new TraceExpr(List(SubStr(
        0,
        Pos(new RegularExpr(List(StartToken)), emptyRegex, CInt(1)),
        Pos(emptyRegex, new RegularExpr(List(SpecialToken('/'))), CInt(1))
      )))),
      (new Bool(List(
        new Conjunction(List(Match(0, new RegularExpr(List(SpecialToken('.'))))))
      )), new TraceExpr(List(SubStr(
        0,
        Pos(new RegularExpr(List(SpecialToken('.'))), emptyRegex, CInt(1)),
        Pos(emptyRegex, new RegularExpr(List(SpecialToken('.'))), CInt(2))
      )))),
      (new Bool(List(
        new Conjunction(List(Match(0, new RegularExpr(List(SpecialToken('-'))))))
      )), new TraceExpr(List(SubStr(
        0,
        Pos(new RegularExpr(List(SpecialToken('-'))), emptyRegex, CInt(2)),
        Pos(new RegularExpr(List(EndToken)), emptyRegex, CInt(1))
      ))))
    )),
    9 -> {
      def f(e: SubStr): TraceExpr = new TraceExpr(List(
        e,
        ConstStr(", "),
        SubStr(
          0,
          Pos(emptyRegex, new RegularExpr(List(OneOrMore(Letters), ExcludeSpecialToken('.'))),
            CInt(1)),
          Pos(emptyRegex, new RegularExpr(List(OneOrMore(LowerCases), ExcludeSpecialToken('.'))),
            CInt(1))
        ),
        ConstStr(".")
      ))

      new StringProgram(List(
        (new Bool(List(
          new Conjunction(List(Match(0, new RegularExpr(List(SpecialToken(','))))))
        )), f(SubStr(
          0,
          Pos(emptyRegex, new RegularExpr(List(OneOrMore(Letters), SpecialToken(','))), CInt(1)),
          Pos(new RegularExpr(List(OneOrMore(Letters))), new RegularExpr(List(SpecialToken(','))),
            CInt(1))
        ))),
        (new Bool(List(
          new Conjunction(List(NotMatch(0, new RegularExpr(List(SpecialToken(','))))))
        )), f(SubStr2(
          0,
          new RegularExpr(List(OneOrMore(Letters))),
          CInt(-1)
        )))
      ))
    },
    10 -> {
      val numTok = new RegularExpr(List(OneOrMore(Numeric)))

      new StringProgram(List(
        (new Bool(List(
          new Conjunction(List(Match(0, numTok, 3)))
        )), new TraceExpr(List(
          SubStr2(0, numTok, CInt(1)),
          ConstStr("-"),
          SubStr2(0, numTok, CInt(2)),
          ConstStr("-"),
          SubStr2(0, numTok, CInt(3))
        ))),
        (new Bool(List(
          new Conjunction(List(NotMatch(0, numTok, 3)))
        )), new TraceExpr(List(
          ConstStr("425-"),
          SubStr2(0, numTok, CInt(1)),
          ConstStr("-"),
          SubStr2(0, numTok, CInt(2))
        )))
      ))
    },
    11 -> new TraceExpr(List(
      InputStr(0),
      ConstStr(", "),
      SubStr(1, CPos(0), CPos(1)),
      ConstStr(".")
    )),
    12 -> new TraceExpr(List(
      ConstStr("case "),
      InputStr(1),
      ConstStr(": return “"),
      InputStr(0),
      ConstStr("”;")
    )),
    13 -> new StringProgram(List(
      (new Bool(List(
        new Conjunction(List(Match(0, new RegularExpr(List(SpecialToken('/'))), 6)))
      )), new TraceExpr(List(InputStr(1)))),
      (new Bool(List(
        new Conjunction(List(NotMatch(0, new RegularExpr(List(SpecialToken('/'))), 6)))
      )), new TraceExpr(List(ConstStr("0"))))
    )),
    14 -> new TraceExpr(List(
      Loop(new TraceExpr(List(
        SubStr(
          0,
          Pos(emptyRegex, new RegularExpr(List(OneOrMore(Numeric))), Linear(1)),
          Pos(
            new RegularExpr(List(OneOrMore(Numeric))),
            new RegularExpr(List(ExcludeOneOrMore(Numeric), OneOrMore(Numeric))),
            Linear(1)
          )
        ),
        ConstStr("+")
      ))),
      SubStr2(0, new RegularExpr(List(OneOrMore(Numeric))), CInt(-1))
    ))
  )

  val x = 0
  val y = 3

  if (x == 0)
    programs.keys.toList.sorted.foreach { k =>
      println(s"[info] Example Program $k")
      val p = programs(k)
      println(p.code())

      var count = 0
      cases(k).foreach {
        case (i, o) =>
          count += 1
          test(s"Example $k - $count") {
            assert(p.eval(i).contains(o))
          }
      }
    }
  else
    test(s"Example $x - $y") {
      val p = programs(x)
      println(p.code())
      cases(x)(y - 1) match {
        case (i, o) => assert(p.eval(i).contains(o))
      }
    }

}
