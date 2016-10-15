/**
  * Created by paul on 06/10/2016.
  */

import Program._
import Utils._
import StringUtils.StringUtilsClass

object ProgramSet {
  def checkErrorLog(expr: String, msg: String): Unit = println(s"$expr: $msg.")

  type Edge = (Int, Int)
  // (x, y) : (Int, Int) s.t. x <= y

  type Mapping = Map[Edge, List[AtomExprSet]]

  class TraceExprSet(val nodes: List[Int], val startNode: Int, val terminateNode: Int,
                     val edges: List[Edge], val w: Mapping) {
    def intersectOrUnify(callIntersect: Boolean)(that: TraceExprSet): TraceExprSet = {
      // x from this, y from that
      def index(x: Int, y: Int): Int = x * (that.terminateNode + 1) + y

      val nodes1 = for {
        x <- nodes
        y <- that.nodes
      } yield index(x, y)
      val edges1 = for {
        (x1, x2) <- edges
        (y1, y2) <- that.edges
      } yield (index(x1, y1), index(x2, y2))
      val w1 = (for {
        (x1, x2) <- edges
        (y1, y2) <- that.edges
      } yield (
        (index(x1, y1), index(x2, y2)),
        for {
          f1 <- w(x1, x2)
          f2 <- that.w(y1, y2)
          e = f1.intersectOrUnify(callIntersect)(f2)
          if e.isDefined
        } yield e.get)).toMap

      new TraceExprSet(nodes1, index(startNode, that.startNode),
        index(terminateNode, that.terminateNode), edges1, w1)
    }

    lazy val size: Int = {
      def sz(i: Int): Int = {
        if (i == startNode) 1
        else (for {
          j <- 0 until i
        } yield sz(j) * w((j, i)).map(_.size).sum).sum
      }
      sz(terminateNode)
    }

    def intersect(that: TraceExprSet): TraceExprSet = intersectOrUnify(true)(that)

    def unify(that: TraceExprSet): TraceExprSet = intersectOrUnify(false)(that)

    def map[T](f: TraceExpr => T): List[T] = ???

    override def toString: String = {
      val s = w map {
        case (edge, exprs) => s"  $edge -> {${exprs.mkString(", ")}}"
      } mkString "\n"
      s"TraceExpr(\n$s\n)"
    }

    def check(expected: String, sigma: InputType): Boolean = {
      if (terminateNode != expected.length) {
        checkErrorLog("TraceExprSet",
          s"length mismatch: $terminateNode found, ${expected.length} expected")
        false
      } else w.forall {
        case ((i, j), exprs) => exprs.forall(_.check(expected.substring(i, j), sigma))
      }
    }
  }

  abstract class AtomExprSet {
    def intersectOrUnify(callIntersect: Boolean)(that: AtomExprSet): Option[AtomExprSet]

    def size: Int

    def intersect(that: AtomExprSet): Option[AtomExprSet] = intersectOrUnify(true)(that)

    def unify(that: AtomExprSet): Option[AtomExprSet] = intersectOrUnify(false)(that)

    def eval(sigma: InputType): Set[String]

    def toString: String

    def check(expected: String, sigma: InputType): Boolean
  }

  case class ConstStrSet(s: String) extends AtomExprSet {
    def intersectOrUnify(callIntersect: Boolean)(that: AtomExprSet): Option[AtomExprSet] = that match {
      case ConstStrSet(s1) => if (s == s1) Some(that) else None
      case _ => None
    }

    val size = 1

    def eval(sigma: InputType): Set[String] = Set(s)

    override def toString: String = s"""ConstStr("$s")"""

    def check(expected: String, sigma: InputType): Boolean = {
      if (s == expected) true
      else {
        checkErrorLog(toString, s"$expected expected")
        false
      }
    }
  }

  case class SubStrSet(i: Int, p: List[PosExprSet], q: List[PosExprSet]) extends AtomExprSet {
    def intersectOrUnify(callIntersect: Boolean)(that: AtomExprSet): Option[AtomExprSet] = that match {
      case SubStrSet(i1, p1, q1) => if (i == i1) Some(SubStrSet(
        i,
        for {
          x <- p
          y <- p1
          e = x.intersectOrUnify(callIntersect)(y)
          if e.isDefined
        } yield e.get,
        for {
          x <- q
          y <- q1
          e = x.intersectOrUnify(callIntersect)(y)
          if e.isDefined
        } yield e.get
      ))
      else None
      case _ => None
    }

    lazy val size = p.map(_.size).sum * q.map(_.size).sum

    override def toString: String = s"SubStr($i, {${p.mkString(",")}}, {${q.mkString(",")}})"

    def eval(sigma: InputType): Set[String] = {
      ???
    }

    def check(expected: String, sigma: InputType): Boolean = {
      val str = sigma(i)
      if (str.indexesOf(expected).exists { index =>
        p.forall(_.check(index, str)) && q.forall(_.check(index + expected.length, str))
      }) true
      else {
        checkErrorLog(toString,
          s"""cannot obtain expected string "$expected" from input "${sigma(i)}"""")
        false
      }
    }
  }

  case class LoopSet(e: TraceExprSet) extends AtomExprSet {
    def intersectOrUnify(callIntersect: Boolean)(that: AtomExprSet): Option[AtomExprSet] = that match {
      case LoopSet(e1) => Some(LoopSet(e.intersectOrUnify(callIntersect)(e1)))
      case _ => None
    }

    def eval(sigma: InputType): Set[String] = {
      (for {
        s <- e.map(Loop(_).eval(sigma))
        if s.isDefined
      } yield s.get).toSet
    }

    lazy val size = e.size

    def check(expected: String, sigma: InputType): Boolean = ???
  }

  abstract class PosExprSet {
    def intersectOrUnify(callIntersect: Boolean)(that: PosExprSet): Option[PosExprSet]

    def size: Int

    def intersect(that: PosExprSet): Option[PosExprSet] = intersectOrUnify(true)(that)

    def unify(that: PosExprSet): Option[PosExprSet] = intersectOrUnify(false)(that)

    def eval(s: String): Set[Int] = ???

    def toString: String

    def check(expected: Int, s: String): Boolean
  }

  case class CPosSet(k: Int) extends PosExprSet {
    def intersectOrUnify(callIntersect: Boolean)(that: PosExprSet): Option[PosExprSet] = that match {
      case CPosSet(k1) => if (k == k1) Some(that) else None
      case _ => None
    }

    val size = 1

    override def toString: String = s"CPos($k)"

    def check(expected: Int, s: String): Boolean = {
      val expected1 = -(s.length - expected)
      if (k == expected || k == expected1) true
      else {
        checkErrorLog(toString, s"$expected or $expected1 expected")
        false
      }
    }
  }

  case class PosSet(r1: TokenSeq, r2: TokenSeq, c: IntegerExprSet) extends PosExprSet {
    def intersectOrUnify(callIntersect: Boolean)(that: PosExprSet): Option[PosExprSet] = that
    match {
      case PosSet(s1, s2, d) =>
        (r1.intersect(s1), r2.intersect(r2), c.intersectOrUnify(callIntersect)(d)) match {
          case (Some(x1), Some(x2), Some(e)) => Some(PosSet(x1, x2, e))
          case _ => None
        }
      case _ => None
    }

    lazy val size = c.size

    override def toString: String = s"Pos($r1, $r2, $c)"

    def check(expected: Int, s: String): Boolean = {
      r1.check(s) && r2.check(s) && {
        val reps1 = r1.repsRegularExpr
        val reps2 = r2.repsRegularExpr
        c.e.forall { c1 =>
          val p = Pos(reps1, reps2, c1)
          p.eval(s) match {
            case Some(pos) if pos == expected => true
            case Some(other) =>
              checkErrorLog(p.toString, s"$other found, $expected expected")
              false
            case None =>
              checkErrorLog(p.toString, s"none found, $expected expected")
              false
          }
        }
      }
    }
  }

  class IntegerExprSet(val e: Set[IntegerExpr]) {
    def intersectOrUnify(callIntersect: Boolean)(that: IntegerExprSet): Option[IntegerExprSet] = {
      if (callIntersect) intersect(that)
      else unify(that)
    }

    def intersect(that: IntegerExprSet): Option[IntegerExprSet] = {
      val s = e.intersect(that.e)
      if (s.isEmpty) None else Some(new IntegerExprSet(s))
    }

    lazy val filtered: Option[Int] = e.filter {
      case CInt(k) => k > 0
      case _ => false
    } match {
      case Seq(CInt(k)) => Some(k)
      case _ => None
    }

    def unify(that: IntegerExprSet): Option[IntegerExprSet] = (filtered, that.filtered) match {
      case (Some(k1), Some(k2)) =>
        if (k1 != k2) Some(new IntegerExprSet(Set(Linear(k2 - k1, k1)))) else None
      case _ => None
    }

    lazy val size = e.size

    override def toString: String = s"{${e.mkString(", ")}}"
  }

  class TokenSeq(val tokenss: List[List[Token]]) {
    def intersect(that: TokenSeq): Option[TokenSeq] = {
      if (tokenss.size != that.tokenss.size) None
      else Some(new TokenSeq(tokenss.zip(that.tokenss).map {
        case (x, y) => x.intersect(y)
      }))
    }

    lazy val repsRegularExpr: RegularExpr = new RegularExpr(tokenss.map(_.head))

    override def toString: String = s"{${tokenss.map(_.head).mkString("")}}"

    def check(s: String): Boolean = {
      def indistinguishable(tokens: List[Token]): Boolean = {
        lazy val matches = tokens.map(_.findMatchesIn(s))
        if (allSame(matches)) true
        else {
          checkErrorLog(s"{${tokens.mkString(", ")}}", "tokens are NOT indistinguishable with " +
            s"""respect to "$s"""")
          false
        }
      }

      tokenss.forall(indistinguishable)
    }
  }

}
