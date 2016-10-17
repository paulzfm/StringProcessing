/**
  * Created by paul on 06/10/2016.
  */

import Program._
import StringUtils.StringUtilsClass
import Utils._

object ProgramSet {

  // NOTE: SIDE EFFECT
  object ErrorLog {
    private var lastLog: String = ""

    def push(expr: String, msg: String): Unit = lastLog = s"$expr: $msg."

    def show: String = lastLog

    override def toString: String = lastLog
  }

  // type BaseNode[T] := T | (BaseNode[T], BaseNode[T])
  abstract class BaseNode[T] {
    def toString: String

    def toStringWith(f: T => String): String
  }

  // type Atom <: BaseNode
  case class Atom[T](k: T) extends BaseNode[T] {
    override def toString: String = k.toString

    def toStringWith(f: T => String): String = f(k)
  }

  // type Pair <: BaseNode
  case class Pair[T](x: BaseNode[T], y: BaseNode[T]) extends BaseNode[T] {
    override def toString: String = s"($x, $y)"

    def toStringWith(f: T => String): String = s"(${x.toStringWith(f)}, ${y.toStringWith(f)})"
  }

  type Node = BaseNode[Int]
  type AtomNode = Atom[Int]

  type PairNode = Pair[Int]

  class TraceExprSet(val nodes: List[Node], val startNode: Node, val terminateNode: Node,
                     val edges: List[(Node, Node)],
                     val w: Map[(Node, Node), List[AtomExprSet]]) {
    lazy val isEmpty: Boolean = w.forall(_._2.isEmpty)

    lazy val nonEmpty: Boolean = !isEmpty

    def template(f: (AtomExprSet, AtomExprSet) => Option[AtomExprSet])
                (that: TraceExprSet): TraceExprSet = {
      val nodes1 = for {
        x <- nodes
        y <- that.nodes
      } yield Pair(x, y)
      val edges1 = for {
        (x1, x2) <- edges
        (y1, y2) <- that.edges
      } yield (Pair(x1, y1), Pair(x2, y2))
      val w1 = (for {
        (x1, x2) <- edges
        (y1, y2) <- that.edges
      } yield (
        (Pair(x1, y1): Node, Pair(x2, y2): Node),
        for {
          f1 <- w(x1, x2)
          f2 <- that.w(y1, y2)
          e <- f(f1, f2)
        } yield e)).toMap

      new TraceExprSet(nodes1, Pair(startNode, that.startNode),
        Pair(terminateNode, that.terminateNode), edges1, w1)
    }

    def intersect: TraceExprSet => TraceExprSet = template(_.intersect(_))

    def unify: TraceExprSet => TraceExprSet = template(_.unify(_))

    lazy val size: Int = {
      def sz(n: Node): Int = {
        if (n == startNode) 1
        else (for {
          m <- nodes
          if m != n
          f <- w.get((m, n))
        } yield sz(m) * f.map(_.size).sum).sum
      }
      sz(terminateNode)
    }

    override def toString: String = {
      val s = w map {
        case (edge, exprs) => s"  $edge -> {${exprs.mkString(", ")}}"
      } mkString "\n"
      s"TraceExpr(\n$s\n)"
    }

    def check(expected: BaseNode[String], sigma: BaseNode[InputType]): Boolean = {
      def dimensionError: Boolean = {
        val e = expected.toStringWith(_.length.toString)
        ErrorLog.push("TraceExprSet", s"dimension mismatch: $e expected, $terminateNode found")
        false
      }

      def checkDimension(node: BaseNode[Int], exp: BaseNode[String]): Boolean =
        (node, exp) match {
          case (Atom(n), Atom(e)) => if (n == e.length) true else dimensionError
          case (Pair(n1, n2), Pair(e1, e2)) =>
            if (checkDimension(n1, e1) && checkDimension(n2, e2)) true
            else dimensionError
        }

      def pairs(x: Node, y: Node, e: BaseNode[String], sigma: BaseNode[InputType])
      : List[(Int, Int, String, InputType)] = (x, y, e, sigma) match {
        case (Atom(i), Atom(j), Atom(exp), Atom(input)) => List((i, j, exp, input))
        case (Pair(x1, x2), Pair(y1, y2), Pair(e1, e2), Pair(s1, s2)) =>
          pairs(x1, y1, e1, s1) ++ pairs(x2, y2, e2, s2)
      }

      val result = checkDimension(terminateNode, expected) && w.forall {
        case ((x, y), exprs) => pairs(x, y, expected, sigma).forall {
          case (i, j, e, input) => exprs.forall(_.check(e.substring(i, j), input))
        }
      }
      if (result) true
      else {
        ErrorLog.show
        false
      }
    }

    @deprecated(message = "Use `check` instead, which is more clear.")
    def check1(expected: BaseNode[String], sigma: BaseNode[InputType]): Boolean = {
      def dimensionError: Boolean = {
        val e = expected.toStringWith(_.length.toString)
        ErrorLog.push("TraceExprSet", s"dimension mismatch: $e expected, $terminateNode found")
        false
      }

      def checkDimension(node: BaseNode[Int], exp: BaseNode[String]): Boolean =
        (node, exp) match {
          case (Atom(n), Atom(e)) => if (n == e.length) true else dimensionError
          case (Pair(n1, n2), Pair(e1, e2)) =>
            if (checkDimension(n1, e1) && checkDimension(n2, e2)) true
            else dimensionError
        }

      def helper[U <: Node](exp: BaseNode[String], sig: BaseNode[InputType],
                            w: Map[(U, U), List[AtomExprSet]]): Boolean = {
        (exp, sig) match {
          case (Atom(e), Atom(s)) => w.forall {
            case ((Atom(i: Int), Atom(j: Int)), exprs) =>
              exprs.forall(_.check(e.substring(i, j), s))
          }
          case (Pair(e1, e2), Pair(s1, s2)) =>
            val w1 = w.toList groupBy {
              case ((Pair(i, _), Pair(j, _)), exprs) => (i, j)
            } mapValues {
              xs => xs.flatMap(_._2).distinct
            }
            val w2 = w.toList groupBy {
              case ((Pair(_, i), Pair(_, j)), exprs) => (i, j)
            } mapValues {
              xs => xs.flatMap(_._2).distinct
            }
            helper(e1, s1, w1) && helper(e2, s2, w2)
        }
      }

      checkDimension(terminateNode, expected) && helper(expected, sigma, w)
    }
  }

  abstract class AtomExprSet {
    def size: Int

    def intersect: AtomExprSet => Option[AtomExprSet]

    def unify: AtomExprSet => Option[AtomExprSet]

    def eval(sigma: InputType): Set[String]

    def toString: String

    def check(expected: String, sigma: InputType): Boolean
  }

  case class ConstStrSet(s: String) extends AtomExprSet {
    def intersect: AtomExprSet => Option[AtomExprSet] = {
      case ConstStrSet(s1) => if (s == s1) Some(this) else None
      case _ => None
    }

    def unify = intersect

    val size = 1

    def eval(sigma: InputType): Set[String] = Set(s)

    override def toString: String = s"""ConstStr("$s")"""

    def check(expected: String, sigma: InputType): Boolean = {
      if (s == expected) true
      else {
        ErrorLog.push(toString, s"$expected expected")
        false
      }
    }
  }

  case class SubStrSet(i: Int, p: List[PosExprSet], q: List[PosExprSet]) extends AtomExprSet {
    def template(f: (PosExprSet, PosExprSet) => Option[PosExprSet])
                (that: AtomExprSet): Option[AtomExprSet] = that match {
      case SubStrSet(i1, p1, q1) =>
        if (i == i1) {
          val ps1 = (for {
            x <- p
            y <- p1
            e <- f(x, y)
          } yield e).distinct
          val ps2 = (for {
            x <- q
            y <- q1
            e <- f(x, y)
          } yield e).distinct
          if (ps1.isEmpty || ps2.isEmpty) None
          else Some(SubStrSet(i, ps1, ps2))
        }
        else None
      case _ => None
    }

    def intersect: AtomExprSet => Option[AtomExprSet] = template(_.intersect(_))

    def unify: AtomExprSet => Option[AtomExprSet] = template(_.unify(_))

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
        ErrorLog.push(toString,
          s"""cannot obtain expected string "$expected" from input "${sigma(i)}"""")
        false
      }
    }
  }

  case class LoopSet(e: TraceExprSet) extends AtomExprSet {
    def template(f: (TraceExprSet, TraceExprSet) => TraceExprSet)
                (that: AtomExprSet): Option[AtomExprSet] = that match {
      case LoopSet(e1) => Some(LoopSet(f(e, e1)))
      case _ => None
    }

    def intersect: AtomExprSet => Option[AtomExprSet] = template(_.intersect(_))

    def unify: AtomExprSet => Option[AtomExprSet] = template(_.unify(_))

    def eval(sigma: InputType): Set[String] = {
      //      (for {
      //        s <- e.map(Loop(_).eval(sigma))
      //        if s.isDefined
      //      } yield s.get).toSet
      ???
    }

    lazy val size = e.size

    def check(expected: String, sigma: InputType): Boolean = ???
  }

  abstract class PosExprSet {
    def size: Int

    def intersect: PosExprSet => Option[PosExprSet]

    def unify: PosExprSet => Option[PosExprSet]

    def eval(s: String): Set[Int] = ???

    def toString: String

    def check(expected: Int, s: String): Boolean
  }

  case class CPosSet(k: Int) extends PosExprSet {
    val intersect: PosExprSet => Option[PosExprSet] = {
      case CPosSet(k1) => if (k == k1) Some(this) else None
      case _ => None
    }

    val unify = intersect

    val size = 1

    override def toString: String = s"CPos($k)"

    def check(expected: Int, s: String): Boolean = {
      val expected1 = -(s.length - expected)
      if (k == expected || k == expected1) true
      else {
        ErrorLog.push(toString, s"$expected or $expected1 expected")
        false
      }
    }
  }

  case class PosSet(r1: TokenSeq, r2: TokenSeq, c: IntegerExprSet) extends PosExprSet {
    val intersect: PosExprSet => Option[PosExprSet] = {
      case PosSet(s1, s2, d) =>
        (r1.intersect(s1), r2.intersect(s2), c.intersect(d)) match {
          case (Some(x1), Some(x2), Some(e)) => Some(PosSet(x1, x2, e))
          case _ => None
        }
      case _ => None
    }

    val unify = intersect

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
              ErrorLog.push(p.toString, s"$other found, $expected expected")
              false
            case None =>
              ErrorLog.push(p.toString, s"none found, $expected expected")
              false
          }
        }
      }
    }
  }

  class IntegerExprSet(val e: Set[IntegerExpr]) {
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
      else {
        val tss = (tokenss zip that.tokenss) map {
          case (x, y) => x.intersect(y)
        }
        if (tss.exists(_.isEmpty)) None
        else Some(new TokenSeq(tss))
      }
    }

    lazy val repsRegularExpr: RegularExpr = new RegularExpr(tokenss.map(_.head))

    override def toString: String = {
      val s = tokenss map {
        case Nil => ""
        case x :: xs => x.toString
      } mkString ""
      s"{$s}"
    }

    def check(s: String): Boolean = {
      def indistinguishable(tokens: List[Token]): Boolean = {
        lazy val matches = tokens.map(_.findMatchesIn(s))
        if (allSame(matches)) true
        else {
          ErrorLog.push(s"{${tokens.mkString(", ")}}", "tokens are NOT indistinguishable with " +
            s"""respect to "$s"""")
          false
        }
      }

      tokenss.forall(indistinguishable)
    }
  }

}
