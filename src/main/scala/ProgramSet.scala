/**
  * Created by paul on 06/10/2016.
  */

import Program._
import StringUtils.StringUtilsClass
import Utils._

import scala.collection.mutable.{Map => MutableMap}


object ProgramSet {

  // NOTE: SIDE EFFECT
  object ErrorLog {
    private var lastLog: String = ""

    def push(expr: String, msg: String): Unit = lastLog = s"$expr: $msg."

    def show: String = lastLog

    override def toString: String = lastLog
  }

  /**
    * Find all permutations for Stream[ Stream[T] ] = { xs1, xs2, ... }
    * i.e. for {
    * x1 <- xs1
    * x2 <- xs2
    * ...
    * } yield Stream(x1, x2, ...)
    *
    * @param xs
    * @tparam T
    * @return
    */
  def permutation[T](xs: List[Stream[T]]): Stream[List[T]] = xs match {
    case Nil => Stream()
    case x :: Nil => x.map(List(_))
    case x :: y :: Nil => for {
      x1 <- x
      y1 <- y
    } yield List(x1, y1)
    case y :: ys => for {
      x1 <- y
      x2 <- permutation(ys)
    } yield x1 :: x2
  }

  abstract class ProgramSetNode[T <: ProgramNode] {
    def allPrograms: Stream[T]

    def take(n: Int): List[T] = allPrograms.take(n).toList

    def head: T = allPrograms.head
  }

  class StringProgramSet(val e: List[(Bool, TraceExprSet)] = Nil)
    extends ProgramSetNode[StringProgram] {
    lazy val allPrograms: Stream[StringProgram] = {
      val bs = e.map(_._1)
      permutation(e.map(_._2.allPrograms)).map(exprs => new StringProgram(bs.zip(exprs)))
    }

    override def toString: String = e map {
      case (b, t) => s"Case $b => $t"
    } mkString "\n"
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
                     val w: Map[(Node, Node), List[AtomExprSet]])
    extends ProgramSetNode[TraceExpr] {
    lazy val isEmpty: Boolean = size == 0

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
        (x1, x2) <- w.keys
        (y1, y2) <- that.w.keys
        es = for {
          f1 <- w(x1, x2)
          f2 <- that.w(y1, y2)
          e <- f(f1, f2)
        } yield e
        if es.nonEmpty
      } yield (
        (Pair(x1, y1): Node, Pair(x2, y2): Node),
        es)).toMap

      new TraceExprSet(nodes1, Pair(startNode, that.startNode),
        Pair(terminateNode, that.terminateNode), edges1, w1)
    }

    def intersect: TraceExprSet => TraceExprSet = template(_.intersect(_))

    def unify: TraceExprSet => TraceExprSet = template(_.unify(_))

    // assert Node = AtomNode
    def subgraph(from: Int, to: Int): TraceExprSet = {
      val nodes1 = (0 to to - from).map(Atom(_))
      val edges1 = for {
        i <- 0 until to - from
        j <- i + 1 to to - from
      } yield (Atom(i), Atom(j))

      val w1 = w.flatMap {
        case ((Atom(i), Atom(j)), es) if from <= i && j <= to =>
          Some(((Atom(i - from): Node, Atom(j - from): Node), es))
        case _ => None
      }

      new TraceExprSet(nodes1.toList, Atom(0), Atom(to - from), edges1.toList, w1)
    }

    lazy val size: Long = {
      val cache1 = w.mapValues(_.map(_.size).sum)
      val keys = w.keys

      val cache: MutableMap[Node, Long] = MutableMap()
      def ret(k: Node, v: Long): Long = {
        cache(k) = v
        v
      }

      def sz(n: Node): Long = cache.get(n) match {
        case Some(v) => v
        case None =>
          if (n == startNode) ret(n, 1L)
          else ret(n, (for {
            m <- keys.flatMap {
              case (i, j) if j == n => Some(i)
              case _ => None
            }
          } yield sz(m) * cache1(m, n)).sum)
      }
      sz(terminateNode)
    }

    override def toString: String = {
      val s = w map {
        case (edge, exprs) => s"  $edge -> {${exprs.mkString(", ")}}"
      } mkString "\n"
      s"TraceExpr(\n$s\n)"
    }

    def check(expected: BaseNode[String], sigma: BaseNode[InputType], k: Int = 0): Boolean = {
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
          case (i, j, e, input) => exprs.forall(_.check(e.substring(i, j), input, k))
        }
      }
      if (result) true
      else {
        System.err.println(ErrorLog.show)
        false
      }
    }

    lazy val allEdges: List[(Node, Node)] = w.keys.toList

    lazy val allNodes: List[Node] = allEdges.flatMap {
      case (x, y) => x :: y :: Nil
    }

    lazy val succs: Map[Node, List[Node]] = (for {
      x <- allNodes
    } yield (x,
      for {
        (a, b) <- allEdges
        if x == a
      } yield b)).toMap

    def pathsFrom(s: Node): Option[List[List[(Node, Node)]]] = {
      if (s == terminateNode) Some(List(List()))
      else succs.get(s) match {
        case None => None
        case Some(ss) => (for {
          q <- ss
          paths <- pathsFrom(q)
        } yield paths.map((s, q) :: _)).flatten match {
          case Nil => None
          case xs => Some(xs)
        }
      }
    }

    lazy val allPaths: List[List[(Node, Node)]] = pathsFrom(startNode) match {
      case Some(xs) => xs.sortBy(_.length)
      case None => Nil
    }

    lazy val allPrograms: Stream[TraceExpr] = {
      val xs = allPaths.toStream.flatMap {
        path => permutation(path.map {
          case (x, y) => w(x, y).flatMap(_.allPrograms).toStream
        })
      }
      xs.map(new TraceExpr(_))
    }

//      w(startNode, terminateNode).toStream
//      .flatMap(_.allPrograms).map {
//      e => new TraceExpr(e)
//    }
  }

  abstract class AtomExprSet extends ProgramSetNode[AtomExpr] {
    def size: Long

    def intersect: AtomExprSet => Option[AtomExprSet]

    def unify: AtomExprSet => Option[AtomExprSet]

    def toString: String

    def check(expected: String, sigma: InputType, k: Int): Boolean
  }

  case class ConstStrSet(s: String) extends AtomExprSet {
    def intersect: AtomExprSet => Option[AtomExprSet] = {
      case ConstStrSet(s1) => if (s == s1) Some(this) else None
      case _ => None
    }

    def unify = intersect

    val size = 1L

    def eval(sigma: InputType): Set[String] = Set(s)

    override def toString: String = s"""ConstStr("$s")"""

    def check(expected: String, sigma: InputType, k: Int): Boolean = {
      if (s == expected) true
      else {
        ErrorLog.push(toString, s"$expected expected")
        false
      }
    }

    lazy val allPrograms: Stream[AtomExpr] = Stream(ConstStr(s))
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

    override def toString: String = s"SubStr($i, {${p.mkString(", ")}}, {${q.mkString(", ")}})"

    def check(expected: String, sigma: InputType, k: Int): Boolean = {
      val str = sigma(i)
      if (str.indexesOf(expected).exists { index =>
        p.forall(_.check(index, str, k)) && q.forall(_.check(index + expected.length, str, k))
      }) true
      else {
        ErrorLog.push(toString,
          s"""cannot obtain expected string "$expected" from input "${sigma(i)}"""")
        false
      }
    }

    lazy val allPrograms: Stream[AtomExpr] = for {
      x <- p.toStream.flatMap(_.allPrograms)
      y <- q.toStream.flatMap(_.allPrograms)
    } yield SubStr(i, x, y)
  }

  case class LoopSet(e: TraceExprSet) extends AtomExprSet {
    def template(f: (TraceExprSet, TraceExprSet) => TraceExprSet)
                (that: AtomExprSet): Option[AtomExprSet] = that match {
      case LoopSet(e1) => Some(LoopSet(f(e, e1)))
      case _ => None
    }

    def intersect: AtomExprSet => Option[AtomExprSet] = template(_.intersect(_))

    def unify: AtomExprSet => Option[AtomExprSet] = template(_.unify(_))

    def checkLoop(sigma: InputType): Option[String] = {
      def checkR(xs: List[String], w: Int): Boolean = xs match {
        case Nil => true
        case y :: ys =>
          if (check(y, sigma, w)) checkR(ys, w + 1)
          else false
      }

      val Loop(e) = head
      val xs = Loop(e).evalS(sigma)
      println(xs)
      if (checkR(xs, 1)) Some(xs.mkString(""))
      else None
    }

    lazy val size = e.size

    def check(expected: String, sigma: InputType, k: Int): Boolean =
      e.check(Pair(Atom(expected), Atom(expected)), Pair(Atom(sigma), Atom(sigma)), k)

    lazy val allPrograms: Stream[AtomExpr] = for {
      x <- e.allPrograms
    } yield Loop(x)
  }

  abstract class PosExprSet extends ProgramSetNode[Position] {
    def size: Long

    def intersect: PosExprSet => Option[PosExprSet]

    def unify: PosExprSet => Option[PosExprSet]

    def toString: String

    def check(expected: Int, s: String, w: Int): Boolean
  }

  case class CPosSet(k: Int) extends PosExprSet {
    val intersect: PosExprSet => Option[PosExprSet] = {
      case CPosSet(k1) => if (k == k1) Some(this) else None
      case _ => None
    }

    val unify = intersect

    val size = 1L

    override def toString: String = s"CPos($k)"

    def check(expected: Int, s: String, w: Int): Boolean = {
      val expected1 = -(s.length - expected)
      if (k == expected || k == expected1) true
      else {
        ErrorLog.push(toString, s"$expected or $expected1 expected")
        false
      }
    }

    lazy val allPrograms: Stream[Position] = Stream(CPos(k))
  }

  case class PosSet(r1: TokenSeq, r2: TokenSeq, c: IntegerExprSet) extends PosExprSet {
    def template(f: (IntegerExprSet, IntegerExprSet) => Option[IntegerExprSet])
                (that: PosExprSet): Option[PosExprSet] = that match {
      case PosSet(s1, s2, d) =>
        (r1.intersect(s1), r2.intersect(s2), f(c, d)) match {
          case (Some(x1), Some(x2), Some(e)) => Some(PosSet(x1, x2, e))
          case _ => None
        }
      case _ => None
    }

    def intersect: PosExprSet => Option[PosExprSet] = template(_.intersect(_))

    def unify: PosExprSet => Option[PosExprSet] = template(_.unify(_))

    lazy val size = r1.size * r2.size * c.size

    override def toString: String = s"Pos($r1, $r2, $c)"

    def check(expected: Int, s: String, w: Int): Boolean = {
      r1.check(s) && r2.check(s) && {
        val reps1 = r1.head
        val reps2 = r2.head
        c.forall(w) { c1 =>
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

    lazy val allPrograms: Stream[Position] = for {
      x <- r1.allPrograms
      y <- r2.allPrograms
      k <- c.allPrograms
    } yield Pos(x, y, k)
  }

  class IntegerExprSet(val e: Set[IntegerExpr]) extends ProgramSetNode[IntegerExpr] {
    def intersect(that: IntegerExprSet): Option[IntegerExprSet] = {
      val s = e.intersect(that.e)
      if (s.isEmpty) None else Some(new IntegerExprSet(s))
    }

    def unify(that: IntegerExprSet): Option[IntegerExprSet] = {
      val CInt(c1) :: CInt(c2) :: Nil = e.toList
      val CInt(d1) :: CInt(d2) :: Nil = that.e.toList
      val s1: Set[IntegerExpr] = if (c1 != d1) Set(Linear(d1 - c1, c1 * 2 - d1)) else Set()
      val s2: Set[IntegerExpr] = if (c2 != d2) Set(Linear(d2 - c2, c2 * 2 - d2)) else Set()
      val s = s1 ++ s2
      if (s.isEmpty) None else Some(new IntegerExprSet(s))
    }

    lazy val size: Long = e.size

    def forall(w: Int)(p: IntegerExpr => Boolean): Boolean = e.map(_.eval(w)).map(CInt).forall(p)

    override def toString: String = s"{${e.mkString(", ")}}"

    lazy val allPrograms: Stream[IntegerExpr] = e.toStream
  }

  class TokenSeq(val tokenss: List[List[Token]] = Nil) extends ProgramSetNode[RegularExpr] {
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

    lazy val size: Long = tokenss.map(_.size).product

    override def toString: String = {
      if (tokenss.isEmpty) "{ε}"
      else {
        val s = tokenss map {
          case Nil => ""
          case x :: xs => x.toString
        } mkString ""
        s"{$s}"
      }
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

    lazy val allPrograms: Stream[RegularExpr] =
      if (tokenss.isEmpty) Stream(emptyRegex)
      else permutation(tokenss.map(_.toStream)).map {
        xs => new RegularExpr(xs)
      }
  }

}
