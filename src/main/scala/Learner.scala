/**
  * Created by paul on 9/26/16.
  */

import Program._
import ProgramSet._
import StringUtils.StringUtilsClass

import scala.collection.mutable.{Map => MutableMap}

class Learner(val examples: List[(Vector[String], String)]) {
  def genStringProgram = {
    examples.map {
      case (i, o) => (i, genTraceExpr(i, o))
    }
  }

  type Partition = List[(InputType, TraceExprSet[Node])]

  def genPartition(t: Partition): Partition = ???

  def generateBoolClassifier = ???

  def genTraceExpr(sigma: InputType, s: String): TraceExprSet[Atom[Int]] = {
    //    println(s"genTraceExpr($sigma, $s)")
    val edges = for {
      i <- 0 until s.length
      j <- (i + 1) to s.length
    } yield (i, j)

    val w = edges.map {
      case (i, j) =>
        ((i, j),
          genConstStr(s.substring(i, j)) :: genSubStr(sigma, s.substring(i, j)))
    }.toMap

    new TraceExprSet[Atom[Int]]((0 to s.length).map(Atom[Int]).toList, Atom(0), Atom(s.length),
      edges map {
        case (x, y) => (Atom(x), Atom(y))
      } toList, w.map {
        case ((x, y), e) => ((Atom(x), Atom(y)), e)
      })
    //    new TraceExprSet((0 to s.length).toList, 0, s.length, edges.toList, genLoop(sigma, s, w))
  }

  def genSubStr(sigma: InputType, s: String): List[AtomExprSet] = {
    //    println(s"genSubStr($sigma, $s)")
    val xs = for {
      i <- sigma.indices
      k <- sigma(i).indexesOf(s)
    } yield {
      val y1 = genCPos(sigma(i), k) ++ genPos(sigma(i), k)
      val y2 = genCPos(sigma(i), k + s.length) ++ genPos(sigma(i), k + s.length)
      SubStrSet(i, y1, y2)
    }
    xs.toList
  }

  def genConstStr(s: String): AtomExprSet = ConstStrSet(s)

  type Mapping = Map[(Int, Int), List[AtomExprSet]]

  def genLoop(sigma: Vector[String], s: String, w: Mapping): Mapping = {
    val loops = (for {
      k1 <- 0 until s.length
      k2 <- k1 until s.length
      k3 <- k2 until s.length
      e1 = genTraceExpr(sigma, s.substring(k1, k2 + 1))
      e2 = genTraceExpr(sigma, s.substring(k2, k3 + 1))
      e = e1.unify(e2)
      outputs = LoopSet(e).eval(sigma)
      if outputs.size == 1 && s.indexesOf(outputs.head).contains(k1)
    } yield {
      val k4 = k1 + outputs.head.length - 1
      ((k1, k4), LoopSet(e))
    }).toList.groupBy(_._1)

    w.map {
      case (k, v) => loops.get(k) match {
        case Some(xs) => (k, w(k) ++ xs.map {
          case (_, ys) => ys
        })
        case None => (k, v)
      }
    }
  }

  def genCPos(s: String, k: Int): List[PosExprSet] = List(
    CPosSet(k),
    CPosSet(-(s.length - k))
  )

  def genPos(s: String, k: Int): List[PosExprSet] = {
    def findRegularExprMatchPrefixOf(s: String, tokens: List[Token]): List[(Int, RegularExpr)] = {
      val cache: MutableMap[Int, List[(Int, RegularExpr)]] = MutableMap()
      def ret(i: Int, r: List[(Int, RegularExpr)]): List[(Int, RegularExpr)] = {
        cache(i) = r
        r
      }

      def matches(i: Int): List[(Int, RegularExpr)] = {
        cache.get(i) match {
          case Some(r) => r
          case None =>
            if (i >= s.length) ret(i, Nil)
            else
              ret(i, (for {
                token <- tokens
                index <- token.matchPrefixOf(s, i)
              } yield (index, new RegularExpr(token)) :: matches(index + 1).map {
                case (index1, r) => (index1, r.prepend(token))
              }).flatten)
        }
      }
      matches(0)
    }

    def genTokenSeq(regex: RegularExpr, partition: TokenPartition): TokenSeq =
      new TokenSeq(regex.tokens.map(partition(_)._1))

    val partition = tokensPartitionOn(s)
    val tokens = partition.keys.toList
    val rs1 = findRegularExprMatchPrefixOf(s.substring(0, k).reverse, tokens) map {
      case (index, regex) => (k - 1 - index, regex.reverse)
    } filter {
      case (index, regex) => regex.findMatchesIn(s).contains((index, k - 1))
    }
    val rs2 = findRegularExprMatchPrefixOf(s.substring(k), tokens) map {
      case (index, regex) => (k + index, regex)
    } filter {
      case (index, regex) => regex.findMatchesIn(s).contains((k, index))
    }

    for {
      (k1, r1) <- rs1
      (k2, r2) <- rs2
      ms = Pos(r1, r2, CInt(0)).evalAll(s)
      c = ms.indexOf(k)
      if c != -1
    } yield PosSet(genTokenSeq(r1, partition), genTokenSeq(r2, partition),
      genIntegerExpr(1 + c, ms.size))
  }

  def genIntegerExpr(c: Int, c1: Int): IntegerExprSet = new IntegerExprSet(Set(
    CInt(c),
    CInt(-(c1 - c + 1))
  ))
}
