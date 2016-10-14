/**
  * Created by paul on 9/26/16.
  */

import Program._
import ProgramSet._
import StringUtils.StringUtilsClass
import scala.collection.mutable.{Map => MutableMap}

class Learner(val examples: Vector[(Vector[String], String)]) {
  def generateStringProgram = {
    ???
  }

  type Partition = Vector[(Vector[String], TraceExprSet)]

  def generatePartition(t: Partition): Partition = {
    class Score(val s1: Int, val s2: Double) {
      def <(that: Score): Boolean = (s1 > that.s1) || (s1 == that.s1 && s2 > that.s2)
    }

    def loop(t: Partition): Partition = ???
    //      val sizes = for {
    //        i <- t.indices
    //      } yield size(t(i)._2)
    //      val intersects = (for {
    //        i <- 0 until (t.size - 1)
    //        j <- (i + 1) until t.size
    //      } yield ((i, j), intersect(t(i)._2, t(j)._2))).toMap
    //      val comps = intersects.mapValues(x => true)
    //
    //      if (!comps.exists(_._2)) t
    //      else {
    //        val ((p, q), _) = (for {
    //          i <- 0 until (t.size - 1)
    //          j <- (i + 1) until t.size
    //        } yield {
    //          val cs1 = (for {
    //            k <- t.indices
    //            if k != i && k != j
    //            if comps((i, k)) == comps((j, k)) &&
    //              comps((j, k)) == compatible(intersects((i, j)), t(k)._2)
    //          } yield 1).sum
    //          val cs2 = size(intersects((i, j))) / max(sizes(i), sizes(j))
    //          ((i, j), new Score(cs1, cs2))
    //        }).maxBy(_._2)
    //
    //        val t1 = (for {
    //          i <- t.indices
    //          if i != p && i != q
    //        } yield t(i)).toVector ++ Vector((t(p)._1 ++ t(q)._1, intersects((p, q))))
    //        loop(t1)
    //      }
    //    }

    loop(t)
  }

  def generateBoolClassifier = ???

  def genTraceExpr(sigma: InputType, s: String): TraceExprSet = {
    println(s"genTraceExpr($sigma, $s)")
    val edges = for {
      i <- 0 until s.length
      j <- (i + 1) to s.length
    } yield (i, j)

    val w = edges.map {
      case (i, j) =>
        ((i, j),
          genConstStr(s.substring(i, j)) :: genSubStr(sigma, s.substring(i, j)))
    }.toMap

    new TraceExprSet((0 to s.length).toList, 0, s.length, edges.toList, w)
    //    new TraceExprSet((0 to s.length).toList, 0, s.length, edges.toList, genLoop(sigma, s, w))
  }

  def genSubStr(sigma: InputType, s: String): List[AtomExprSet] = {
    println(s"genSubStr($sigma, $s)")
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
    println(s"genPos($s, $k)")

    def findRegularExprMatchPrefixOf(s: String, tokens: List[Token]): List[(Int, RegularExpr)] = {
      val cache: MutableMap[Int, List[(Int, RegularExpr)]] = MutableMap()
      def ret(i: Int, r: List[(Int, RegularExpr)]): List[(Int, RegularExpr)] = {
        cache(i) = r
        r
      }

      def matches(i: Int): List[(Int, RegularExpr)] = {
        cache.get(i) match {
          case Some(r) => r
          case None => ???
//            if (str.outOfRange(i)) ret(i, Nil)
//            else
//              ret(i, (for {
//                token <- tokens
//                j <- token.matchPrefixOf(str, i)
//                index = i + j
//              } yield (index, new RegularExpr(token)) :: matches(index + 1).map {
//                case (index1, r) => (index1, r.prepend(token))
//              }).flatten)
        }
      }
      matches(0)
    }

    def genTokenSeq(regex: RegularExpr, partition: TokenPartition): TokenSeq =
      new TokenSeq(regex.tokens.map(partition(_)._1))

    val s1 = s.substring(0, k).reverse
    val s2 = s.substring(k)
    val ts1 = tokensPartitionOn(s1)
    val ts2 = tokensPartitionOn(s2)

    val rs1 = findRegularExprMatchPrefixOf(s1, ts1.keys.toList).map {
      case (index, re) => (index, re.reverse)
    }
    val rs2 = findRegularExprMatchPrefixOf(s2, ts2.keys.toList)

    println(s"ts1=${ts1.keys.toList}")
    println(s"ts2=${ts2.keys.toList}")
    println(s"rs1=$rs1")
    println(s"rs2=$rs2")

    for {
      (a, r1) <- rs1
      (b, r2) <- rs2
    } yield {
      val k1 = k - a - 1
      val k2 = k + b
      val ms = r1.concat(r2).findMatchesIn(s)
      PosSet(genTokenSeq(r1, ts1), genTokenSeq(r2, ts2),
        genIntegerExpr(ms.indexOf((k1, k2)), ms.size))
    }
  }

  def genIntegerExpr(c: Int, c1: Int): IntegerExprSet = new IntegerExprSet(Set(
    CInt(c),
    CInt(-(c1 - c + 1))
  ))
}
