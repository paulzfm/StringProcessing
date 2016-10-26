/**
  * Created by paul on 9/26/16.
  */

import java.lang.Math.max

import Program._
import ProgramSet._
import StringUtils.StringUtilsClass

import scala.collection.mutable.{Map => MutableMap}

class Learner(val examples: List[(InputType, String)], val check: Boolean = false) {
  private lazy val program: StringProgramSet = {
    val partition = genPartition(examples map {
      case (i, o) =>
        val exprs = genTraceExpr(i, o)
        if (check) exprs.check(Atom(o), Atom(i))
        (List(i), exprs)
    } toVector)

    println(s"${partition.size} partitions")

    def exclude(k: Int): List[InputType] = (for {
      i <- partition.indices
      if i != k
    } yield partition(i)._1).flatten.toList

    partition.size match {
      case 0 => new StringProgramSet
      case 1 => new StringProgramSet(List((new Bool, partition(0)._2)))
      case n => new StringProgramSet((for {
        i <- 0 until n
        b <- genBoolClassifier(partition(i)._1, exclude(i))
      } yield (b, partition(i)._2)).toList)
    }
  }

  def learn: StringProgramSet = program

  type Partition = Vector[(List[InputType], TraceExprSet)]

  def genPartition(partition: Partition): Partition = {
    val comps = (for {
      i <- 0 until (partition.size - 1)
      j <- (i + 1) until partition.size
      e = partition(i)._2.intersect(partition(j)._2)
      if e.nonEmpty // Comp(i, j) = true, i < j
    } yield ((i, j), e)).toMap

    def comp(i: Int, j: Int): Boolean = comps.get((i, j)) match {
      case Some(_) => true
      case None => false
    }

    class Score(val cs1: Int, val cs2: Double) extends Ordered[Score] {
      override def compare(that: Score): Int = {
        val b = (cs1 > that.cs1) || (cs1 == that.cs1 && cs2 > that.cs2)
        if (b) 1 else -1
      }
    }

    val scores = comps map {
      case ((i, j), e) =>
        val cs1 = (for {
          k <- partition.indices
          if k != i && k != j
          if comp(i, k) == comp(j, k) && comp(j, k) == e.intersect(partition(k)._2).nonEmpty
        } yield 1).sum
        val cs2 = e.size.toDouble / max(partition(i)._2.size, partition(j)._2.size)
        ((i, j), new Score(cs1, cs2))
    }

    if (comps.isEmpty) partition
    else {
      val ((i, j), _) = scores.maxBy(_._2)
      val e = comps((i, j))
      val p = for {
        k <- partition.indices
        if k != i && k != j
      } yield partition(k)
      genPartition(p.toVector :+ (partition(i)._1 ++ partition(j)._1, e))
    }
  }

  def genBoolClassifier(sigma1: List[InputType], sigma2: List[InputType]): Option[Bool] = {
    // We ONLY consider `Match` predicate and use ONLY the represent tokens to generate regular
    // expressions.
    def genPredicates(sigma: InputType): List[Predicate] = (for {
      i <- sigma.indices
    } yield {
      val s = sigma(i)
      val tokens = tokensPartitionOn(s).keys.toList
      for {
        j <- s.indices
        (_, r) <- findRegularExprMatchPrefixOf(s.substring(j), tokens)
      } yield {
        val k = r.findMatchesIn(s).size
        Match(i, r, k)
      }
    }).flatten.toList

    // When score all predicates, we pick the shortest (with as least tokens as possible) from
    // those who have the highest csp.
    val preds: List[Predicate] =
    (sigma1.flatMap(genPredicates) ++ sigma2.flatMap(genPredicates)).distinct.sortBy {
      case Match(_, r, _) => r.size
      case NotMatch(_, r, _) => r.size
    }

    def csp(p: Predicate, s1: Set[InputType], s2: Set[InputType]): Int =
      s1.count(p.eval) * s2.count(!p.eval(_))

    def genConjunction(s1: Set[InputType], s2: Set[InputType], acc: List[Predicate]):
    Option[(Conjunction, Set[InputType], Set[InputType])] = {
      if (s2.isEmpty) Some((new Conjunction(acc), s1, s2))
      else {
        val p = preds.maxBy(csp(_, s1, s2))
        val sig2 = s2.filter(p.eval)
        if (sig2.size == s2.size) None
        else genConjunction(s1.filter(p.eval), sig2, p :: acc)
      }
    }

    def genBool(s1: Set[InputType], s2: Set[InputType], acc: List[Conjunction]):
    Option[Bool] = {
      if (s1.isEmpty) Some(new Bool(acc))
      else genConjunction(s1, s2, Nil) match {
        case Some((con, sig1, sig2)) =>
          if (sig1.isEmpty) None
          else genBool(s1 -- sig1, s2, con :: acc)
        case None => None
      }
    }

    genBool(sigma1.toSet, sigma2.toSet, Nil)
  }

  def genTraceExpr(sigma: InputType, s: String): TraceExprSet = {
    println(s"genTraceExpr($sigma, $s)")
    val edges = for {
      i <- 0 until s.length
      j <- (i + 1) to s.length
    } yield (i, j)

    val w = edges.map {
      case (i, j) =>
        ((i, j),
          genSubStr(sigma, s.substring(i, j)) :+ genConstStr(s.substring(i, j)))
    }.toMap

    new TraceExprSet((0 to s.length).map(Atom[Int]).toList, Atom(0), Atom(s.length),
      edges map {
        case (x, y) => (Atom(x), Atom(y))
      } toList, w.map {
        case ((x, y), e) => ((Atom(x), Atom(y)), e)
      })
    //    new TraceExprSet((0 to s.length).toList, 0, s.length, edges.toList, genLoop(sigma, s, w))
  }

  def genSubStr(sigma: InputType, s: String): List[AtomExprSet] = {
    val xs = for {
      i <- sigma.indices
      k <- sigma(i).indexesOf(s)
    } yield {
      val y1 = genPos(sigma(i), k) ++ genCPos(sigma(i), k)
      val y2 = genPos(sigma(i), k + s.length) ++ genCPos(sigma(i), k + s.length)
      SubStrSet(i, y1, y2)
    }
    xs.toList
  }

  def genConstStr(s: String): AtomExprSet = ConstStrSet(s)

  /*
  def genLoop(sigma: Vector[String], s: String, t: TraceExprSet): Map[(Node, Node), List[AtomExprSet]] = {
    val w1 = MutableMap() ++ t.w

    for {
      k1 <- 0 until s.length
      k2 <- k1 until s.length
      k3 <- 1 + k2 until s.length
      e1 = t.subgraph(k1, k2 + 1)
      e2 = t.subgraph(k2 + 1, k3 + 1)
      e = LoopSet(e1.unify(e2))
      output <- e.checkLoop(sigma)
    } yield {
      println("!!!!")
      val k4 = k1 + output.length - 1
      w1((Atom(k1), Atom(k4))) = e :: w1((Atom(k1), Atom(k4)))
    }

    w1.toMap
  }
  */

  def genCPos(s: String, k: Int): List[PosExprSet] = List(
    CPosSet(k),
    CPosSet(-(s.length - k))
  )

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
    (-1, emptyRegex) :: matches(0)
  }

  def genPos(s: String, k: Int): List[PosExprSet] = {
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
      if r1.nonEmpty || r2.nonEmpty // else, another CPos expression is equivalent
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
