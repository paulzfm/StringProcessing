/**
  * Created by paul on 9/29/16.
  */

import java.lang.Math.max

import StringUtils.StringUtilsClass
import Utils._

object Program {
  def debug[T](x: T): T = {
    println(x)
    x
  }

  type InputType = Vector[String]

  def indents(indent: Int = 0): String = "  " * indent

  trait ProgramTopNode {
    def eval(sigma: InputType, w: Int = 0): Option[String]

    def code(indent: Int = 0): String
  }

  abstract class ProgramNode

  class StringProgram(switches: List[(Bool, TraceExpr)]) extends ProgramNode with ProgramTopNode {
    def eval(sigma: InputType, w: Int): Option[String] = {
      def evalR(xs: List[(Bool, TraceExpr)]): Option[String] = xs match {
        case Nil => None
        case (b, e) :: ys =>
          if (b.eval(sigma)) e.eval(sigma, 0)
          else evalR(ys)
      }
      evalR(switches)
    }

    def code(indent: Int): String = {
      val blanks = indents(indent)
      switches.flatMap(_ match {
        case (b, e) => List(
          blanks + "Case (",
          b.code(indent + 1),
          blanks + ") =>",
          e.code(indent + 1),
          blanks + "}"
        )
      }).mkString("\n")
    }
  }

  class Bool(conjunctions: List[Conjunction] = Nil) {
    def this(predicate: Predicate) = this(List(new Conjunction(predicate)))

    def eval(sigma: InputType): Boolean = {
      def evalR(xs: List[Conjunction]): Boolean = xs match {
        case Nil => false
        case b :: bs => if (b.eval(sigma)) true else evalR(bs)
      }
      evalR(conjunctions)
    }

    def code(indent: Int = 0): String = {
      val blanks = indents(indent)
      conjunctions.map(blanks + "(" + _.code + ")").mkString(" or\n")
    }

    override def toString: String = s"Or(${conjunctions.mkString(", ")})"
  }

  class Conjunction(predicates: List[Predicate]) {
    def this(predicate: Predicate) = this(List(predicate))

    def eval(sigma: InputType): Boolean = {
      def evalR(xs: List[Predicate]): Boolean = xs match {
        case Nil => true
        case b :: bs => if (!b.eval(sigma)) false else evalR(bs)
      }
      evalR(predicates)
    }

    lazy val code = predicates.map(_.code).mkString(" and ")

    override def toString: String = s"And(${predicates.mkString(", ")})"
  }

  abstract class Predicate {
    def eval(sigma: InputType): Boolean

    def code: String

    override def toString: String = code

    def negate: Predicate
  }

  case class Match(i: Int, regex: RegularExpr, k: Int = 1) extends Predicate {
    def eval(sigma: InputType): Boolean = regex.findMatchesIn(sigma(i)).length >= k

    lazy val code = s"Match($i, ${regex.code}, $k)"

    lazy val negate = NotMatch(i, regex, k)
  }

  case class NotMatch(i: Int, regex: RegularExpr, k: Int = 1) extends Predicate {
    def eval(sigma: InputType): Boolean = regex.findMatchesIn(sigma(i)).length < k

    lazy val code = s"not Match($i, ${regex.code}, $k)"

    lazy val negate = Match(i, regex, k)
  }

  class TraceExpr(traces: List[AtomExpr] = Nil) extends ProgramNode with ProgramTopNode {
    def this(e: AtomExpr) = this(List(e))

    def eval(sigma: InputType, w: Int = 0): Option[String] = {
      def concatenate(xs: List[Option[String]], acc: String): Option[String] = xs match {
        case Nil => Some(acc)
        case Some(y) :: ys => concatenate(ys, acc + y)
        case None :: _ => None
      }
      concatenate(traces.map(_.eval(sigma, w)), "")
    }

    def code(indent: Int = 0): String = {
      val blanks = indents(indent)
      List(
        blanks + "Concatenate(",
        traces.map(_.code(indent + 1)).mkString(",\n"),
        blanks + ")"
      ).mkString("\n")
    }
  }

  val emptyString = new TraceExpr(Nil)

  abstract class AtomExpr extends ProgramNode with ProgramTopNode {
    def eval(sigma: InputType, w: Int = 0): Option[String]

    def code(indent: Int = 0): String
  }

  case class SubStr(i: Int, p1: Position, p2: Position) extends AtomExpr {
    def eval(sigma: InputType, w: Int): Option[String] = {
      val s = sigma(i)
      s.subRange(p1.eval(s, w), p2.eval(s, w))
    }

    def code(indent: Int): String = {
      indents(indent) + s"SubStr($i, ${p1.code}, ${p2.code})"
    }
  }

  def SubStr2(i: Int, r: RegularExpr, c: IntegerExpr) =
    SubStr(i, Pos(emptyRegex, r, c), Pos(r, emptyRegex, c))

  def InputStr(i: Int) = SubStr(i, CPos(0), CPos(-1))

  case class ConstStr(str: String) extends AtomExpr {
    def eval(sigma: InputType, w: Int): Option[String] = Some(str)

    def code(indent: Int): String = {
      indents(indent) + s"""ConstStr("$str")"""
    }
  }

  case class Loop(expr: TraceExpr) extends AtomExpr {
    def eval(sigma: InputType, w: Int): Option[String] = {
      def evalR(k: Int, acc: String): String = {
        expr.eval(sigma, k) match {
          case Some(s) => evalR(k + 1, acc + s)
          case None => acc
        }
      }
      Some(evalR(1, ""))
    }

    def code(indent: Int): String = {
      val blanks = indents(indent)
      List(
        blanks + "Loop(w =>",
        expr.code(indent + 1),
        blanks + ")"
      ).mkString("\n")
    }
  }

  abstract class Position extends ProgramNode {
    def eval(s: String, w: Int = 0): Option[Int]

    def code: String
  }

  case class CPos(k: Int) extends Position {
    def eval(s: String, w: Int): Option[Int] = Some(
      if (k >= 0) k
      else 1 + k + s.length
    )

    lazy val code = s"CPos($k)"
  }

  case class Pos(r1: RegularExpr, r2: RegularExpr, c: IntegerExpr) extends Position {
    def eval(s: String, w: Int): Option[Int] = nth(evalAll(s), c.eval(w))

    def evalAll(s: String): List[Int] = {
      for {
        (t1, t2) <- r1.findMatchesIn(s) // t1 <= t2 or t2 = t1 - 1
        (t3, t4) <- r2.findMatchesIn(s) // t3 <= t4 or t4 = t3 - 1
        if t2 + 1 == t3 // t = t3
      } yield t3
    }

    lazy val code = s"Pos(${r1.code}, ${r2.code}, ${c.code})"
  }

  abstract class IntegerExpr extends ProgramNode {
    def eval(w: Int = 0): Int

    def code: String

    override def toString: String = code
  }

  case class CInt(k: Int) extends IntegerExpr {
    def eval(w: Int): Int = k

    lazy val code = s"$k"
  }

  case class Linear(k1: Int, k2: Int = 0) extends IntegerExpr {
    def eval(w: Int): Int = k1 * w + k2

    lazy val code = s"$k1 * w + $k2"
  }

  /**
    * If one or more characters are matched, pair (i, j) means index range [i, j].
    * (0 <= i <= j <= s - 1 where s is the string length)
    * If zero character is matched, we use pair (i, i - 1) to mean the location exactly before i.
    * (0 <= i <= s where s is the string length)
    */
  type MatchPair = (Int, Int)

  class RegularExpr(val tokens: List[Token] = Nil) extends ProgramNode {
    def this(token: Token) = this(List(token))

    def concat(that: RegularExpr): RegularExpr = new RegularExpr(tokens ++ that.tokens)

    lazy val reverse = new RegularExpr(tokens.reverse)

    lazy val size = tokens.size

    def prepend(token: Token): RegularExpr = new RegularExpr(token :: tokens)

    /**
      * To find all matches of string `s`.
      *
      * @param s the string to be matched
      * @return a list of MatchPair showing all matches
      */
    def findMatchesIn(s: String): List[MatchPair] = {
      def helper(from: Int, acc: List[MatchPair]): List[MatchPair] = s.length match {
        case l if from > l => acc
        // special example: RegularExpr(List(EndToken))
        case l if from == l && tokens == List(EndToken) => acc :+ (from, from - 1)
        case _ => matchPrefixOf(s, from) match {
          // use `max` to look up StartToken ONLY ONCE
          case Some(end) => helper(max(end, 0) + 1, acc :+ (from, end))
          case None => helper(from + 1, acc)
        }
      }

      // special example: RegularExpr(Nil)
      if (tokens.isEmpty) (for {
        i <- 0 to s.length
      } yield (i, i - 1)).toList // all match pairs that contains zero character
      else helper(0, Nil)
    }

    /**
      * To match some prefix of string `s.substring(from)`.
      *
      * @param s    string to be matched
      * @param from starting index [0, `s.length`)
      * @return Some(end) - when matched, the MatchPair should be (from, end)
      *         None      - when not matched
      */
    def matchPrefixOf(s: String, from: Int): Option[Int] = {
      def helper(start: Int, tokens: List[Token]): Option[Int] = tokens match {
        case Nil => Some(start - 1)
        case EndToken :: Nil if start == s.length => Some(start - 1)
        case t :: ts =>
          if (start >= s.length) None
          else {
            t.matchPrefixOf(s, start) match {
              case Some(end) => helper(end + 1, ts)
              case None => None
            }
          }
      }
      helper(from, tokens)
    }

    override def toString: String = {
      if (tokens.isEmpty) "ε"
      else tokens.mkString("")
    }

    lazy val code = toString
  }

  val emptyRegex = new RegularExpr(Nil)

  abstract class Token {
    def findMatchesIn(s: String): List[MatchPair] = new RegularExpr(this).findMatchesIn(s)

    def matchPrefixOf(s: String, from: Int): Option[Int]

    def toString: String
  }

  // C+
  case class OneOrMore(char: CharacterClass) extends Token {
    def matchPrefixOf(s: String, from: Int): Option[Int] = {
      s.substring(from).takeWhile(char.contains).length match {
        case 0 => None // match 0 char
        case l => Some(from + l - 1) // match l chars
      }
    }

    override def toString: String = s"[$char]+"
  }

  // [^C]+
  case class ExcludeOneOrMore(char: CharacterClass) extends Token {
    def matchPrefixOf(s: String, from: Int): Option[Int] = {
      s.substring(from).takeWhile(!char.contains(_)).length match {
        case 0 => None // match 0 char
        case l => Some(from + l - 1) // match l chars
      }
    }

    override def toString: String = s"[^$char]+"
  }

  // ^
  case object StartToken extends Token {
    def matchPrefixOf(s: String, from: Int): Option[Int] = {
      if (from == 0) Some(-1)
      else None
    }

    override def toString: String = "^"
  }

  // $
  case object EndToken extends Token {
    def matchPrefixOf(s: String, from: Int): Option[Int] = None

    override def toString: String = "$"
  }

  // `+`, `.`, `-`, etc. NOTE: should NOT be any numeric/letters/whitespaces
  case class SpecialToken(char: Char) extends Token {
    def matchPrefixOf(s: String, from: Int): Option[Int] = {
      if (s(from) == char) Some(from)
      else None
    }

    override def toString: String = s"$char"
  }

  case class ExcludeSpecialToken(char: Char) extends Token {
    def matchPrefixOf(s: String, from: Int): Option[Int] = {
      if (s(from) != char) Some(from)
      else None
    }

    override def toString: String = s"[^$char]"
  }

  abstract class CharacterClass {
    def contains(char: Char): Boolean

    def toString: String
  }

  // 0-9
  case object Numeric extends CharacterClass {
    def contains(char: Char): Boolean = '0' <= char && char <= '9'

    override def toString: String = "0-9"
  }

  // A-Z
  case object UpperCases extends CharacterClass {
    def contains(char: Char): Boolean = 'A' <= char && char <= 'Z'

    override def toString: String = "A-Z"
  }

  // a-z
  case object LowerCases extends CharacterClass {
    def contains(char: Char): Boolean = 'a' <= char && char <= 'z'

    override def toString: String = "a-z"
  }

  // UpperCases or LowerCases
  case object Letters extends CharacterClass {
    def contains(char: Char): Boolean = UpperCases.contains(char) || LowerCases.contains(char)

    override def toString: String = "A-Za-z"
  }

  // Numeric or Letters
  case object NumericWithLetters extends CharacterClass {
    def contains(char: Char): Boolean = Numeric.contains(char) || Letters.contains(char)

    override def toString: String = "0-9A-Za-z"
  }

  // \w
  case object Whitespaces extends CharacterClass {
    def contains(char: Char): Boolean = Set(' ', '\t', '\n', '\r').contains(char)

    override def toString: String = " "
  }

  // any character
  case object All extends CharacterClass {
    def contains(char: Char): Boolean = true

    override def toString: String = "ANY"
  }

  val specialChars: List[Char] = List('+', '-', '*', '/', '.', '\\', '(', ')', ',')
  val spaceChars: List[Char] = List(' ')
  val characterSets: List[CharacterClass] = List(Numeric, UpperCases, LowerCases, Letters,
    NumericWithLetters, Whitespaces)
  val allTokens: List[Token] =
    characterSets.map(OneOrMore) ++ characterSets.map(ExcludeOneOrMore) ++
      specialChars.map(SpecialToken) // ++ specialChars.map(ExcludeSpecialToken)

  type TokenPartition = Map[Token, (List[Token], List[MatchPair])]

  def tokensPartitionOn(s: String): TokenPartition = {
    allTokens.groupBy(_.findMatchesIn(s)).map {
      case (ms, ts) =>
        val sorted = ts.sortBy {
          case SpecialToken(_) => 1
          case OneOrMore(Whitespaces) => 2
          case OneOrMore(Numeric) => 2
          case OneOrMore(UpperCases) => 2
          case OneOrMore(LowerCases) => 2
          case OneOrMore(Letters) => 3
          case OneOrMore(NumericWithLetters) => 4
          case _ => 5
        }
        (sorted.head, (sorted, ms))
    }
  }
}
