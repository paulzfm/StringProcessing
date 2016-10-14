/**
  * Created by paul on 9/29/16.
  */
object StringUtils {

  implicit class StringUtilsClass(val s: String) {
    def subRange(left: Option[Int], right: Option[Int]): Option[String] = (left, right) match {
      case (Some(l), Some(r)) =>
        if (l < 0 || l >= s.length || r > s.length) None
        else Some(s.substring(l, r))
      case _ => None
    }

    def indexesOf(pattern: String) : List[Int] = {
      def helper(start: Int, acc: List[Int]): List[Int] = {
        val index = s.indexOf(pattern, start)
        if (index == -1) acc
        else helper(index + 1, acc :+ index)
      }
      helper(0, Nil)
    }

    def indexesWhere(p: Char => Boolean):List[Int] = {
      def helper(start: Int, acc: List[Int]): List[Int] = {
        val index = s.indexWhere(p, start)
        if (index == -1) acc
        else helper(index + 1, acc :+ index)
      }
      helper(0, Nil)
    }
  }

}
