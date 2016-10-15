/**
  * Created by paul on 15/10/2016.
  */
object Utils {
  /**
    * Tell whether a list `ts` has the same element.
    * @param ts list
    * @tparam T element type
    * @return whether all elements are the same
    */
  def allSame[T](ts: List[T]): Boolean = ts match {
    case Nil => true
    case x :: Nil => true
    case x :: y :: xs => if (x == y) allSame(y :: xs) else false
  }

  /**
    * Find the i-th element of list `xs`.
    * @param xs list
    * @param i element index, start from 0, can be negative (count from the tail of the list)
    * @tparam T element type
    * @return the i-th element (may be None)
    */
  def nth[T](xs: List[T], i: Int): Option[T] = {
    // i > 0
    def ith[U](xs: List[U], i: Int): Option[U] = xs match {
      case Nil => None
      case y :: ys => if (i == 1) Some(y) else ith(ys, i - 1)
    }

    if (i > 0) ith(xs, i)
    else ith(xs.reverse, -i)
  }
}
