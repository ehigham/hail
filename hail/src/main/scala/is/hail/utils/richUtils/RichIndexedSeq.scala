package is.hail.utils.richUtils

import scala.reflect.ClassTag

/** Rich wrapper for an indexed sequence.
  *
  * Houses the generic binary search methods. All methods taking
  *   - a search key 'x: U',
  *   - a key comparison 'lt: (U, U) => Boolean' (the most generic versions
  *   allow the search key 'x' to be of a different type than the elements of
  *   the sequence, and take one or two mixed type comparison functions),
  *   - and a key projection 'k: (T) => U',
  * assume the following preconditions for all 0 <= i <= j < a.size (writing <
  * for 'lt'):
  *   1. if 'x' < k(a(i)) then 'x' < k(a(j))
  *   2. if k(a(j)) < 'x' then k(a(i)) < 'x'
  * These can be rephrased as 1: 'x' < k(_) partitions a, and 2: k(_) < 'x'
  * partitions a. (Actually, upperBound only needs 1. and lowerBound only needs
  * 2.)
  */
final class RichIndexedSeq[T](override val a: IndexedSeq[T]) extends RichFastSeq[T, IndexedSeq] {
  override implicit val T: FastSeqOps[IndexedSeq] =
    new FastSeqOps[IndexedSeq] {
      override def length(r: IndexedSeq[_]): Int = r.length

      override def at[A](r: IndexedSeq[A], idx: Int): A = r(idx)

      override def allocate[A: ClassTag](size: Int): IndexedSeq[A] = new Array[A](size)

      override def assign[A](r: IndexedSeq[A], i: Int, value: A): Unit = r.updated(i, a)
    }
}
