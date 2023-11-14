package is.hail.utils.richUtils

import scala.language.higherKinds
import scala.reflect.ClassTag

trait RichIndexableOps[Repr[_]] {
  @inline def length(r: Repr[_]): Int
  @inline def at[A](r: Repr[A], idx: Int): A
  @inline def allocate[A: ClassTag](size: Int): Repr[A]
  @inline def assign[A](r: Repr[A], i: Int, a: A): Unit
  @inline def tabulate[A: ClassTag](size: Int)(f: Int => A): Repr[A] = {
    val r = allocate[A](size)
    var i = 0
    while (i < size) {
      assign(r, i, f(i))
      i = i + 1
    }
    r
  }

  @inline def fill[A: ClassTag](size: Int)(a: A): Repr[A] = {
    val r = allocate[A](size)
    var i = 0
    while (i < size) {
      assign(r, i, a)
      i = i + 1
    }
    r
  }
}

trait RichIndexable[A, Repr[_]] {
  def T: RichIndexableOps[Repr]

  def a: Repr[A]

  /** Returns 'start' <= i <= 'end' such that
   *   - a(i) < 'x' for all i in ['start', i), and
   *   - !(a(i) < 'x') (i.e. 'x' <= a(i)) for all i in [i, 'end')
   */
  def lowerBound[U, V](x: V, start: Int, end: Int, lt: (U, V) => Boolean, k: A => U): Int =
    partitionPoint[U](!lt(_: U, x), start, end, k)

  def lowerBound[U >: A, V](x: V, start: Int, end: Int, lt: (U, V) => Boolean): Int =
    lowerBound(x, start, end, lt, identity[U])

  def lowerBound[U, V](x: V, lt: (U, V) => Boolean, k: A => U): Int =
    lowerBound(x, 0, T.length(a), lt, k)

  def lowerBound[U >: A, V](x: V, lt: (U, V) => Boolean): Int =
    lowerBound(x, 0, T.length(a), lt)

  /** Returns i in ['start', 'end'] such that
   *   - !('x' < a(i)) (i.e. a(i) <= 'x') for all i in ['start', i), and
   *   - 'x' < a(i) for all i in [i, 'end')
   */
  def upperBound[U, V](x: V, start: Int, end: Int, lt: (V, U) => Boolean, k: A => U): Int =
    partitionPoint[U](lt(x, _: U), start, end, k)

  def upperBound[U >: A, V](x: V, start: Int, end: Int, lt: (V, U) => Boolean): Int =
    upperBound(x, start, end, lt, identity[U])

  def upperBound[U, V](x: V, lt: (V, U) => Boolean, k: A => U): Int =
    upperBound(x, 0, T.length(a), lt, k)

  def upperBound[U >: A, V](x: V, lt: (V, U) => Boolean): Int =
    upperBound(x, 0, T.length(a), lt)

  /** Returns (l, u) such that
   *   - a(i) < 'x' for all i in [0, l),
   *   - !(a(i) < 'x') && !('x' < a(i)) (i.e. a(i) == 'x') for all i in [l, u),
   *   - 'x' < a(i) for all i in [u, a.size).
   */
  def equalRange[U](x: U, lt: (U, U) => Boolean, k: A => U): (Int, Int) =
    equalRange(x, lt, lt, k)

  def equalRange[U, V](x: V,
                       ltUV: (U, V) => Boolean,
                       ltVU: (V, U) => Boolean,
                       k: A => U
                      ): (Int, Int) =
    runSearch(x, ltUV, ltVU, k,
      (l, m, u) =>
        (lowerBound(x, l, m, ltUV, k), upperBound(x, m + 1, u, ltVU, k)),
      (m) =>
        (m, m))

  def equalRange[U >: A](x: U, lt: (U, U) => Boolean): (Int, Int) =
    equalRange(x, lt, lt, identity[U])

  def equalRange[U >: A, V](x: V, ltUV: (U, V) => Boolean, ltVU: (V, U) => Boolean): (Int, Int) =
    equalRange(x, ltUV, ltVU, identity[U])

  def containsOrdered[U, V](x: V,
                            ltUV: (U, V) => Boolean,
                            ltVU: (V, U) => Boolean,
                            k: A => U
                           ): Boolean =
    runSearch(x, ltUV, ltVU, k, (_, _, _) => true, _ => false)

  def containsOrdered[U](x: U, lt: (U, U) => Boolean, k: A => U): Boolean =
    containsOrdered(x, lt, lt, k)

  def containsOrdered[U >: A](x: U, lt: (U, U) => Boolean): Boolean =
    containsOrdered(x, lt, lt, identity[U])

  def containsOrdered[U >: A, V](x: V, ltUV: (U, V) => Boolean, ltVU: (V, U) => Boolean): Boolean =
    containsOrdered(x, ltUV, ltVU, identity[U])

  /** Returns 'start' <= i <= 'end' such that p(k(a(j))) is false for all j
   * in ['start', i), and p(k(a(j))) is true for all j in [i, 'end').
   *
   * Assumes p(k(_)) partitions a, i.e. for all 0 <= i <= j < a.size,
   * if p(k(a(i))) then p(k(a(j))).
   */
  def partitionPoint[U](p: U => Boolean, start: Int, end: Int, k: A => U): Int = {
    var left = start
    var right = end
    while (left < right) {
      val mid = (left + right) >>> 1 // works even when sum overflows
      if (p(k(T.at(a, mid))))
        right = mid
      else
        left = mid + 1
    }
    left
  }

  def partitionPoint[U >: A](p: U => Boolean, start: Int, end: Int): Int =
    partitionPoint(p, start, end, identity[U])

  def partitionPoint[U](p: U => Boolean, k: A => U): Int =
    partitionPoint(p, 0, T.length(a), k)

  def partitionPoint[U >: A](p: U => Boolean): Int =
    partitionPoint(p, identity[U])

  /** Perform binary search until either an index i is found for which k(a(i))
   * is incomparible with 'x', or it is certain that no such i exists. In the
   * first case, call 'found'(l, i, u), where [l, u] is the current range of
   * the search. In the second case, call 'notFound'(j), where k(a(i)) < x for
   * all i in [0, j) and x < k(a(i)) for all i in [j, a.size).
   */
  private def runSearch[U, V, R](x: V,
                                 ltUV: (U, V) => Boolean,
                                 ltVU: (V, U) => Boolean,
                                 k: A => U,
                                 found: (Int, Int, Int) => R,
                                 notFound: Int => R
                                ): R = {
    var left = 0
    var right = T.length(a)
    while (left < right) {
      // a(i) < x for all i in [0, left)
      // x < a(i) for all i in [right, a.size)
      val mid = (left + right) >>> 1 // works even when sum overflows
      if (ltVU(x, k(T.at(a, mid))))
      // x < a(i) for all i in [mid, a.size)
        right = mid
      else if (ltUV(k(T.at(a, mid)), x))
      // a(i) < x for all i in [0, mid]
        left = mid + 1
      else
      // !(a(i) < x) for all i in [mid, a.size)
      // !(x < a(i)) for all i in [0, mid]
        return found(left, mid, right)
    }
    notFound(left)
  }

  def treeReduce(f: (A, A) => A)(implicit ev: A <:< AnyRef): A =
    T.length(a) match {
      case 0 => null.asInstanceOf[A]
      case length =>
        var res = T.at(a, 0)
        var i = 1
        while (i < length) {
          res = f(res, T.at(a, i))
          i = i + 1
        }

        res
    }

  def elementsSameObjects(b: Repr[A])(implicit ev: A <:< AnyRef): Boolean = {
    val alen = T.length(a)
    if (alen != T.length(b)) return false

    var i = 0
    while (i < alen) {
      if (T.at(a, i) != T.at(b, i)) return false
      i += 1
    }

    true
  }

  def fmap[B: ClassTag](f: A => B): Repr[B] = 
    T.tabulate[B](T.length(a)) { idx =>
      f(T.at(a, idx))
    }

  def concatMap[B: ClassTag](b: Repr[A])(f: A => B): Repr[B] = {
    val alen = T.length(a)
    val blen = T.length(b)

    val r = T.allocate[B](alen + blen)
    var i = 0
    while (i < alen) {
      T.assign(r, i, f(T.at(a, i)))
      i += i
    }

    var j = 0
    while (j < blen) {
      T.assign(r, j + alen, f(T.at(b, j)))
      j += 1
    }

    r
  }

  def zipMap[B, C: ClassTag](b: Repr[B])(f: (A, B) => C): Repr[C] =
    T.tabulate[C](Math.min(T.length(a), T.length(b))) { idx =>
      f(T.at(a, idx), T.at(b, idx))
    }

}
