package is.hail.utils

import scala.reflect.ClassTag

object FastSeq {
  def empty[T: ClassTag]: IndexedSeq[T] =
    FastSeq()

  def apply[T: ClassTag](args: T*): IndexedSeq[T] =
    args.toFastSeq

  def tabulate[A: ClassTag](n: Int)(f: Int => A): IndexedSeq[A] =
    0 until n fmap f

  def tabulate[A: ClassTag](m: Int, n: Int)(f: (Int, Int) => A): IndexedSeq[IndexedSeq[A]] =
    tabulate(m)(i => tabulate(n)(j => f(i, j)))
}
