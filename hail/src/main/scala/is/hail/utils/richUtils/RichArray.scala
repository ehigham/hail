package is.hail.utils.richUtils

import is.hail.io.fs.FS
import is.hail.io.{DoubleInputBuffer, DoubleOutputBuffer}
import is.hail.utils._

import scala.reflect.ClassTag

object RichArray extends RichArrayOps {
  val defaultBufSize: Int = 4096 << 3
  
  def importFromDoubles(fs: FS, path: String, n: Int): Array[Double] = {
    val a = new Array[Double](n)
    importFromDoubles(fs, path, a, defaultBufSize)
    a
  }
  
  def importFromDoubles(fs: FS, path: String, a: Array[Double], bufSize: Int): Unit = {
    using(fs.open(path)) { is =>
      val in = new DoubleInputBuffer(is, bufSize)

      in.readDoubles(a)
    }
  }

  def exportToDoubles(fs: FS, path: String, a: Array[Double]): Unit =
    exportToDoubles(fs, path, a, defaultBufSize)

  def exportToDoubles(fs: FS, path: String, a: Array[Double], bufSize: Int): Unit = {
    using(fs.create(path)) { os =>
      val out = new DoubleOutputBuffer(os, bufSize)

      out.writeDoubles(a)
      out.flush()
    }
  }
}

final case class RichArray[T](override val a: Array[T])
  extends RichIndexable[T, Array]
    with RichArrayOps {
  override def T: RichIndexableOps[Array] =
    this

  def index: Map[T, Int] =
    a.zipWithIndex.toMap
}

sealed trait RichArrayOps extends RichIndexableOps[Array]{
  override def length(r: Array[_]): Int = r.length
  override def at[A](r: Array[A], idx: Int): A = r(idx)
  override def allocate[A: ClassTag](size: Int): Array[A] = new Array[A](size)
  override def assign[A](r: Array[A], idx: Int, a: A): Unit = r.update(idx, a)
}