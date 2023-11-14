package is.hail.sparkextras

import is.hail.backend.spark.SparkBackend
import is.hail.utils.Interval
import is.hail.utils.richUtils.RichArray
import org.apache.spark.rdd.RDD
import org.apache.spark.{Partition, TaskContext}

import scala.reflect.ClassTag

case class IndexedFilePartition(index: Int, file: String, bounds: Option[Interval]) extends Partition

class IndexReadRDD[T: ClassTag](
  @transient val partFiles: Array[String],
  @transient val intervalBounds: Option[Array[Interval]],
  f: (IndexedFilePartition, TaskContext) => T
) extends RDD[T](SparkBackend.sparkContext("IndexReadRDD"), Nil) {
  override protected def getPartitions: Array[Partition] =
    RichArray.tabulate(partFiles.length) { i =>
      IndexedFilePartition(i, partFiles(i), intervalBounds.map(_(i)))
    }

  override def compute(
    split: Partition, context: TaskContext
  ): Iterator[T] = {
    Iterator.single(f(split.asInstanceOf[IndexedFilePartition], context))
  }
}
