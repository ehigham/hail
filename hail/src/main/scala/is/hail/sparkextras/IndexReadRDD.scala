package is.hail.sparkextras

import is.hail.backend.spark.SparkBackend
import is.hail.utils.{Interval, toRichIndexedSeq}
import org.apache.spark.{Dependency, Partition, RangeDependency, SparkContext, TaskContext}
import org.apache.spark.rdd.RDD

import scala.reflect.ClassTag

case class IndexedFilePartition(index: Int, file: String, bounds: Option[Interval]) extends Partition

class IndexReadRDD[T: ClassTag](
  @transient val partFiles: Array[String],
  @transient val intervalBounds: Option[Array[Interval]],
  f: (IndexedFilePartition, TaskContext) => T
) extends RDD[T](SparkBackend.sparkContext("IndexReadRDD"), Nil) {
  override protected def getPartitions: Array[Partition] =
    partFiles.indices fmap[Partition] { i =>
      IndexedFilePartition(i, partFiles(i), intervalBounds.map(_(i)))
    }

  override def compute(
    split: Partition, context: TaskContext
  ): Iterator[T] = {
    Iterator.single(f(split.asInstanceOf[IndexedFilePartition], context))
  }
}
