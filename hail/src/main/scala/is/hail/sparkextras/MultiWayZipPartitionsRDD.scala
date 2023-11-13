package is.hail.sparkextras

import is.hail.utils.{FastSeq, toRichIndexedSeq}
import org.apache.spark.rdd.RDD
import org.apache.spark.{OneToOneDependency, Partition, SparkContext, TaskContext}

import scala.reflect.ClassTag

object MultiWayZipPartitionsRDD {
  def apply[T: ClassTag , V: ClassTag](rdds: IndexedSeq[RDD[T]])
                                      (f: IndexedSeq[Iterator[T]] => Iterator[V])
  : MultiWayZipPartitionsRDD[T, V] = {
    new MultiWayZipPartitionsRDD(rdds.head.sparkContext, rdds, f)
  }
}

private case class MultiWayZipPartition(val index: Int, val partitions: IndexedSeq[Partition])
  extends Partition

class MultiWayZipPartitionsRDD[T: ClassTag, V: ClassTag](
  sc: SparkContext,
  var rdds: IndexedSeq[RDD[T]],
  var f: (IndexedSeq[Iterator[T]]) => Iterator[V]
) extends RDD[V](sc, rdds.fmap(x => new OneToOneDependency(x))) {
  require(rdds.nonEmpty)
  private val numParts = rdds(0).partitions.length
  require(rdds.forall(rdd => rdd.partitions.length == numParts))

  override val partitioner = None

  override def getPartitions: Array[Partition] = {
    (0 until numParts).fmap { i =>
      MultiWayZipPartition(i, rdds.fmap(rdd => rdd.partitions(i)))
    }
  }

  override def compute(s: Partition, tc: TaskContext) = {
    val partitions = s.asInstanceOf[MultiWayZipPartition].partitions
    val arr = FastSeq.tabulate(rdds.length)(i => rdds(i).iterator(partitions(i), tc))
    f(arr)
  }

  override def clearDependencies() {
    super.clearDependencies
    rdds = null
    f = null
  }
}
