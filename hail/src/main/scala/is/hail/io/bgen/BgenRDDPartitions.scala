package is.hail.io.bgen

import is.hail.backend.ExecuteContext
import is.hail.types.virtual._
import is.hail.utils._
import is.hail.utils.richUtils.RichArray

case class FilePartitionInfo(
  metadata: BgenFileMetadata,
  intervals: IndexedSeq[Interval],
  partStarts: IndexedSeq[Long],
  partN: IndexedSeq[Long]
)

object BgenRDDPartitions extends Logging {
  def checkFilesDisjoint(ctx: ExecuteContext, fileMetadata: IndexedSeq[BgenFileMetadata], keyType: Type): Array[Interval] = {
    assert(fileMetadata.nonEmpty)
    val pord = keyType.ordering(ctx.stateManager)

    val overlappingBounds = new BoxedArrayBuilder[(String, Interval, String, Interval)]
    var i = 0
    while (i < fileMetadata.length) {
      var j = 0
      while (j < i) {
        val b1 = fileMetadata(i)
        val b2 = fileMetadata(j)
        if (!b1.rangeBounds.isDisjointFrom(pord, b2.rangeBounds))
          overlappingBounds += ((b1.path, b1.rangeBounds, b2.path, b2.rangeBounds))
        j += 1
      }
      i += 1
    }

    if (!overlappingBounds.isEmpty)
      fatal(
        s"""Each BGEN file must contain a region of the genome disjoint from other files. Found the following overlapping files:
           |  ${
          overlappingBounds.result().fmap { case (f1, i1, f2, i2) =>
            s"file1: $f1\trangeBounds1: $i1\tfile2: $f2\trangeBounds2: $i2"
          }.mkString("\n  ")
        })""".stripMargin)

    RichArray.tabulate(fileMetadata.length){ i =>
      fileMetadata(i).rangeBounds
    }
  }

  def apply(
    ctx: ExecuteContext,
    rg: Option[String],
    files: IndexedSeq[BgenFileMetadata],
    blockSizeInMB: Option[Int],
    nPartitions: Option[Int],
    keyType: Type
  ): IndexedSeq[FilePartitionInfo] = {
    val fs = ctx.fs

    val fileRangeBounds = checkFilesDisjoint(ctx, files, keyType)
    val intervalOrdering = TInterval(keyType).ordering(ctx.stateManager)

    val sortedFiles = files.zip(fileRangeBounds)
      .sortWith { case ((_, i1), (_, i2)) => intervalOrdering.lt(i1, i2) }
      .fmap(_._1)

    val totalSize = sortedFiles.fmap(_.header.fileByteSize).sum

    val fileNPartitions = (blockSizeInMB, nPartitions) match {
      case (Some(blockSizeInMB), _) =>
        val blockSizeInB = blockSizeInMB * 1024 * 1024
        sortedFiles.fmap { md =>
          val size = md.header.fileByteSize
          ((size + blockSizeInB - 1) / blockSizeInB).toInt
        }
      case (_, Some(nParts)) =>
        sortedFiles.fmap { md =>
          val size = md.header.fileByteSize
          ((size * nParts + totalSize - 1) / totalSize).toInt
        }
      case (None, None) => fatal(s"Must specify either of 'blockSizeInMB' or 'nPartitions'.")
    }

    val nonEmptyFilesAfterFilter = sortedFiles.filter(_.nVariants > 0)

    val (leafSpec, intSpec) = BgenSettings.indexCodecSpecs(files.head.indexVersion, rg)
    val getKeysFromFile = StagedBGENReader.queryIndexByPosition(ctx, leafSpec, intSpec)

    nonEmptyFilesAfterFilter.zipWithIndex.fmap { case (file, fileIndex) =>
      val nPartitions = math.min(fileNPartitions(fileIndex), file.nVariants).toInt
      val partNVariants = partition(file.nVariants, nPartitions)
      val partFirstVariantIndex = partNVariants.view.scan(0L)(_ + _).init
      val partLastVariantIndex = partFirstVariantIndex.zip(partNVariants).map { case (idx, n) => idx + n }

      val allPositions = (partFirstVariantIndex ++ partLastVariantIndex.map(_ - 1L)).toArray
      val keys = getKeysFromFile(file.indexPath, allPositions)
      val rangeBounds = RichArray.tabulate(nPartitions){ i =>
        Interval(keys(i), keys(i + nPartitions), true,
          true // this must be true -- otherwise boundaries with duplicates will have the wrong range bounds
        )
      }

      FilePartitionInfo(file, rangeBounds, partFirstVariantIndex.toArray, partNVariants)
    }
  }
}
