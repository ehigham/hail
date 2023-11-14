package is.hail.linalg
  
import breeze.linalg.DenseMatrix
import is.hail.HailSuite
import is.hail.check.Gen
import is.hail.utils._
import is.hail.utils.richUtils.RichIndexedSeq
import org.testng.annotations.Test

class RowMatrixSuite extends HailSuite {
  private def rowArrayToRowMatrix(a: IndexedSeq[IndexedSeq[Double]], nPartitions: Int = sc.defaultParallelism): RowMatrix = {
    require(a.nonEmpty)
    val nRows = a.length
    val nCols = a(0).length
    
    RowMatrix(sc.parallelize(a.zipWithIndex.fmap { case (row, i) => (i.toLong, row.toArray) }, nPartitions), nCols, nRows)
  }
  
  private def rowArrayToLocalMatrix(a: IndexedSeq[IndexedSeq[Double]]): DenseMatrix[Double] = {
    require(a.nonEmpty)
    val nRows = a.length
    val nCols = a(0).length
    
    new DenseMatrix[Double](nRows, nCols, a.flatten.toArray, 0, nCols, isTranspose = true)
  }
  
  @Test
  def localizeRowMatrix() {
    val fname = ctx.createTmpPath("test")
    
    val rowArrays = FastSeq(
      FastSeq(1.0, 2.0, 3.0),
      FastSeq(4.0, 5.0, 6.0))

    val rowMatrix = rowArrayToRowMatrix(rowArrays)
    val localMatrix = rowArrayToLocalMatrix(rowArrays)
    
    BlockMatrix.fromBreezeMatrix(localMatrix).write(ctx, fname)
    
    assert(rowMatrix.toBreezeMatrix() === localMatrix)
  }

  @Test
  def readBlockSmall() {
    val fname = ctx.createTmpPath("test")
    
    val localMatrix = DenseMatrix(
      FastSeq(1.0, 2.0, 3.0),
      FastSeq(4.0, 5.0, 6.0))
    
    BlockMatrix.fromBreezeMatrix(localMatrix).write(ctx, fname, forceRowMajor = true)
    
    val rowMatrixFromBlock = RowMatrix.readBlockMatrix(fs, fname, 1)
    
    assert(rowMatrixFromBlock.toBreezeMatrix() == localMatrix)
  }
  
  @Test
  def readBlock() {
    val fname = ctx.createTmpPath("test")
    val lm = Gen.denseMatrix[Double](9, 10).sample()

    for {
      blockSize <- Seq(1, 2, 3, 4, 6, 7, 9, 10)
      partSize <- Seq(1, 2, 4, 9, 11)
    } {
      BlockMatrix.fromBreezeMatrix(lm, blockSize).write(ctx, fname, overwrite = true, forceRowMajor = true)
      val rowMatrix = RowMatrix.readBlockMatrix(fs, fname, partSize)
      
      assert(rowMatrix.toBreezeMatrix() === lm)
    }
  }
  
  private def readCSV(fname: String): IndexedSeq[Array[Double]] =
    fs.readLines(fname)( it =>
      it.map(_.value.split(",").fmap(_.toDouble)).toFastSeq
    )

  private def exportImportAssert(export: (String) => Unit, expected: IndexedSeq[Double]*) {
    val fname = ctx.createTmpPath("test")
    export(fname)
    assert(readCSV(fname) === expected)
  }

  @Test
  def exportWithIndex() {
    val rowArrays = FastSeq(
      FastSeq(1.0, 2.0, 3.0),
      FastSeq(4.0, 5.0, 6.0),
      FastSeq(7.0, 8.0, 9.0))
    val rowMatrix = rowArrayToRowMatrix(rowArrays, nPartitions = 2)

    val rowArraysWithIndex = FastSeq(
      FastSeq(0.0, 1.0, 2.0, 3.0),
      FastSeq(1.0, 4.0, 5.0, 6.0),
      FastSeq(2.0, 7.0, 8.0, 9.0))

    exportImportAssert(rowMatrix.export(ctx, _, ",", header = None, addIndex = true, exportType = ExportType.CONCATENATED),
      rowArraysWithIndex: _*)
  }

  @Test
  def exportSquare() {
    val rowArrays = FastSeq(
      FastSeq(1.0, 2.0, 3.0),
      FastSeq(4.0, 5.0, 6.0),
      FastSeq(7.0, 8.0, 9.0))
    val rowMatrix = rowArrayToRowMatrix(rowArrays)

    exportImportAssert(rowMatrix.export(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      rowArrays: _*)

    exportImportAssert(rowMatrix.exportLowerTriangle(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      FastSeq(1.0),
      FastSeq(4.0, 5.0),
      FastSeq(7.0, 8.0, 9.0))

    exportImportAssert(rowMatrix.exportStrictLowerTriangle(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      FastSeq(4.0),
      FastSeq(7.0, 8.0))

    exportImportAssert(rowMatrix.exportUpperTriangle(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      FastSeq(1.0, 2.0, 3.0),
      FastSeq(5.0, 6.0),
      FastSeq(9.0))

    exportImportAssert(rowMatrix.exportStrictUpperTriangle(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      FastSeq(2.0, 3.0),
      FastSeq(6.0))
  }
  
  @Test
  def exportWide() {
    val rowArrays = FastSeq(
      FastSeq(1.0, 2.0, 3.0),
      FastSeq(4.0, 5.0, 6.0))
    val rowMatrix = rowArrayToRowMatrix(rowArrays)

    exportImportAssert(rowMatrix.export(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      rowArrays: _*)

    exportImportAssert(rowMatrix.exportLowerTriangle(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      FastSeq(1.0),
      FastSeq(4.0, 5.0))

    exportImportAssert(rowMatrix.exportStrictLowerTriangle(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      FastSeq(4.0))
    
    exportImportAssert(rowMatrix.exportUpperTriangle(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      FastSeq(1.0, 2.0, 3.0),
      FastSeq(5.0, 6.0))
    
    exportImportAssert(rowMatrix.exportStrictUpperTriangle(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      FastSeq(2.0, 3.0),
      FastSeq(6.0))
  }
  
  @Test
  def exportTall() {
    val rowArrays = FastSeq(
      FastSeq(1.0, 2.0),
      FastSeq(4.0, 5.0),
      FastSeq(7.0, 8.0))
    val rowMatrix = rowArrayToRowMatrix(rowArrays)

    exportImportAssert(rowMatrix.export(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      rowArrays: _*)

    exportImportAssert(rowMatrix.exportLowerTriangle(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      FastSeq(1.0),
      FastSeq(4.0, 5.0),
      FastSeq(7.0, 8.0))

    exportImportAssert(rowMatrix.exportStrictLowerTriangle(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      FastSeq(4.0),
      FastSeq(7.0, 8.0))

    exportImportAssert(rowMatrix.exportUpperTriangle(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      FastSeq(1.0, 2.0),
      FastSeq(5.0))
    
    exportImportAssert(rowMatrix.exportStrictUpperTriangle(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      FastSeq(2.0))
  }  

  @Test
  def exportBig() {
    val rowArrays: IndexedSeq[IndexedSeq[Double]] =
      RichIndexedSeq.tabulate(20)( r => RichIndexedSeq.tabulate(30)(c => 30 * c + r))
    val rowMatrix = rowArrayToRowMatrix(rowArrays)
    
    exportImportAssert(rowMatrix.export(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      rowArrays: _*)

    exportImportAssert(rowMatrix.exportLowerTriangle(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      rowArrays.zipWithIndex
        .fmap { case (a, i) => a.zipWithIndex.filter { case (_, j) => j <= i }.fmap(_._1).toFastSeq }
        .toFastSeq:_*
    )
        
    exportImportAssert(rowMatrix.exportStrictLowerTriangle(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      rowArrays.zipWithIndex
        .fmap { case (a, i) => a.zipWithIndex.filter { case (_, j) => j < i }.fmap(_._1).toFastSeq }
        .filter(_.nonEmpty)
        .toFastSeq :_*
    )

    exportImportAssert(rowMatrix.exportUpperTriangle(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      rowArrays.zipWithIndex
        .fmap { case (a, i) => a.zipWithIndex.filter { case (_, j) => j >= i }.fmap(_._1).toFastSeq }
        .toFastSeq :_*
    )

    exportImportAssert(rowMatrix.exportStrictUpperTriangle(ctx, _, ",", header=None, addIndex = false, exportType = ExportType.CONCATENATED),
      rowArrays.zipWithIndex
        .fmap { case (a, i) => a.zipWithIndex.filter { case (_, j) => j > i }.fmap(_._1).toFastSeq }
        .filter(_.nonEmpty)
        .toFastSeq :_*
    )
  }
}