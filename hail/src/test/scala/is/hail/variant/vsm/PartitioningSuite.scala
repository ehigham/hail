package is.hail.variant.vsm

import is.hail.HailSuite
import is.hail.annotations.BroadcastRow
import is.hail.expr.ir
import is.hail.expr.ir.lowering.Lower.monadLowerInstanceForLower
import is.hail.expr.ir.lowering.LoweringState
import is.hail.expr.ir.{Interpret, MatrixAnnotateRowsTable, TableLiteral, TableValue}
import is.hail.rvd.RVD
import is.hail.types._
import is.hail.types.virtual.{TInt32, TStruct}
import is.hail.utils.FastIndexedSeq
import org.testng.annotations.Test

class PartitioningSuite extends HailSuite {
  @Test def testShuffleOnEmptyRDD() {
    val typ = TableType(TStruct("tidx" -> TInt32), FastIndexedSeq("tidx"), TStruct.empty)
    val t = TableLiteral[Run](
      TableValue(
        typ,
        BroadcastRow.empty[Run].apply(ctx),
        RVD.empty(ctx, typ.canonicalRVDType)
      )
    ).apply(ctx)

    val rangeReader = ir.MatrixRangeReader(100, 10, Some(10))
    Interpret(
      MatrixAnnotateRowsTable(
        ir.MatrixRead(rangeReader.fullMatrixType, false, false, rangeReader),
        t,
        "foo",
        product = false
      ),
      optimize = false
    ).runA(ctx, LoweringState()).rvd.count()
  }
}
