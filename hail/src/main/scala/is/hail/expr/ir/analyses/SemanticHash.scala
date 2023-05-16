package is.hail.expr.ir.analyses

import is.hail.expr.ir._
import is.hail.io.fs.FS
import is.hail.types.virtual._
import is.hail.utils.Logging
import org.apache.commons.codec.digest.MurmurHash3

import java.util.UUID
import scala.language.implicitConversions

case object SemanticHash extends Logging {

  type Type = Int
  type NextHash = () => Type

  // Picked from https://softwareengineering.stackexchange.com/a/145633
  object Type {
    def apply(c: Class[_]): Type =
      apply(c.getName)

    def apply(a: Any): Type =
      apply(a.toString)

    def apply(s: String): Type =
      apply(s.getBytes)

    def apply(bytes: Array[Byte]): Type =
      MurmurHash3.hash32x86(bytes)
  }

  object Implicits {
    implicit class MagmaHashType(a: Type) {
      def <>(b: Type): Type =
        MurmurHash3.hash32(a, b, MurmurHash3.DEFAULT_SEED)
    }
  }

  import Implicits._

  def getFileHash(fs: FS)(path: String): Type =
    Type(fs.fileChecksum(path))

  def unique: Type =
    Type(UUID.randomUUID.toString)

  def apply(fs: FS)(root: BaseIR): NextHash = {
    val memo = Memo.empty[SemanticHash.Type]
    val normalized = new NormalizeNames(_.toString)(root)
    for (ir <- IRTraversal.postOrder(normalized)) {
      memo.bind(ir, ir match {
        case Apply(fname, _, args, _, _) =>
          args.foldLeft(Type(classOf[Apply]) <> Type(fname))(_ <> memo(_))

        case ApplyBinaryPrimOp(op, x, y) =>
          Type(classOf[ApplyBinaryPrimOp]) <> Type(op.getClass) <> memo(x) <> memo(y)

        case ApplyComparisonOp(op, x, y) =>
          Type(classOf[ApplyComparisonOp]) <> Type(op.getClass) <> memo(x) <> memo(y)

        case ApplySeeded(fname, args, rngState: IR, _, _) =>
          args.foldLeft(Type(classOf[ApplySeeded]) <> Type(fname))(_ <> memo(_)) <> memo(rngState)

        case ApplySpecial(fname, _, args, _, _) =>
          args.foldLeft(Type(classOf[ApplySpecial]) <> Type(fname))(_ <> memo(_))

        case ApplyUnaryPrimOp(op, x) =>
          Type(classOf[ApplyUnaryPrimOp]) <> Type(op.getClass) <> memo(x)

        case Cast(ir, typ) =>
          Type(classOf[Cast]) <> memo(ir) <> Type(SemanticTypeName(typ))

        case GetField(ir, name) =>
          Type(classOf[GetField]) <> memo(ir) <> Type(ir.typ.asInstanceOf[TStruct].fieldIdx(name))

        case GetTupleElement(ir, idx) =>
          Type(classOf[GetTupleElement]) <> memo(ir) <> Type(idx)

        case Literal(typ, value) =>
          Type(classOf[Literal]) <> Type(typ.toJSON(value))

        case MakeStruct(fields) =>
          fields.zipWithIndex.foldLeft(Type(classOf[MakeStruct])) { case (result, ((_, ir), index)) =>
            result <> Type(index) <> memo(ir)
          }

        case MakeTuple(fields) =>
          fields.foldLeft(Type(classOf[MakeTuple])) { case (result, (index, ir)) =>
            result <> Type(index) <> memo(ir)
          }

        case MatrixRead(_, _, _, reader) =>
          reader.pathsUsed.map(getFileHash(fs)).foldLeft(Type(classOf[MatrixRead]))(_ <> _)

        case MatrixWrite(child, writer) =>
          Type(classOf[MatrixWrite]) <> memo(child) <> Type(writer.path)

        case ReadPartition(context, _, reader) =>
          Type(classOf[ReadPartition]) <> memo(context) <> Type(reader.toJValue)

        // Notes:
        // - If the name in the (Relational)Ref is free then it's impossible to know the semantic hash
        // - The semantic hash of a (Relational)Ref cannot be the same as the value it binds to as
        //   that would imply that the evaluation of the value to is the same as evaluating the ref itself.
        case Ref(name, _) =>
          Type(classOf[Ref]) <> Type(name)

        case RelationalRef(name, _) =>
          Type(classOf[RelationalRef]) <> Type(name)

        case SelectFields(struct, names) =>
          Type(classOf[SelectFields]) <> names.foldLeft(memo(struct))(_ <> Type(_))

        case StreamZip(streams, _, body, behaviour, _) =>
          streams.foldLeft(Type(classOf[StreamZip]))(_ <> memo(_)) <> memo(body) <> Type(behaviour)

        case TableRead(_, _, reader) =>
          reader.pathsUsed.map(getFileHash(fs)).foldLeft(Type(classOf[TableRead]))(_ <> _)

        case TableRange(count, numPartitions) =>
          Type(classOf[TableRange]) <> Type(count) <> Type(numPartitions)

        case TableWrite(child, writer) =>
          Type(classOf[TableWrite]) <> memo(child) <> Type(writer.path)

        case TableKeyBy(child, keys, _) =>
          keys.foldLeft(Type(classOf[TableKeyBy]) <> memo(child))(_ <> Type(_))

        case WritePartition(partition, context, writer) =>
          Type(classOf[WritePartition]) <> memo(partition) <> memo(context) <> Type(writer.toJValue)

        case WriteMetadata(writeAnnotations, writer) =>
          Type(classOf[WriteMetadata]) <> memo(writeAnnotations) <> Type(writer.toJValue)

        case WriteValue(value, path, _, stagingFile) =>
          stagingFile.foldLeft(Type(classOf[WriteValue]) <> memo(value) <> memo(path))(_ <> memo(_))

        // The following are parameterized entirely by the operation's input and the operation itself
        case _: ArrayLen |
             _: ArrayRef |
             _: ArraySlice |
             _: ArraySort |
             _: ArrayZeros |
             _: Begin |
             _: BlockMatrixCollect |
             _: CastToArray |
             _: Coalesce |
             _: CollectDistributedArray |
             _: ConsoleLog |
             _: Consume |
             _: Die |
             _: GroupByKey |
             _: If |
             _: InsertFields |
             _: IsNA |
             _: Let |
             _: LiftMeOut |
             _: MakeArray |
             _: MakeNDArray |
             _: MakeStream |
             _: MatrixAggregate |
             _: MatrixCount |
             _: MatrixMapGlobals |
             _: MatrixLiteral |
             _: MatrixMapRows |
             _: MatrixFilterRows |
             _: MatrixMapCols |
             _: MatrixFilterCols |
             _: MatrixMapEntries |
             _: MatrixFilterEntries |
             _: MatrixDistinctByRow |
             _: NDArrayShape |
             _: NDArraySlice |
             _: NDArrayWrite |
             _: RelationalLet |
             _: StreamDrop |
             _: StreamDropWhile |
             _: StreamFilter |
             _: StreamFlatMap |
             _: StreamFold |
             _: StreamFold2 |
             _: StreamFor |
             _: StreamLen |
             _: StreamMap |
             _: StreamRange |
             _: StreamTake |
             _: StreamTakeWhile |
             _: TableGetGlobals |
             _: TableCollect |
             _: TableAggregate |
             _: TableCount |
             _: TableMapRows |
             _: TableMapGlobals |
             _: TableFilter |
             _: TableDistinct |
             _: ToArray |
             _: ToDict |
             _: ToSet |
             _: ToStream |
             _: Trap =>
          ir.children.map(memo(_)).foldLeft(Type(ir.getClass))(_ <> _)

        // Discrete values
        case c@(_: Void | _: True | _: False) =>
          Type(c.getClass)

        // integral constants
        case I32(x) => Type(classOf[I32]) <> Type(x)
        case I64(x) => Type(classOf[I64]) <> Type(x)
        case F32(x) => Type(classOf[F32]) <> Type(x)
        case F64(x) => Type(classOf[F64]) <> Type(x)
        case NA(typ) => Type(classOf[NA]) <> Type(SemanticTypeName(typ))
        case Str(x) => Type(classOf[Str]) <> Type(x)

        // In these cases, just return a random SemanticHash meaning that two
        // invocations will never return the same thing.
        case _ =>
          log.info(s"SemanticHash unknown: ${ir.getClass.getName}")
          unique
      })

      log.info(s"[${memo(ir)}]: $ir")
    }

    val semhash = memo(normalized)
    var count = 0
    () => { val h = count; count += 1; semhash <> Type(h)}
  }
}

case object SemanticTypeName {
  def apply(t: Type): String = {
    val sb = StringBuilder.newBuilder

    def go(typ: Type): Unit = {
      sb.append(typ.getClass.getSimpleName)
      val children = typ.children
      if (children.nonEmpty) {
        sb.append('[')
        go(children.head)

        children.tail.foreach { t =>
          sb.append(',')
          go(t)
        }

        sb.append(']')
      }
    }

    go(t)

    sb.toString()
  }
}
