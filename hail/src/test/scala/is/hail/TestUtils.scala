package is.hail

import breeze.linalg.{DenseMatrix, Matrix, Vector}
import cats.implicits.toFunctorOps
import cats.syntax.all._
import is.hail.annotations.{Region, RegionValueBuilder, SafeRow}
import is.hail.asm4s._
import is.hail.backend.ExecuteContext
import is.hail.expr.ir.lowering.utils._
import is.hail.expr.ir.lowering.{Lower, LowererUnsupportedOperation, LoweringState, MonadLower}
import is.hail.expr.ir._
import is.hail.io.vcf.MatrixVCFReader
import is.hail.types.physical.stypes.PTypeReferenceSingleCodeType
import is.hail.types.physical.{PBaseStruct, PCanonicalArray, PType}
import is.hail.types.virtual._
import is.hail.utils._
import is.hail.variant._
import org.apache.spark.SparkException
import org.apache.spark.sql.Row

import java.io.PrintWriter
import scala.collection.mutable
import scala.language.higherKinds

object ExecStrategy extends Enumeration {
  type ExecStrategy = Value
  val Interpret, InterpretUnoptimized, JvmCompile, LoweredJVMCompile, JvmCompileUnoptimized = Value

  val unoptimizedCompileOnly: Set[ExecStrategy] = Set(JvmCompileUnoptimized)
  val compileOnly: Set[ExecStrategy] = Set(JvmCompile, JvmCompileUnoptimized)
  val javaOnly: Set[ExecStrategy] = Set(Interpret, InterpretUnoptimized, JvmCompile, JvmCompileUnoptimized)
  val interpretOnly: Set[ExecStrategy] = Set(Interpret, InterpretUnoptimized)
  val nonLowering: Set[ExecStrategy] = Set(Interpret, InterpretUnoptimized, JvmCompile, JvmCompileUnoptimized)
  val lowering: Set[ExecStrategy] = Set(LoweredJVMCompile)
  val backendOnly: Set[ExecStrategy] = Set(LoweredJVMCompile)
  val allRelational: Set[ExecStrategy] = interpretOnly.union(lowering)
}

object TestUtils {
  val theHailClassLoader = new HailClassLoader(getClass().getClassLoader())

  import org.scalatest.Assertions._

  def interceptException[E <: Throwable : Manifest](regex: String)(f: => Any) {
    val thrown = intercept[E](f)
    val p = regex.r.findFirstIn(thrown.getMessage).isDefined
    val msg =
      s"""expected fatal exception with pattern '$regex'
         |  Found: ${ thrown.getMessage } """
    if (!p)
      println(msg)
    assert(p, msg)
  }
  def interceptFatal(regex: String)(f: => Any) {
    interceptException[HailException](regex)(f)
  }

  def interceptSpark(regex: String)(f: => Any) {
    interceptException[SparkException](regex)(f)
  }

  def interceptAssertion(regex: String)(f: => Any) {
    interceptException[AssertionError](regex)(f)
  }

  def assertVectorEqualityDouble(A: Vector[Double], B: Vector[Double], tolerance: Double = utils.defaultTolerance) {
    assert(A.size == B.size)
    assert((0 until A.size).forall(i => D_==(A(i), B(i), tolerance)))
  }

  def assertMatrixEqualityDouble(A: Matrix[Double], B: Matrix[Double], tolerance: Double = utils.defaultTolerance) {
    assert(A.rows == B.rows)
    assert(A.cols == B.cols)
    assert((0 until A.rows).forall(i => (0 until A.cols).forall(j => D_==(A(i, j), B(i, j), tolerance))))
  }

  def isConstant(A: Vector[Int]): Boolean = {
    (0 until A.length - 1).foreach(i => if (A(i) != A(i + 1)) return false)
    true
  }

  def removeConstantCols(A: DenseMatrix[Int]): DenseMatrix[Int] = {
    val data = (0 until A.cols).flatMap { j =>
      val col = A(::, j)
      if (TestUtils.isConstant(col))
        Array[Int]()
      else
        col.toArray
    }.toArray

    val newCols = data.length / A.rows
    new DenseMatrix(A.rows, newCols, data)
  }

  def unphasedDiploidGtIndicesToBoxedCall(m: DenseMatrix[Int]): DenseMatrix[BoxedCall] = {
    m.map(g => if (g == -1) null: BoxedCall else Call2.fromUnphasedDiploidGtIndex(g): BoxedCall)
  }


  def loweredExecute(ctx: ExecuteContext, x: IR, env: Env[(Any, Type)],
    args: IndexedSeq[(Any, Type)],
    agg: Option[(IndexedSeq[Row], TStruct)],
    bytecodePrinter: Option[PrintWriter] = None
  ): Any = {
    if (agg.isDefined || !env.isEmpty || !args.isEmpty)
      throw new LowererUnsupportedOperation("can't test with aggs or user defined args/env")

    ExecutionTimer.logTime("TestUtils.loweredExecute") { timer =>
      HailContext.sparkBackend("TestUtils.loweredExecute")
        .jvmLowerAndExecute(ctx, x, optimize = false, lowerTable = true, lowerBM = true, print = bytecodePrinter)
    }
  }

  def eval(x: IR): Any = ExecuteContext.scoped() { ctx =>
    eval[Lower](x, Env.empty, FastIndexedSeq(), None, None, true).runA(ctx, LoweringState())
  }

  def eval[M[_]](x: IR,
                 env: Env[(Any, Type)],
                 args: IndexedSeq[(Any, Type)],
                 agg: Option[(IndexedSeq[Row], TStruct)],
                 bytecodePrinter: Option[PrintWriter] = None,
                 optimize: Boolean = true
                )
                (implicit M: MonadLower[M]): M[Any] = {
    val inputTypesB = new BoxedArrayBuilder[Type]()
    val inputsB = new mutable.ArrayBuffer[Any]()

    args.foreach { case (v, t) =>
      inputsB += v
      inputTypesB += t
    }

    env.m.foreach { case (name, (v, t)) =>
      inputsB += v
      inputTypesB += t
    }

    val argsType = TTuple(inputTypesB.result(): _*)
    val resultType = TTuple(x.typ)
    val argsVar = genUID()

    val (_, substEnv) = env.m.foldLeft((args.length, Env.empty[IR])) { case ((i, env), (name, (v, t))) =>
      (i + 1, env.bind(name, GetTupleElement(Ref(argsVar, argsType), i)))
    }

    def rewrite(x: IR): IR = {
      x match {
        case In(i, t) =>
          GetTupleElement(Ref(argsVar, argsType), i)
        case _ =>
          MapIR(rewrite)(x)
      }
    }

    val argsPType = PType.canonical(argsType).setRequired(true)
    agg match {
      case Some((aggElements, aggType)) =>
        val aggElementVar = genUID()
        val aggArrayVar = genUID()
        val aggPType = PType.canonical(aggType)
        val aggArrayPType = PCanonicalArray(aggPType, required = true)

        val substAggEnv = aggType.fields.foldLeft(Env.empty[IR]) { case (env, f) =>
          env.bind(f.name, GetField(Ref(aggElementVar, aggType), f.name))
        }
        val aggIR = StreamAgg(ToStream(Ref(aggArrayVar, aggArrayPType.virtualType)),
          aggElementVar,
          MakeTuple.ordered(FastSeq(rewrite(Subst(x, BindingEnv(eval = substEnv, agg = Some(substAggEnv)))))))

        for {
          (Some(PTypeReferenceSingleCodeType(resultType2)), f) <-
            Compile[M, AsmFunction3RegionLongLongLong](
              FastIndexedSeq(
                argsVar -> SingleCodeEmitParamType(true, PTypeReferenceSingleCodeType(argsPType)),
                aggArrayVar -> SingleCodeEmitParamType(true, PTypeReferenceSingleCodeType(aggArrayPType))
              ),
              FastIndexedSeq(classInfo[Region], LongInfo, LongInfo),
              LongInfo,
              aggIR,
              print = bytecodePrinter,
              optimize = optimize
            )
          _ <- assertA(resultType2.virtualType == resultType)
          ctx <- M.ask
        } yield ctx.r.pool.scopedRegion { region =>
          val rvb = new RegionValueBuilder(ctx.stateManager, region)
          rvb.start(argsPType)
          rvb.startTuple()
          var i = 0
          while (i < inputsB.length) {
            rvb.addAnnotation(inputTypesB(i), inputsB(i))
            i += 1
          }
          rvb.endTuple()
          val argsOff = rvb.end()

          rvb.start(aggArrayPType)
          rvb.startArray(aggElements.length)
          aggElements.foreach { r =>
            rvb.addAnnotation(aggType, r)
          }
          rvb.endArray()
          val aggOff = rvb.end()

          val resultOff = f(theHailClassLoader, ctx.fs, ctx.taskContext, region)(region, argsOff, aggOff)
          SafeRow(resultType2.asInstanceOf[PBaseStruct], resultOff).get(0)
        }

      case None =>
        for {
          (Some(PTypeReferenceSingleCodeType(resultType2)), f) <-
            Compile[M, AsmFunction2RegionLongLong](
              FastIndexedSeq((argsVar, SingleCodeEmitParamType(true, PTypeReferenceSingleCodeType(argsPType)))),
              FastIndexedSeq(classInfo[Region], LongInfo), LongInfo,
              MakeTuple.ordered(FastSeq(rewrite(Subst(x, BindingEnv(substEnv))))),
              optimize = optimize,
              print = bytecodePrinter
            )

          _ <- assertA(resultType2.virtualType == resultType)
          ctx <- M.ask
        } yield ctx.r.pool.scopedRegion { region =>
          val rvb = new RegionValueBuilder(ctx.stateManager, region)
          rvb.start(argsPType)
          rvb.startTuple()
          var i = 0
          while (i < inputsB.length) {
            rvb.addAnnotation(inputTypesB(i), inputsB(i))
            i += 1
          }
          rvb.endTuple()
          val argsOff = rvb.end()

          val resultOff = f(theHailClassLoader, ctx.fs, ctx.taskContext, region)(region, argsOff)
          SafeRow(resultType2.asInstanceOf[PBaseStruct], resultOff).get(0)
        }
    }
  }

  def assertEvalSame(x: IR) {
    assertEvalSame(x, Env.empty, FastIndexedSeq())
  }

  def assertEvalSame(x: IR, args: IndexedSeq[(Any, Type)]) {
    assertEvalSame(x, Env.empty, args)
  }

  def assertEvalSame(x: IR, env: Env[(Any, Type)], args: IndexedSeq[(Any, Type)]) {
    val t = x.typ

    implicit val M = Lower.monadLowerInstanceForLower
    val reset = M.set(LoweringState())
    val (i, i2, c) = ExecuteContext.scoped() { ctx =>
      (for {
        i: Any <- reset *> Interpret(x, env, args)
        i2: Any <- reset *> Interpret(x, env, args, optimize = false)
        c: Any <- reset *> eval(x, env, args, None, None)
      } yield (i, i2, c)).runA(ctx, LoweringState())
    }

    assert(t.typeCheck(i))
    assert(t.typeCheck(i2))
    assert(t.typeCheck(c))

    assert(t.valuesSimilar(i, c), s"interpret $i vs compile $c")
    assert(t.valuesSimilar(i2, c), s"interpret (optimize = false) $i vs compile $c")
  }

  def assertThrows[E <: Throwable : Manifest](x: IR, regex: String) {
    assertThrows[E](x, Env.empty[(Any, Type)], FastIndexedSeq.empty[(Any, Type)], regex)
  }

  def assertThrows[E <: Throwable : Manifest](x: IR, env: Env[(Any, Type)], args: IndexedSeq[(Any, Type)], regex: String) {
    ExecuteContext.scoped() { ctx =>
      import Lower.monadLowerInstanceForLower
      interceptException[E](regex)(Interpret(x, env, args).run(ctx, LoweringState()))
      interceptException[E](regex)(Interpret(x, env, args, optimize = false).run(ctx, LoweringState()))
      interceptException[E](regex)(eval(x, env, args, None, None).run(ctx, LoweringState()))
    }
  }

  def assertFatal(x: IR, regex: String) {
    assertThrows[HailException](x, regex)
  }

  def assertFatal(x: IR, args: IndexedSeq[(Any, Type)], regex: String) {
    assertThrows[HailException](x, Env.empty[(Any, Type)], args, regex)
  }

  def assertFatal(x: IR, env: Env[(Any, Type)], args: IndexedSeq[(Any, Type)], regex: String) {
    assertThrows[HailException](x, env, args, regex)
  }

  def assertCompiledThrows[E <: Throwable : Manifest](x: IR, env: Env[(Any, Type)], args: IndexedSeq[(Any, Type)], regex: String) {
    ExecuteContext.scoped() { ctx =>
      import Lower.monadLowerInstanceForLower
      interceptException[E](regex)(eval(x, env, args, None, None).runA(ctx, LoweringState()))
    }
  }

  def assertCompiledThrows[E <: Throwable : Manifest](x: IR, regex: String) {
    assertCompiledThrows[E](x, Env.empty[(Any, Type)], FastIndexedSeq.empty[(Any, Type)], regex)
  }

  def assertCompiledFatal(x: IR, regex: String) {
    assertCompiledThrows[HailException](x, regex)
  }

  def importVCF(ctx: ExecuteContext, file: String, force: Boolean = false,
    forceBGZ: Boolean = false,
    headerFile: Option[String] = None,
    nPartitions: Option[Int] = None,
    blockSizeInMB: Option[Int] = None,
    minPartitions: Option[Int] = None,
    dropSamples: Boolean = false,
    callFields: Set[String] = Set.empty[String],
    rg: Option[String] = Some(ReferenceGenome.GRCh37),
    contigRecoding: Option[Map[String, String]] = None,
    arrayElementsRequired: Boolean = true,
    skipInvalidLoci: Boolean = false,
    partitionsJSON: Option[String] = None,
    partitionsTypeStr: Option[String] = None): MatrixIR = {
    val entryFloatType = TFloat64._toPretty

    val reader = MatrixVCFReader(ctx,
      Array(file),
      callFields,
      entryFloatType,
      headerFile,
      /*sampleIDs=*/None,
      nPartitions,
      blockSizeInMB,
      minPartitions,
      rg,
      contigRecoding.getOrElse(Map.empty[String, String]),
      arrayElementsRequired,
      skipInvalidLoci,
      forceBGZ,
      force,
      TextInputFilterAndReplace(),
      partitionsJSON,
      partitionsTypeStr)
    MatrixRead(reader.fullMatrixTypeWithoutUIDs, dropSamples, false, reader)
  }
}
