package is.hail.expr.ir.anormal

import is.hail.expr.ir.anormal.InferType.Contexts.{InArgument, InCaseAlternative, InEquationFor, InExpression, InPattern}
import is.hail.expr.ir.anormal.InferType.Errors.{ArgumentLengthMismatch, TypeMismatch, UnboundVar}
import is.hail.expr.ir.{IR, IsConstant, Name}
import is.hail.types.virtual.{TBoolean, VType}
import is.hail.utils.toRichIterable
import org.json4s.JValue
import org.json4s.JsonAST.JString

sealed trait ANormal extends Product with Serializable
final case class Expr(e: AExpr) extends ANormal
final case class Let(bindings: IndexedSeq[(Var, AExpr)], body: AExpr) extends ANormal

sealed trait AExpr extends Product with Serializable
final case class Val(a: AVal) extends AExpr
final case class Fn(types: IndexedSeq[VType], params: IndexedSeq[Var], body: ANormal) extends AExpr
final case class Ap(fn: AVal, args: IndexedSeq[AVal]) extends AExpr
final case class If(pred: AVal, cons: ANormal, alt: ANormal) extends AExpr
final case class Case(a: AVal, cases: IndexedSeq[(Const, ANormal)]) extends AExpr

sealed trait AVal extends Product with Serializable
final case class Var(name: Name) extends AVal
final case class Const(c: IR) extends AVal

final case class FnType(params: IndexedSeq[VType], ret: VType) extends VType {
  override def toJSON: JValue = JString(toString)
  override def pretty(sb: StringBuilder, indent: Int, compact: Boolean): Unit = {
    if (params.length > 1) sb += '('
    params.foreachBetween(_.pretty(sb, indent, compact))(sb += ',')
    if (params.length > 1) sb += ')'
    sb ++= " -> "
    ret.pretty(sb, indent, compact)
  }
}

object InferType {

  private[this] case object NotInferred extends VType {
    override def toJSON: JValue = JString(toString)
    override def pretty(sb: StringBuilder, indent: Int, compact: Boolean): Unit = sb ++= "<not inferred>"
  }

  case class TypeInferenceFailure(cause: TypeError, context: List[TypeErrorContext])
    extends Exception({
      val sb = new StringBuilder()
      sb.append(cause)
      context.addString(sb, "\n").toString()
    })

  sealed trait TypeError extends Product
  sealed trait TypeErrorContext extends Product

  object Errors {
    case class TypeMismatch(expected: VType, actual: VType) extends TypeError
    case class ArgumentLengthMismatch(expected: IndexedSeq[VType], actual: IndexedSeq[VType]) extends TypeError
    case class UnboundVar(v: Var) extends TypeError
  }

  object Contexts {
    case class InEquationFor(v: Var) extends TypeErrorContext
    case class InExpression(e: AExpr) extends TypeErrorContext
    case class InArgument(i: Int, fn: AVal) extends TypeErrorContext
    case class InPattern(pattern: Const) extends TypeErrorContext
    case class InCaseAlternative(a: ANormal) extends TypeErrorContext
  }

  def apply(env: Map[Var, VType], an: ANormal): VType =
    an match {
      case Expr(e) => InferType(env, e)
      case Let(bindings, body) =>
        val bodyEnv = bindings.foldLeft(env) {
          case (env, (v, e)) => env + push(v)(v -> InferType(env, e))
        }
        InferType(bodyEnv, body)
    }

  def apply(env: Map[Var, VType], e: AExpr): VType =
    push(e) {
      case Val(a) =>
        InferType(env, a)

      case Fn(types, params, body) =>
        FnType(types, InferType(Map(params.zip(types) :_*), body))

      case Ap(fn, args) =>
        val fnType = InferType(env, fn)
        val argTypes = args.map(InferType(env, _))
        require(fnType.isInstanceOf[FnType], TypeMismatch(FnType(argTypes, NotInferred), fnType))

        val FnType(types, ret) = fnType.asInstanceOf[FnType]
        require(types.length == argTypes.length, ArgumentLengthMismatch(types, argTypes))

        (types, args, types.indices).zipped.foreach { (t, v, i) =>
          val actual = InferType(env, v)
          require(t == actual, TypeMismatch(t, actual), InArgument(i, fn))
        }

        ret

      case If(pref, cons, altr) =>
        val ptype = InferType(env, pref)
        require(ptype == TBoolean, TypeMismatch(TBoolean, ptype))

        val ctype = InferType(env, cons)
        val atype = InferType(env, altr)
        require(ctype == atype, TypeMismatch(ctype, atype))

        ctype

      case Case(a, cases) =>
        val atype = InferType(env, a)
        var rtype: VType = NotInferred

        for ((pattern, alternative) <- cases) {
          val ctype = InferType(env, pattern)
          require(atype == ctype, TypeMismatch(atype, ctype), InPattern(pattern))

          val etype = InferType(env, alternative)
          if (rtype == NotInferred) {
            rtype = etype
          } else {
            require(rtype == etype, TypeMismatch(rtype, etype), InCaseAlternative(alternative))
          }
        }

        rtype
    }

  def apply(env: Map[Var, VType], v: AVal): VType =
    v match {
      case v: Var =>
        env.getOrElse(v, throw TypeInferenceFailure(UnboundVar(v), Nil))

      case Const(c) =>
        assume(IsConstant(c))
        c.typ
    }

  @inline private[this] def push[A](an: AExpr)(f: AExpr => A): A =
    try f(an)
    catch {
      case t: TypeInferenceFailure =>
        val t2 = t.copy(context = InExpression(an) :: t.context)
        t2.setStackTrace(t.getStackTrace)
        throw t2
    }

  @inline private[this] def push[A](binding: Var)(f: => A): A =
    try f
    catch {
      case t: TypeInferenceFailure =>
        val t2 = t.copy(context = InEquationFor(binding) :: t.context)
        t2.setStackTrace(t.getStackTrace)
        throw t2
    }

  @inline private[this] def require(condition: Boolean, orElseThrow: => TypeError): Unit =
    if (!condition) throw TypeInferenceFailure(orElseThrow, Nil)

  @inline private[this] def require(condition: Boolean, orElseThrow: => TypeError, context: => TypeErrorContext): Unit =
    if (!condition) throw TypeInferenceFailure(orElseThrow, context :: Nil)
}
