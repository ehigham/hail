package is.hail.types.virtual

import is.hail.annotations.{Annotation, ExtendedOrdering}
import is.hail.backend.HailStateManager
import is.hail.check.Gen
import is.hail.utils.toRichIndexedSeq
import org.json4s.jackson.JsonMethods

import scala.collection.mutable
import scala.reflect.{ClassTag, classTag}

final case class TArray(elementType: Type) extends TContainer {
  override def pyString(sb: StringBuilder): Unit = {
    sb.append("array<")
    elementType.pyString(sb)
    sb.append('>')
  }

  def _toPretty = s"Array[$elementType]"

  override def canCompare(other: Type): Boolean = other match {
    case TArray(otherType) => elementType.canCompare(otherType)
    case _ => false
  }

  override def unify(concrete: Type): Boolean = concrete match {
    case TArray(celementType) => elementType.unify(celementType)
    case _ => false
  }

  override def subst() = TArray(elementType.subst())

  override def _pretty(sb: StringBuilder, indent: Int, compact: Boolean = false) {
    sb.append("Array[")
    elementType.pretty(sb, indent, compact)
    sb.append("]")
  }

  def _typeCheck(a: Any): Boolean =
    a match {
      case as: IndexedSeq[_] =>
        as.forall(elementType.typeCheck)
      case as: mutable.WrappedArray[_] =>
        as.forall(elementType.typeCheck)
      case _ =>
        false
    }

  override def _showStr(a: Annotation): String =
    a.asInstanceOf[IndexedSeq[Annotation]]
      .fmap(elt => elementType.showStr(elt))
      .mkString("[", ",", "]")

  override def str(a: Annotation): String = JsonMethods.compact(toJSON(a))

  override def genNonmissingValue(sm: HailStateManager): Gen[IndexedSeq[Annotation]] =
    Gen.buildableOf[Array](elementType.genValue(sm)).map(x => x: IndexedSeq[Annotation])

  def mkOrdering(sm: HailStateManager, missingEqual: Boolean): ExtendedOrdering =
    ExtendedOrdering.iterableOrdering(elementType.ordering(sm), missingEqual)

  override def scalaClassTag: ClassTag[IndexedSeq[AnyRef]] = classTag[IndexedSeq[AnyRef]]

  override def valueSubsetter(subtype: Type): Any => Any = {
    if (this == subtype)
      return identity

    val subsetElem = elementType.valueSubsetter(subtype.asInstanceOf[TArray].elementType)
    (a: Any) => a.asInstanceOf[IndexedSeq[Any]].fmap(subsetElem)
  }

  override def arrayElementsRepr: TArray = this
}
