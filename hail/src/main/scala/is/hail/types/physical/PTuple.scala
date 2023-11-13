package is.hail.types.physical

import is.hail.types.virtual.{TTuple, TupleField}
import is.hail.utils.toRichIndexedSeq

case class PTupleField(index: Int, typ: PType)

trait PTuple extends PBaseStruct {
  val _types: IndexedSeq[PTupleField]
  val fieldIndex: Map[Int, Int]

  lazy val virtualType: TTuple = TTuple(_types.fmap(tf => TupleField(tf.index, tf.typ.virtualType)))

  lazy val fields: IndexedSeq[PField] = _types.zipWithIndex.fmap { case (PTupleField(tidx, t), i) => PField(s"$tidx", t, i) }
  lazy val nFields: Int = fields.size

  def identBase: String = "tuple"
}
