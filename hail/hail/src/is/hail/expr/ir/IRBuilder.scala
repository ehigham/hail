package is.hail.expr.ir

import is.hail.expr.ir.defs.{Let, Ref, TrivialIR}
import is.hail.utils.BoxedArrayBuilder

object IRBuilder {
  def scoped(f: IRBuilder => IR): IR = {
    val builder = new IRBuilder()
    val result = f(builder)
    Let(builder.getBindings, result)
  }
}

class IRBuilder {
  private val bindings: BoxedArrayBuilder[(Name, IR)] =
    new BoxedArrayBuilder[(Name, IR)]()

  def getBindings: IndexedSeq[(Name, IR)] = bindings.result()

  def memoize(ir: IR): TrivialIR = ir match {
    case ir: TrivialIR => ir
    case _ => strictMemoize(ir)
  }

  def strictMemoize(ir: IR): Ref =
    bind(freshName(), ir)

  def bind(name: Name, value: IR): Ref = {
    bindings += name -> value
    Ref(name, value.typ)
  }

  def bind(name: String, value: IR): Ref =
    bind(Name(name), value)
}
