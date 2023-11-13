package is.hail.expr.ir

import is.hail.utils.toRichIndexedSeq

object FreeVariables {
  def apply(ir: IR, supportsAgg: Boolean, supportsScan: Boolean): BindingEnv[Unit] = {

    def compute(ir1: IR, baseEnv: BindingEnv[Unit]): BindingEnv[Unit] = {
      ir1 match {
        case Ref(name, _) =>
          baseEnv.bindEval(name, ())
        case TableAggregate(_, _) => baseEnv
        case MatrixAggregate(_, _) => baseEnv
        case StreamAggScan(a, name, query) =>
          val aE = compute(a, baseEnv)
          val qE = compute(query, baseEnv.copy(scan = Some(Env.empty)))
          aE.merge(qE.copy(eval = qE.eval.bindIterable(qE.scan.get.m - name), scan = baseEnv.scan))
        case StreamAgg(a, name, query) =>
          val aE = compute(a, baseEnv)
          val qE = compute(query, baseEnv.copy(agg = Some(Env.empty)))
          aE.merge(qE.copy(eval = qE.eval.bindIterable(qE.agg.get.m - name), agg = baseEnv.agg))
        case ApplyAggOp(init, seq, sig) =>
          val initEnv = baseEnv.copy(agg = None)
          val initFreeVars = init
            .foldLeft(initEnv) { case (env, x) => env.merge(compute(x, initEnv)) }
            .copy(agg = Some(Env.empty[Unit]))
          val seqEnv = baseEnv.promoteAgg
          seq.foldLeft(initFreeVars) { case (fvs, x) =>
            val e = compute(x, seqEnv)
            fvs.merge(e.copy(eval = Env.empty[Unit], agg = Some(e.eval)))
          }
        case ApplyScanOp(init, seq, sig) =>
          val initEnv = baseEnv.copy(scan = None)
          val initFreeVars = init
            .foldLeft(initEnv) { case (env, x) => env.merge(compute(x, initEnv)) }
            .copy(agg = Some(Env.empty[Unit]))
          val seqEnv = baseEnv.promoteScan
          seq.foldLeft(initFreeVars) { case (fvs, x) =>
            val e = compute(x, seqEnv)
            fvs.merge(e.copy(eval = Env.empty[Unit], agg = Some(e.eval)))
          }
        case AggFold(zero, seqOp, combOp, accumName, otherAccumName, isScan) =>
          val zeroEnv = if (isScan) baseEnv.copy(scan = None) else baseEnv.copy(agg = None)
          val zeroFreeVarsCompute = compute(zero, zeroEnv)
          val zeroFreeVars = if (isScan) zeroFreeVarsCompute.copy(scan = Some(Env.empty[Unit])) else zeroFreeVarsCompute.copy(agg = Some(Env.empty[Unit]))
          val seqOpEnv = if (isScan) baseEnv.promoteScan else baseEnv.promoteAgg
          val seqOpFreeVarsCompute = compute(seqOp, seqOpEnv)
          val seqOpFreeVars = if (isScan) {
            seqOpFreeVarsCompute.copy(eval = Env.empty[Unit], scan = Some(seqOpFreeVarsCompute.eval))
          } else {
            seqOpFreeVarsCompute.copy(eval = Env.empty[Unit], agg = Some(seqOpFreeVarsCompute.eval))
          }
          val combEval = Env.fromSeq(IndexedSeq((accumName, {}), (otherAccumName, {})))
          val combOpFreeVarsCompute = compute(combOp, baseEnv.copy(eval=combEval))
          val combOpFreeVars = combOpFreeVarsCompute.copy(eval = Env.empty[Unit], scan = Some(combOpFreeVarsCompute.eval))
          zeroFreeVars.merge(seqOpFreeVars).merge(combOpFreeVars)
        case _ =>
          ir1.children
            .zipWithIndex
            .foldLeft(baseEnv) {
              case (env, (child: IR, i)) =>
                val childEnv = ChildEnvWithoutBindings(ir1, i, baseEnv)
                val sub = compute(child, childEnv)
                  .subtract(NewBindings(ir1, i, childEnv))
                env.merge(
                  if (UsesAggEnv(ir1, i)) sub.copy(eval = Env.empty[Unit], agg = Some(sub.eval), scan = baseEnv.scan)
                  else if (UsesScanEnv(ir1, i)) sub.copy(eval = Env.empty[Unit], agg = baseEnv.agg, scan = Some(sub.eval))
                  else sub
                )
              case (env, _) =>
                env
            }
      }
    }

    compute(ir, BindingEnv(Env.empty,
      if (supportsAgg) Some(Env.empty[Unit]) else None,
      if (supportsScan) Some(Env.empty[Unit]) else None))
  }
}
