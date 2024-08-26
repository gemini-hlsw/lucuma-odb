// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import cats.syntax.option.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.ObserveClass
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.optics.syntax.lens.*
import lucuma.core.optics.syntax.optional.*
import lucuma.odb.sequence.data.SciExposureTime

// TODO: delete me

/**
 * GMOS long slit science atoms
 *
 * @tparam D dynamic config type
 * @tparam G grating type
 * @tparam L filter type
 * @tparam U FPU type
 */
trait ScienceAtomSequenceState[D, G, L, U] extends SequenceState[D] {

  def optics: DynamicOptics[D, G, L, U]

  // TODO: delete me
  def stream(
    mode:         Config[G, L, U],
    exposureTime: SciExposureTime
  ): Stream[Pure, ScienceAtom[D]]

  // TODO: delete me
  def unfold(
    mode: Config[G, L, U],
    time: SciExposureTime,
    Δλs:  Stream[Pure, WavelengthDither],
    qs:   Stream[Pure, Offset.Q]
  ): Stream[Pure, ScienceAtom[D]] = {
    val λ = mode.centralWavelength

    val init = (for {
      _ <- optics.exposure    := time.timeSpan
      _ <- optics.grating     := (mode.grating, GmosGratingOrder.One, λ).some
      _ <- optics.filter      := mode.filter
      _ <- optics.fpu         := GmosFpuMask.builtin.reverseGet(mode.fpu).some

      _ <- optics.xBin        := mode.xBin
      _ <- optics.yBin        := mode.yBin
      _ <- optics.ampReadMode := mode.ampReadMode
      _ <- optics.ampGain     := mode.ampGain

      _ <- optics.roi         := mode.roi
    } yield ()).runS(initialConfig).value

    def nextAtom(o: StepOrder, Δ: WavelengthDither, q: Offset.Q, d: D): ScienceAtom[D] =
      (for {
        w <- optics.wavelength := λ.offset(Δ).getOrElse(λ)
        s <- scienceStep(Offset(Offset.P.Zero, q), ObserveClass.Science)
        f <- flatStep(ObserveClass.PartnerCal)
        label = f"q ${Angle.signedDecimalArcseconds.get(q.toAngle)}%.1f″, λ ${Wavelength.decimalNanometers.reverseGet(w.getOrElse(λ))}%.1f nm"
      } yield ScienceAtom(NonEmptyString.unsafeFrom(label), o, s, f)).runA(d).value

    Stream.unfold((StepOrder.ScienceThenFlat, Δλs, qs, init)) { case (o, wds, sos, d) =>
      // TODO: head.toList.head ? there's got to be a better way
      val a = nextAtom(o, wds.head.toList.head, sos.head.toList.head, d)
      Some((a, (o.next, wds.tail, sos.tail, a.science.value)))
    }
  }
}