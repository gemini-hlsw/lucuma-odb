// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import cats.data.NonEmptyList
import cats.syntax.eq.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosDouble
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.ObserveClass
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.SourceProfile
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.optics.syntax.lens.*
import lucuma.core.optics.syntax.optional.*
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.SciExposureTime

import scala.annotation.tailrec

/**
 * GMOS long slit science atoms
 *
 * @tparam D dynamic config type
 * @tparam G grating type
 * @tparam F filter type
 * @tparam U FPU type
 */
sealed trait Science[D, G, F, U] extends SequenceState[D] {

  def optics: DynamicOptics[D, G, F, U]

  def compute(
    mode:          Config[G, F, U],
    exposureTime:  SciExposureTime,
    sourceProfile: SourceProfile,
    imageQuality:  ImageQuality,
    sampling:      PosDouble
  ): Stream[Pure, Science.Atom[D]] = {

    @tailrec def gcd(a: BigInt, b: BigInt): BigInt = if (b === 0) a else gcd(b, a%b)
    def lcm(as: BigInt*): BigInt = as.reduce { (a, b) => a/gcd(a,b)*b }

    // How many potentially unique configs will be generated.
    val uniqueConfigCount = lcm(
      2, // to account for alternation between (science, flat), (flat, science)
      math.max(mode.wavelengthDithers.size, 1),
      math.max(mode.spatialOffsets.size, 1)
    )

    val λ    = mode.centralWavelength
    val p0   = Offset.P.Zero
    val Δλs  = mode.wavelengthDithers match {
      case Nil => Stream(WavelengthDither.Zero).repeat
      case ws  => Stream.emits(ws).repeat
    }
    val qs   = mode.spatialOffsets match {
      case Nil => Stream(Offset.Q.Zero).repeat
      case os  => Stream.emits(os).repeat
    }
    val xBin = mode.xBin(sourceProfile, imageQuality, sampling)

    def nextAtom(stepOrder: Science.StepOrder, Δ: WavelengthDither, q: Offset.Q, d: D): Science.Atom[D] =
      (for {
        w <- optics.wavelength := λ.offset(Δ).getOrElse(λ)
        s <- scienceStep(Offset(p0, q), ObserveClass.Science)
        f <- flatStep(ObserveClass.PartnerCal)
        label = f"q ${Angle.signedDecimalArcseconds.get(q.toAngle)}%.1f″, λ ${Wavelength.decimalNanometers.reverseGet(w.getOrElse(λ))}%.1f nm"
      } yield Science.Atom(NonEmptyString.unsafeFrom(label), stepOrder, s, f)).runA(d).value

    val init: D =
      (for {
        _ <- optics.exposure    := exposureTime.timeSpan
        _ <- optics.grating     := (mode.grating, GmosGratingOrder.One, λ).some
        _ <- optics.filter      := mode.filter
        _ <- optics.fpu         := GmosFpuMask.builtin.reverseGet(mode.fpu).some

        _ <- optics.xBin        := xBin
        _ <- optics.yBin        := mode.yBin
        _ <- optics.ampReadMode := mode.ampReadMode
        _ <- optics.ampGain     := mode.ampGain

        _ <- optics.roi         := mode.roi
      } yield ()).runS(initialConfig).value

    val seq =
     Stream.unfold((Science.StepOrder.ScienceThenFlat, Δλs, qs, init)) { case (o, wds, sos, d) =>
       // TODO: head.toList.head ? there's got to be a better way
       val a = nextAtom(o, wds.head.toList.head, sos.head.toList.head, d)
       Some((a, (o.next, wds.tail, sos.tail, a.science.value)))
     }

    // If the number of unique configs is reasonably small (as it almost always
    // should be), pre-generate them and then just repeat.  Otherwise, we'll
    // just (re)generate steps as we go.
    if (uniqueConfigCount > 100) seq
    else Stream.emits(seq.take(uniqueConfigCount.toLong).toList).repeat

  }

}

object Science {

  enum StepOrder:
    def next: StepOrder =
      this match {
        case ScienceThenFlat => FlatThenScience
        case FlatThenScience => ScienceThenFlat
      }
    case ScienceThenFlat, FlatThenScience

  /**
   * Science and associated matching flat.
   */
  final case class Atom[D](
    description: NonEmptyString,
    stepOrder:   StepOrder,
    science:     ProtoStep[D],
    flat:        ProtoStep[D]
  ) {

    def steps: NonEmptyList[ProtoStep[D]] =
      stepOrder match {
        case StepOrder.ScienceThenFlat => NonEmptyList.of(science, flat)
        case StepOrder.FlatThenScience => NonEmptyList.of(flat, science)
      }

  }

  object GmosNorth extends GmosNorthInitialDynamicConfig
                      with Science[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] {

    override def optics: DynamicOptics[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
      DynamicOptics.North

  }

  object GmosSouth extends GmosSouthInitialDynamicConfig
                      with Science[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] {

    override def optics: DynamicOptics[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] =
      DynamicOptics.South

  }

}
