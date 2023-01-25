// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import coulomb.Quantity
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.numeric.PosDouble
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.ImageQuality
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.math.syntax.int.*
import lucuma.core.math.units.Nanometer
import lucuma.core.model.SourceProfile
import lucuma.core.model.sequence.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.GmosFpuMask
import lucuma.core.optics.syntax.lens.*
import lucuma.core.optics.syntax.optional.*
import lucuma.odb.sequence.SequenceState
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.SciExposureTime

import scala.collection.immutable.LazyList

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
    mode:          GmosLongSlitConfig[G, F, U],
    exposureTime:  SciExposureTime,
    sourceProfile: SourceProfile,
    imageQuality:  ImageQuality,
    sampling:      PosDouble
  ): LazyList[Science.Atom[D]] = {

    val λ    = mode.centralWavelength
    val p0   = Offset.P.Zero
    val Δλs  = LazyList.continually(mode.wavelengthDithers match {
      case Nil => LazyList(WavelengthDither.Zero)
      case ws  => ws.to(LazyList)
    }).flatten
    val qs   = LazyList.continually(mode.spatialOffsets match {
      case Nil => LazyList(Offset.Q.Zero)
      case os  => os.to(LazyList)
    }).flatten
    val xBin = mode.xBin(sourceProfile, imageQuality, sampling)

    def nextAtom(index: Int, Δ: WavelengthDither, q: Offset.Q, d: D): Science.Atom[D] =
      (for {
        _ <- optics.wavelength := λ.offset(Δ).getOrElse(λ)
        s <- scienceStep(Offset(p0, q))
        f <- flatStep
      } yield Science.Atom(index, s, f)).runA(d).value

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

    LazyList.unfold((0, Δλs, qs, init)) { case (i, wds, sos, d) =>
      val a = nextAtom(i, wds.head, sos.head, d)
      Some((a, (i+1, wds.tail, sos.tail, a.science.instrumentConfig)))
    }
  }

}

object Science {

  /**
   * Science and associated matching flat.
   */
  final case class Atom[D](
    index:   Int,
    science: ProtoStep[D],
    flat:    ProtoStep[D]
  ) {

    def steps: NonEmptyList[ProtoStep[D]] =
      if ((index % 2) === 0) NonEmptyList.of(science, flat)
      else NonEmptyList.of(flat, science)

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
