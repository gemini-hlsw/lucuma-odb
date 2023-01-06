// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data
package gmos
package longslit

import cats.data.NonEmptyList
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.option._
import coulomb.Quantity
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosDouble
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
import lucuma.core.math.syntax.int._
import lucuma.core.math.units.Nanometer
import lucuma.core.model.SourceProfile
import lucuma.core.model.sequence.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.GmosFpuMask
import lucuma.core.optics.syntax.lens._
import lucuma.core.optics.syntax.optional._
import lucuma.odb.graphql.input.GmosLongSlitInput

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
    val p0   = Offset.P(0.arcsec)
    val Δλs  = mode.wavelengthDithers
    val qs   = mode.spatialOffsets
    val xBin = mode.xBin(sourceProfile, imageQuality, sampling)

    def nextAtom(index: Int, d: D): Science.Atom[D] = {
      val Δ = Δλs(index % Δλs.length)
      val q = qs(index % qs.length)

      (for {
        _ <- optics.wavelength := λ.offset(Δ).getOrElse(λ)
        s <- scienceStep(Offset(p0, q))
        f <- flatStep
      } yield Science.Atom(index, s, f)).runA(d).value
    }

    val init: D =
      (for {
        _ <- optics.exposure    := exposureTime.duration
        _ <- optics.grating     := (mode.grating, GmosGratingOrder.One, λ).some
        _ <- optics.filter      := mode.filter
        _ <- optics.fpu         := GmosFpuMask.builtin.reverseGet(mode.fpu).some

        _ <- optics.xBin        := xBin
        _ <- optics.yBin        := mode.yBin
        _ <- optics.ampReadMode := mode.ampReadMode
        _ <- optics.ampGain     := mode.ampGain

        _ <- optics.roi         := mode.roi
      } yield ()).runS(initialConfig).value

    LazyList.unfold((0, init)) { case (i, d) =>
      val a = nextAtom(i, d)
      Some((a, (i+1, a.science.instrumentConfig)))
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
