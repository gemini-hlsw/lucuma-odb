// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.order.*
import eu.timepit.refined.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ObserveClass
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.int.*
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.optics.syntax.lens.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.TargetIntegrationTime
import lucuma.odb.sequence.data.AcqExposureTime
import lucuma.odb.sequence.data.ProtoStep
import lucuma.refined.*

sealed trait Acquisition[D, G, F, U] extends SequenceState[D] {

  def optics: DynamicOptics[D, G, F, U]

  def wavelength(f: F): Wavelength

  def compute(
    acqFilters:   NonEmptyList[F],
    fpu:          U,
    exposureTime: AcqExposureTime,
    λ:            Wavelength
  ): Acquisition.Steps[D] = {

    def filter: F = Acquisition.filter(acqFilters, λ, wavelength)

    // Last step, max 360s
    // https://app.shortcut.com/lucuma/story/1999/determine-exposure-time-for-acquisition-images#activity-2516
    def lastExpTime(exposureTime: AcqExposureTime): TimeSpan = {
      val base = (exposureTime * NonNegInt.unsafeFrom(3)).timeSpan
      if (base > Acquisition.MaxExpTimeLastStep)
        Acquisition.MaxExpTimeLastStep
      else base
    }

    eval {
      for {
        _  <- optics.exposure      := exposureTime.timeSpan
        _  <- optics.filter        := filter.some
        _  <- optics.fpu           := none[GmosFpuMask[U]]
        _  <- optics.grating       := none[(G, GmosGratingOrder, Wavelength)]
        _  <- optics.xBin          := GmosXBinning.Two
        _  <- optics.yBin          := GmosYBinning.Two
        _  <- optics.roi           := GmosRoi.Ccd2
        s0 <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)

        _  <- optics.exposure      := 20.secondTimeSpan
        _  <- optics.fpu           := GmosFpuMask.Builtin(fpu).some
        _  <- optics.xBin          := GmosXBinning.One
        _  <- optics.yBin          := GmosYBinning.One
        _  <- optics.roi           := GmosRoi.CentralStamp
        s1 <- scienceStep(10.arcsec, 0.arcsec, ObserveClass.Acquisition)

        _  <- optics.exposure      := lastExpTime(exposureTime)
        s2 <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)

      } yield Acquisition.Steps(s0, s1, s2)
    }
  }

}

object Acquisition {
  val AcquisitionSN: SignalToNoise =
    SignalToNoise.FromBigDecimalExact.getOption(10).get

  val DefaultIntegrationTime: TargetIntegrationTime =
    TargetIntegrationTime(
      Zipper.one(IntegrationTime(TimeSpan.fromSeconds(1).get, 1.refined, AcquisitionSN)),
      Band.R // Band is meaningless here, but we need to provide one
    )

  val MinExposureTime    = TimeSpan.fromSeconds(1).get
  val MaxExposureTime    = TimeSpan.fromSeconds(180).get
  val MaxExpTimeLastStep = TimeSpan.fromSeconds(360).get

  def filter[F](acqFilters: NonEmptyList[F], λ: Wavelength, wavelength: F => Wavelength): F =
    acqFilters.toList.minBy { f => λ.diff(wavelength(f)).abs }

  /**
   * Unique step configurations used to form an acquisition sequence.
   *
   * @param ccd2 image, 2x2 using CCD2 ROI
   * @param p10  20 second exposure, 1x1 Central Stamp, 10 arcsec offset in p
   * @param slit image through the slit
   */
  final case class Steps[D](
    ccd2: ProtoStep[D],
    p10:  ProtoStep[D],
    slit: ProtoStep[D]
  ) {

    val initialAtom: NonEmptyList[ProtoStep[D]] =
      NonEmptyList.of(ccd2, p10, slit)

    val repeatingAtom: NonEmptyList[ProtoStep[D]] =
      NonEmptyList.of(slit)

  }

  object GmosNorth extends GmosNorthInitialDynamicConfig
                      with Acquisition[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] {

    override def optics: DynamicOptics[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
      DynamicOptics.North

    override def wavelength(f: GmosNorthFilter): Wavelength =
      f.wavelength

  }

  object GmosSouth extends GmosSouthInitialDynamicConfig
                      with Acquisition[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] {

    override def optics: DynamicOptics[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] =
      DynamicOptics.South

    override def wavelength(f: GmosSouthFilter): Wavelength =
      f.wavelength

  }
}
