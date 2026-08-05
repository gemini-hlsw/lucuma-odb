// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package mos

import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.order.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosMosAcquisitionType
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
import lucuma.core.enums.SequenceType
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.int.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.gmos.spectroscopy.AcquisitionAtoms
import lucuma.odb.sequence.gmos.spectroscopy.AcquisitionSteps
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

/**
 * GMOS MOS acquisition sequence generation.
 *
 * Unlike long slit, a MOS acquisition does not run an ITC pass: its exposure
 * time is read verbatim from a Time & Count acquisition exposure time mode, and
 * the count sets the number of through-mask images.  See ADR 0002 for the
 * reasoning and the accepted trade-offs.
 *
 * The shape is chosen by the observation's [[acquisitionType]]:
 *
 *  - `MaskIn` (default): one through-mask step.
 *  - `MaskOut`: an unmasked field step followed by a through-mask step.
 *
 * Either way the initial atom carries the breakpoint on its last (through-mask)
 * step, followed by `count - 1` copies of a single through-mask atom.  Both
 * steps are Full Frame (slitlets span the whole field), take the acquisition
 * filter and the stated exposure time unmodified, and carry no grating and no
 * offset.  The unmasked step is 2x2 with no FPU; the through-mask step is 1x1
 * through the Custom Mask.
 */
object Acquisition:
  val FastReadModeLimit: TimeSpan = 60.secTimeSpan

  private val ModeName: String =
    "GMOS MOS"

  /**
   * The two step configurations a MOS acquisition uses.
   *
   * @param field the unmasked field step, present only for `MaskOut`
   * @param mask  the through-mask step
   */
  case class Steps[D](
    field: Option[ProtoStep[D]],
    mask:  ProtoStep[D]
  ):
    /** The step grouping, without the breakpoint (placed by `AcquisitionAtoms`). */
    def acquisitionSteps: AcquisitionSteps[D] =
      field match
        case None    => AcquisitionSteps(NonEmptyList.of(mask), NonEmptyList.of(mask))
        case Some(f) => AcquisitionSteps(NonEmptyList.of(f, mask), NonEmptyList.of(mask))

  private sealed trait StepComputer[D, G, L, U] extends GmosSequenceState[D, G, L, U]:

    def compute(
      acqConfig:      AcquisitionConfig[L],
      customMask:     GmosFpuMask.Custom,
      acquisitionType: GmosMosAcquisitionType,
      exposureTime:   TimeSpan
    ): Acquisition.Steps[D] =
      val readMode = if exposureTime <= FastReadModeLimit then GmosAmpReadMode.Fast else GmosAmpReadMode.Slow
      val filter   = acqConfig.filter

      eval:
        // The unmasked field step, present only when imaging the field inline.
        def fieldStep =
          for
            _  <- optics.exposure    := exposureTime
            _  <- optics.filter      := filter.some
            _  <- optics.fpu         := none[GmosFpuMask[U]]
            _  <- optics.grating     := none[(G, GmosGratingOrder, Wavelength)]
            _  <- optics.xBin        := GmosXBinning.Two
            _  <- optics.yBin        := GmosYBinning.Two
            _  <- optics.ampReadMode := readMode
            _  <- optics.roi         := GmosRoi.FullFrame
            s  <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)
          yield s

        // The through-mask step.
        def maskStep =
          for
            _  <- optics.exposure    := exposureTime
            _  <- optics.filter      := filter.some
            _  <- optics.fpu         := customMask.some
            _  <- optics.grating     := none[(G, GmosGratingOrder, Wavelength)]
            _  <- optics.xBin        := GmosXBinning.One
            _  <- optics.yBin        := GmosYBinning.One
            _  <- optics.ampReadMode := readMode
            _  <- optics.roi         := GmosRoi.FullFrame
            s  <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)
          yield s

        for
          f  <- acquisitionType match
                  case GmosMosAcquisitionType.MaskOut => fieldStep.map(_.some)
                  case GmosMosAcquisitionType.MaskIn  => State.pure[D, Option[ProtoStep[D]]](none)
          m  <- maskStep
        yield Acquisition.Steps(f, m)

    end compute
  end StepComputer

  private object StepComputer:

    object North extends GmosNorthSequenceState
                    with StepComputer[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]

    object South extends GmosSouthSequenceState
                    with StepComputer[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]

  end StepComputer

  // The acquisition ETM must be Time & Count: there is no ITC pass to solve a
  // signal-to-noise one.  This is a defensive read; the invariant is enforced
  // upstream in the consistency check and the MOS input.
  private def timeAndCount(
    oid:  Observation.Id,
    etm:  ExposureTimeMode
  ): Either[OdbError, (TimeSpan, PosInt)] =
    etm match
      case ExposureTimeMode.TimeAndCountMode(t, c, _) => (t, c).asRight
      case _                                          =>
        OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $ModeName acquisition requires a Time & Count exposure time mode.".some).asLeft

  def gmosNorth(
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[StaticConfig.GmosNorth, GmosNorth],
    static:        StaticConfig.GmosNorth,
    namespace:     UUID,
    config:        Config.GmosNorth,
    calRole:       Option[CalibrationRole]
  ): Either[OdbError, SequenceGenerator[GmosNorth]] =
    timeAndCount(observationId, config.acquisition.exposureTimeMode).flatMap: (time, count) =>
      AcquisitionAtoms.instantiate(
        observationId,
        IntegrationTime(time, count).asRight,
        calRole,
        AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Acquisition),
        ModeName,
        t => StepComputer.North.compute(config.acquisition, config.customMask, config.acquisitionType, t).acquisitionSteps,
        math.max(0, count.value - 1)
      )

  def gmosSouth(
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[StaticConfig.GmosSouth, GmosSouth],
    static:        StaticConfig.GmosSouth,
    namespace:     UUID,
    config:        Config.GmosSouth,
    calRole:       Option[CalibrationRole]
  ): Either[OdbError, SequenceGenerator[GmosSouth]] =
    timeAndCount(observationId, config.acquisition.exposureTimeMode).flatMap: (time, count) =>
      AcquisitionAtoms.instantiate(
        observationId,
        IntegrationTime(time, count).asRight,
        calRole,
        AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Acquisition),
        ModeName,
        t => StepComputer.South.compute(config.acquisition, config.customMask, config.acquisitionType, t).acquisitionSteps,
        math.max(0, count.value - 1)
      )

end Acquisition
