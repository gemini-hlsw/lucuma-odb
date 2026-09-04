// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package flamingos2
package spectroscopy

import cats.Monad
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState.Disabled
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig as F2
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

/**
 * Telluric standard sequence for a Flamingos 2 MOS observation.
 *
 * The standard is a single star, so it is observed through the builtin long slit
 * whose aperture matches the mask's rather than through the mask itself.
 * Thus the calibration observation is essentially a long slit one.
 *
 * The star is stepped along that slit following the mode's telescope configs, and
 * an arc taken at the last position closes the sequence.
 *
 * This is not exactly how a long slit observation's own telluric is generated. That
 * one keeps the ABBA cadence and its interleaved calibrations.
 */
object MosTelluric:

  val ScienceAtomTitle: NonEmptyString =
    NonEmptyString.unsafeFrom("Telluric")

  val ArcAtomTitle: NonEmptyString =
    NonEmptyString.unsafeFrom("Nighttime Calibrations")

  case class StepDefinition(
    science: NonEmptyList[ProtoStep[F2]],
    arc:     NonEmptyList[ProtoStep[F2]]
  ):
    def cycleCount(t: IntegrationTime): Either[String, NonNegInt] =
      calculateCycleCount[F2](s => s.telescopeConfig.guiding.isGuided, science.toList, t)

  private object StepComputer extends SequenceState[F2] with Flamingos2InitialDynamicConfig:

    /**
     * Builds the science steps and the still unexpanded SmartGcal arc.  The
     * decker comes from the equivalent long slit rather than from the config,
     * whose MOS decker would vignette the slit.
     */
    def compute(
      config:  Config,
      time:    IntegrationTime,
      calRole: Option[CalibrationRole]
    ): (NonEmptyList[ProtoStep[F2]], ProtoStep[F2]) =

      val readMode =
        config
          .explicitReadMode
          .getOrElse(Flamingos2ReadMode.forExposureTime(time.exposureTime))

      val mask = Flamingos2FpuMask.builtin(config.gcalFpu)

      eval:
        for
          _  <- F2.exposure    := time.exposureTime
          _  <- F2.disperser   := config.disperser.some
          _  <- F2.filter      := config.filter
          _  <- F2.readMode    := readMode
          _  <- F2.lyotWheel   := Flamingos2LyotWheel.F16
          _  <- F2.fpu         := mask
          _  <- F2.decker      := mask.defaultDecker
          _  <- F2.readoutMode := config.readoutMode
          _  <- F2.reads       := config.explicitReads.getOrElse(readMode.readCount)
          ss <- config.telescopeConfigs.traverse(scienceStep(_, calRole.sciClass))
          r  <- arcStep(ss.last.telescopeConfig.copy(guiding = Disabled), calRole.gcalClass)
        yield (ss, r)

  private def expand[F[_]: Monad](
    static:   Flamingos2StaticConfig,
    expander: SmartGcalExpander[F, Flamingos2StaticConfig, F2],
    science:  NonEmptyList[ProtoStep[F2]],
    arc:      ProtoStep[F2]
  ): EitherT[F, String, StepDefinition] =
    EitherT(expander.expandStep(static, arc))
      .map(as => StepDefinition(science, as.map(adjustReadMode)))

  private case class Generator(
    steps:      StepDefinition,
    builder:    AtomBuilder[F2],
    goalCycles: NonNegInt
  ) extends SequenceGenerator[F2]:

    override def generate: Stream[Pure, Atom[F2]] =
      builder.buildStream:
        Stream.emits:
          List.fill(1.max(goalCycles.value))(ProtoAtom(ScienceAtomTitle.some, steps.science)) :+
            ProtoAtom(ArcAtomTitle.some, steps.arc)

  def instantiate[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[Flamingos2StaticConfig, F2],
    static:        Flamingos2StaticConfig,
    namespace:     UUID,
    expander:      SmartGcalExpander[F, Flamingos2StaticConfig, F2],
    modeName:      String,
    config:        Config,
    time:          Either[OdbError, IntegrationTime],
    calRole:       Option[CalibrationRole]
  ): F[Either[OdbError, SequenceGenerator[F2]]] =
    (for
       t         <- EitherT.fromEither[F]:
                      time.filterOrElse(
                        _.exposureTime.toNonNegMicroseconds.value > 0,
                        zeroExposureTime(observationId, modeName)
                      )
       (ss, arc)  = StepComputer.compute(config, t, calRole)
       d         <- expand(static, expander, ss, arc).leftMap(definitionError(observationId, _))
       c         <- EitherT.fromEither[F](d.cycleCount(t).leftMap(definitionError(observationId, _)))
     yield Generator(d, AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science), c): SequenceGenerator[F2]).value
