// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package flamingos2
package spectroscopy

import cats.Monad
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.order.*
import eu.timepit.refined.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState.Disabled
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.StepViolation
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig as F2
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.*
import lucuma.odb.sequence.SmartGcalExpander.Calibration
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.AtomBuilder
import lucuma.odb.sequence.util.DaytimeSmartGcal
import lucuma.odb.sequence.util.definitionError
import lucuma.refined.*

import java.util.UUID

/**
 * Flamingos 2 spectroscopy science sequence generation, shared by the long slit
 * and MOS modes.
 *
 * The two modes differ only in the aperture their steps carry.  The sequence
 * opens with a single "Nighttime Calibrations" atom, however long the science
 * runs: the observer takes any further sets.  A science cycle must be shorter
 * than the calibration set interval, which is always the short one since
 * Flamingos 2 never reaches the long wavelength cutoff.
 */
object Science:

  /**
   * The name of the ABBA cycle atoms.
   */
  val AbbaCycleTitle: NonEmptyString = "ABBA Cycle".refined

  val MosMaskFlatTitle: NonEmptyString = "MOS Mask Flat".refined

  case class StepDefinition(
    a0:   ProtoStep[F2],
    b0:   ProtoStep[F2],
    b1:   ProtoStep[F2],
    a1:   ProtoStep[F2],
    cals: List[ProtoStep[F2]]
  ):
    /** ABBA science cycle. */
    def abbaCycle: NonEmptyList[ProtoStep[F2]] =
      NonEmptyList.of(a0, b0, b1, a1)

    def cycleCount(t: IntegrationTime): Either[String, NonNegInt] =
      calculateCycleCount[F2](s => s.telescopeConfig.guiding.isGuided, abbaCycle.toList, t)

  object StepDefinition extends SequenceState[F2] with Flamingos2InitialDynamicConfig:

    def f2ScienceStep(tc: TelescopeConfig, obsClass: ObserveClass): State[F2, ProtoStep[F2]] =
      scienceStep(tc, obsClass)

    // PreDef is a StepDefinition before SmartGcal expansion.
    case class PreDef(
      a0:   ProtoStep[F2],
      b0:   ProtoStep[F2],
      b1:   ProtoStep[F2],
      a1:   ProtoStep[F2],
      flat: ProtoStep[F2],  // Unexpanded SmartGcal Flat
      arc:  ProtoStep[F2]   // Unexpanded SmartGcal Arc
    ):
      def expand[F[_]: Monad](
        static:   Flamingos2StaticConfig,
        expander: SmartGcalExpander[F, Flamingos2StaticConfig, F2]
      ): F[(List[StepViolation], StepDefinition)] =
        expander.expandFlatAndArc(static, flat, arc).map: (missing, cs) =>
          (missing, StepDefinition(a0, b0, b1, a1, cs))

    object PreDef:

      def apply(
         config:   Config,
         time:     IntegrationTime,
         readMode: Flamingos2ReadMode,
         a0Off:    TelescopeConfig,
         b0Off:   TelescopeConfig,
         b1Off:   TelescopeConfig,
         a1Off:   TelescopeConfig,
         calRole: Option[CalibrationRole]
      ): PreDef =

        // The explicit read mode if set, else the ITC's (its choice in S/N mode, the
        // requested one in time-and-count mode).
        val resolvedReadMode = config.explicitReadMode.getOrElse(readMode)

        val sciClass = calRole.sciClass

        eval:
          for
            _  <- F2.exposure    := time.exposureTime
            _  <- F2.disperser   := config.disperser.some
            _  <- F2.filter      := config.filter
            _  <- F2.readMode    := resolvedReadMode
            _  <- F2.lyotWheel   := Flamingos2LyotWheel.F16
            _  <- F2.fpu         := config.fpuMask
            _  <- F2.decker      := config.decker
            _  <- F2.readoutMode := config.readoutMode
            _  <- F2.reads       := config.explicitReads.getOrElse(resolvedReadMode.readCount)
            a0 <- f2ScienceStep(a0Off, sciClass)
            b0 <- f2ScienceStep(b0Off, sciClass)
            b1 <- f2ScienceStep(b1Off, sciClass)
            a1 <- f2ScienceStep(a1Off, sciClass)
            // Taken at a0 since the set opens the sequence, so no offset precedes the first cycle.
            f  <- flatStep(a0.telescopeConfig.copy(guiding = Disabled), ObserveClass.NightCal)
            r  <- arcStep(a0.telescopeConfig.copy(guiding = Disabled), ObserveClass.NightCal)
          yield PreDef(a0, b0, b1, a1, f, r)

    def compute[F[_]: Monad](
      modeName:  String,
      config:    Config,
      time:      IntegrationTime,
      static:    Flamingos2StaticConfig,
      expander:  SmartGcalExpander[F, Flamingos2StaticConfig, F2],
      calRole:   Option[CalibrationRole]
    ): EitherT[F, String, (List[StepViolation], StepDefinition)] =
      for
        r <- EitherT.fromEither[F](itcReadMode(time))
        p <- EitherT.fromEither:
               config.telescopeConfigs match
                 case NonEmptyList(a0, b0 :: b1 :: a1 :: Nil) => PreDef(config, time, r, a0, b0, b1, a1, calRole).asRight
                 // This case should be caught when validating arguments to the mode
                 // construction / update.  Nevertheless, we'll guarantee it here.
                 case _                    => s"Exactly 4 offset positions are needed for $modeName (${config.telescopeConfigs.size} provided).".asLeft
        d <- EitherT.liftF(p.expand(static, expander))
      yield d

  end StepDefinition

  case class Generator(
    steps:      StepDefinition,
    builder:    AtomBuilder[F2],
    goalCycles: NonNegInt
  ) extends SequenceGenerator[F2]:

    override def generate: Stream[Pure, Atom[F2]] =

      val gcalAtom: Option[ProtoAtom[ProtoStep[F2]]] =
        NonEmptyList.fromList(steps.cals).map(ProtoAtom(AtomTitle.NighttimeCal.some, _))

      val protoAtoms: List[ProtoAtom[ProtoStep[F2]]] =
        if goalCycles.value === 0 then Nil
        else gcalAtom.toList ++ List.fill(goalCycles.value)(ProtoAtom(AbbaCycleTitle.some, steps.abbaCycle))

      builder.buildStream(Stream.emits(protoAtoms))

  end Generator

  /**
   * The sequence of a MOS mask flat: one smart GCAL flat imaging the mask with
   * no disperser, the H filter, the MOS decker and bright read mode.
   */
  private def mosMaskFlat[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[Flamingos2StaticConfig, F2],
    static:        Flamingos2StaticConfig,
    namespace:     UUID,
    expander:      SmartGcalExpander[F, Flamingos2StaticConfig, F2],
    config:        Config
  ): F[Either[OdbError, SequenceGenerator[F2]]] =
    val flat = StepDefinition.eval:
      for
        _ <- F2.exposure    := TimeSpan.Zero
        _ <- F2.disperser   := none[Flamingos2Disperser]
        _ <- F2.filter      := Flamingos2Filter.H
        _ <- F2.readMode    := Flamingos2ReadMode.Bright
        _ <- F2.lyotWheel   := Flamingos2LyotWheel.F16
        _ <- F2.fpu         := config.fpuMask
        _ <- F2.decker      := Flamingos2Decker.MOS
        _ <- F2.readoutMode := config.readoutMode
        _ <- F2.reads       := Flamingos2ReadMode.Bright.readCount
        f <- StepDefinition.flatStep(TelescopeConfig(Offset.Zero, Disabled), ObserveClass.DayCal)
      yield f
    DaytimeSmartGcal.sequence(observationId, estimator, static, namespace, expander, Calibration.ImagingFlat, NonEmptyList.one((MosMaskFlatTitle, flat)))

  /**
   * @param modeName observing mode name, for error messages
   */
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
    if calRole.contains(CalibrationRole.MosMaskFlat) then
      mosMaskFlat(observationId, estimator, static, namespace, expander, config)
    else
      scienceGenerator(observationId, estimator, static, namespace, expander, modeName, config, time, calRole)

  private def scienceGenerator[F[_]: Monad](
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

    val posTime: EitherT[F, OdbError, IntegrationTime] =
      EitherT.fromEither:
        time.filterOrElse(_.exposureTime.toNonNegMicroseconds.value > 0, zeroExposureTime(observationId, modeName))

    val gen = for
      t      <- posTime
      (v, s) <- StepDefinition.compute(modeName, config, t, static, expander, calRole).leftMap(m => definitionError(observationId, m))
      c      <- EitherT.fromEither(s.cycleCount(t).leftMap(m => definitionError(observationId, m)))
    yield Generator(
      s,
      AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science),
      c
    ).reporting(v)

    gen.value
