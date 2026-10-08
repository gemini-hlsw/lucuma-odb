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
import cats.syntax.option.*
import cats.syntax.order.*
import eu.timepit.refined.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState.Disabled
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig as F2
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.*
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

/**
 * Flamingos 2 spectroscopy science sequence generation, shared by the long slit
 * and MOS modes.
 *
 * The two modes differ only in the aperture their steps carry.  The sequence
 * opens with a "Nighttime Calibrations" atom and, when the science runs longer
 * than the calibration set interval, closes with one too.  Nothing is placed in
 * between: the observer takes any further sets.  Flamingos 2 never reaches the
 * long wavelength cutoff, so the interval is always the short one.
 */
object Science:

  /**
   * The name of the ABBA cycle atoms.
   */
  val AbbaCycleTitle: NonEmptyString = NonEmptyString.unsafeFrom("ABBA Cycle")

  /**
   * The name of the nighttime cal atoms.
   */
  val NighttimeCalTitle: NonEmptyString = NonEmptyString.unsafeFrom("Nighttime Calibrations")

  private val Interval: TimeSpan = InfraredCalibration.ShortWavelengthSetInterval

  extension [A, B](lst: List[A])
    def removeFirstBy(b: B)(f: (A, B) => Boolean): List[A] =
      @annotation.tailrec
      def loop(rem: List[A], acc: List[A]): List[A] =
        rem match
          case Nil    => lst
          case h :: t => if f(h, b) then acc.reverse ++ t else loop(t, h :: acc)

      loop(lst, Nil)

  case class StepDefinition(
    a0:   ProtoStep[F2],
    b0:   ProtoStep[F2],
    b1:   ProtoStep[F2],
    a1:   ProtoStep[F2],
    cals: NonEmptyList[ProtoStep[F2]]
  ):
    /** ABBA science cycle. */
    def abbaCycle: NonEmptyList[ProtoStep[F2]] =
      NonEmptyList.of(a0, b0, b1, a1)

    def cycleCount(t: IntegrationTime): Either[String, NonNegInt] =
      calculateCycleCount[F2](s => s.telescopeConfig.guiding.isGuided, abbaCycle.toList, t)

  object StepDefinition extends SequenceState[F2] with Flamingos2InitialDynamicConfig:

    def f2ScienceStep(tc: TelescopeConfig, obsClass: ObserveClass): State[F2, ProtoStep[F2]] =
      scienceStep(tc, obsClass)

    /** Replaces the FPU used for the smart gcal lookup with the real aperture. */
    private def applyFpuMask(mask: Flamingos2FpuMask)(step: ProtoStep[F2]): ProtoStep[F2] =
      (ProtoStep.value[F2] andThen F2.fpu).replace(mask)(step)

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
        expander: SmartGcalExpander[F, Flamingos2StaticConfig, F2],
        mask:     Flamingos2FpuMask
      ): EitherT[F, String, StepDefinition] =

        val fpu = applyFpuMask(mask)

        EitherT(expander.expandFlatAndOrArc(static, flat, arc))
          .map(cs => StepDefinition(a0, b0, b1, a1, cs.map(adjustReadMode.andThen(fpu))))

    object PreDef:

      /**
       * The flat and arc are built carrying the config's `gcalFpu`, since the smart
       * gcal tables are keyed on builtin FPUs and would not match a custom mask.
       */
      def apply(
         config:  Config,
         time:    IntegrationTime,
         a0Off:   TelescopeConfig,
         b0Off:   TelescopeConfig,
         b1Off:   TelescopeConfig,
         a1Off:   TelescopeConfig,
         calRole: Option[CalibrationRole]
      ): PreDef =

        val readMode =
          config
            .explicitReadMode
            .getOrElse:
              Flamingos2ReadMode.forExposureTime(time.exposureTime)

        val sciClass = calRole.sciClass

        eval:
          for
            _  <- F2.exposure    := time.exposureTime
            _  <- F2.disperser   := config.disperser.some
            _  <- F2.filter      := config.filter
            _  <- F2.readMode    := readMode
            _  <- F2.lyotWheel   := Flamingos2LyotWheel.F16
            _  <- F2.fpu         := config.fpuMask
            _  <- F2.decker      := config.decker
            _  <- F2.readoutMode := config.readoutMode
            _  <- F2.reads       := config.explicitReads.getOrElse(readMode.readCount)
            a0 <- f2ScienceStep(a0Off, sciClass)
            b0 <- f2ScienceStep(b0Off, sciClass)
            b1 <- f2ScienceStep(b1Off, sciClass)
            a1 <- f2ScienceStep(a1Off, sciClass)
            _  <- F2.fpu         := Flamingos2FpuMask.builtin(config.gcalFpu)
            f  <- flatStep(a1.telescopeConfig.copy(guiding = Disabled), ObserveClass.NightCal)
            r  <- arcStep(a1.telescopeConfig.copy(guiding = Disabled), ObserveClass.NightCal)
          yield PreDef(a0, b0, b1, a1, f, r)

    def compute[F[_]: Monad](
      modeName:  String,
      config:    Config,
      time:      IntegrationTime,
      static:    Flamingos2StaticConfig,
      expander:  SmartGcalExpander[F, Flamingos2StaticConfig, F2],
      calRole:   Option[CalibrationRole]
    ): EitherT[F, String, StepDefinition] =
      for
        p <- EitherT.fromEither:
               config.telescopeConfigs match
                 case NonEmptyList(a0, b0 :: b1 :: a1 :: Nil) => PreDef(config, time, a0, b0, b1, a1, calRole).asRight
                 // This case should be caught when validating arguments to the mode
                 // construction / update.  Nevertheless, we'll guarantee it here.
                 case _                    => s"Exactly 4 offset positions are needed for $modeName (${config.telescopeConfigs.size} provided).".asLeft
        d <- p.expand(static, expander, config.fpuMask)
      yield d

  end StepDefinition

  case class Generator(
    steps:         StepDefinition,
    cycleEstimate: TimeSpan,
    builder:       AtomBuilder[F2],
    goalCycles:    NonNegInt
  ) extends SequenceGenerator[F2]:

    override def generate: Stream[Pure, Atom[F2]] =

      val gcalAtom: ProtoAtom[ProtoStep[F2]] = ProtoAtom(NighttimeCalTitle.some, steps.cals)

      val protoAtoms: List[ProtoAtom[ProtoStep[F2]]] =
        if goalCycles.value === 0 then Nil
        else
          val science     = List.fill(goalCycles.value)(ProtoAtom(AbbaCycleTitle.some, steps.abbaCycle))
          val scienceTime = cycleEstimate *| goalCycles.value
          (gcalAtom :: science) ++ Option.when(scienceTime > Interval)(gcalAtom).toList

      builder.buildStream(Stream.emits(protoAtoms))

  end Generator

  private def exposureTimeTooLong(oid: Observation.Id, estimate: TimeSpan): OdbError =
    import InfraredCalibration.minutes
    definitionError(oid, s"Estimated ABBA cycle time (${minutes(estimate)} minutes) for $oid must be less than ${minutes(Interval)} minutes.")

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

    val posTime: EitherT[F, OdbError, IntegrationTime] =
      EitherT.fromEither:
        time.filterOrElse(_.exposureTime.toNonNegMicroseconds.value > 0, zeroExposureTime(observationId, modeName))

    def cycleEstimate(steps: StepDefinition): EitherT[F, OdbError, TimeSpan] =
      val estimate = StepTimeEstimateCalculator.runEmpty(estimator.estimateTotalNel(static, steps.abbaCycle))
      EitherT.fromEither:
        Either.cond(estimate < Interval, estimate, exposureTimeTooLong(observationId, estimate))

    val gen = for
      t <- posTime
      s <- StepDefinition.compute(modeName, config, t, static, expander, calRole).leftMap(m => definitionError(observationId, m))
      e <- cycleEstimate(s)
      c <- EitherT.fromEither(s.cycleCount(t).leftMap(m => definitionError(observationId, m)))
    yield Generator(
      s,
      e,
      AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science),
      c
    ): SequenceGenerator[F2]

    gen.value
