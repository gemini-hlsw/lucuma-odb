// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package flamingos2
package longslit

import cats.Eq
import cats.Monad
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.syntax.flatMap.*
import cats.syntax.option.*
import eu.timepit.refined.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState.Disabled
import lucuma.core.math.syntax.int.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig as F2
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StepRecord
import lucuma.odb.sequence.util.AtomBuilder
import lucuma.odb.sequence.util.IndexTracker

import java.util.UUID

/**
 * Flamingos 2 long slit science sequence generation.
 */
object Science:

  case class Steps(
    a:     ProtoStep[F2],
    b:     ProtoStep[F2],
    flats: NonEmptyList[ProtoStep[F2]],
    arcs:  NonEmptyList[ProtoStep[F2]]
  ):
    val nominalSequence: NonEmptyList[ProtoStep[F2]] =
      NonEmptyList.of(NonEmptyList.of(a, b, b, a), flats, arcs).flatten

  object Steps extends SequenceState[F2] with Flamingos2InitialDynamicConfig:

    def compute[F[_]: Monad](
      oid:      Observation.Id,
      config:   Config,
      time:     IntegrationTime,
      expander: SmartGcalExpander[F, F2]
    ): EitherT[F, OdbError, Steps] =
      val (a, b, f, r) = eval:
        for
          _ <- F2.exposure    := time.exposureTime
          _ <- F2.disperser   := config.disperser.some
          _ <- F2.filter      := config.filter
          _ <- F2.readMode    := Flamingos2ReadMode.forExposureTime(time.exposureTime)
          _ <- F2.lyotWheel   := Flamingos2LyotWheel.F16
          _ <- F2.fpu         := Flamingos2FpuMask.builtin(config.fpu)
          _ <- F2.decker      := config.decker
          _ <- F2.readoutMode := config.readoutMode
          _ <- F2.reads       := config.explicitReads.getOrElse(Flamingos2ReadMode.forExposureTime(time.exposureTime).readCount)
          a <- scienceStep(0.arcsec,  15.arcsec, ObserveClass.Science)
          b <- scienceStep(0.arcsec, -15.arcsec, ObserveClass.Science)
          f <- flatStep(a.telescopeConfig.copy(guiding = Disabled), ObserveClass.NightCal)
          r <- arcStep(a.telescopeConfig.copy(guiding = Disabled), ObserveClass.NightCal)
        yield (a, b, f, r)

      (for
        fs <- EitherT(expander.expandStep(f))
        rs <- EitherT(expander.expandStep(r))
      yield Steps(a, b, fs, rs)).leftMap: msg =>
        OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $msg".some)

  case class ScienceState(
    steps:     Steps,
    builder:   AtomBuilder[F2],
    calcState: TimeEstimateCalculator.Last[F2],
    tracker:   IndexTracker,
    completed: Map[ProtoStep[F2], Int]
  ) extends SequenceGenerator[F2]:

    def generate(when: Timestamp): Stream[Pure, Atom[F2]] =
      val ps = NonEmptyList.fromList(
        Stream
          .emits(steps.nominalSequence.toList)
          .mapAccumulate(completed): (m, s) =>
            val count = m.getOrElse(s, 0)
            if count <= 0 then (m, s.some)
            else (m.updatedWith(s)(_.map(_ - 1)), none)
          .flatMap: (_, o) =>
            Stream.emits(o.toList)
          .compile
          .toList
        )

      ps.fold(Stream.empty): ss =>
        val f2    = steps.a._1
        val name  = s"${f2.fpu.builtinFpu.fold("image")(_.shortName)}, ${f2.filter.shortName}, ${f2.disperser.fold("none")(_.shortName)}"
        val state = builder.build(NonEmptyString.unapply(name), tracker.atomCount, tracker.stepCount, ss)
        Stream.emit(state.runA(calcState).value)

    def recordStep(step: StepRecord[F2])(using Eq[F2]): SequenceGenerator[F2] =
      if step.isAcquisitionSequence then this
      else copy(
        calcState = calcState.next(step.protoStep),
        tracker   = tracker.record(step),
        completed = if step.successfullyCompleted
                    then completed.updatedWith(step.protoStep)(n => (n.getOrElse(0) + 1).some)
                    else completed
      )

  def instantiate[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[Flamingos2StaticConfig, F2],
    static:        Flamingos2StaticConfig,
    namespace:     UUID,
    expander:      SmartGcalExpander[F, F2],
    config:        Config,
    time:          Either[OdbError, IntegrationTime]
  ): F[Either[OdbError, SequenceGenerator[F2]]] =

    def usingTime(t: IntegrationTime): EitherT[F, OdbError, SequenceGenerator[F2]] =
      Steps
        .compute(observationId, config, t, expander)
        .map: steps =>
          ScienceState(
            steps,
            AtomBuilder.instantiate(
              estimator,
              static,
              namespace,
              SequenceType.Science
            ),
            TimeEstimateCalculator.Last.empty[F2],
            IndexTracker.Zero,
            Map.empty
          )

    val posTime: EitherT[F, OdbError, IntegrationTime] =
      EitherT.fromEither:
        time.filterOrElse(_.exposureTime.toNonNegMicroseconds.value > 0, OdbError.SequenceUnavailable(observationId, s"Could not generate a sequence for $observationId: Flamingos 2 Long Slit requires a positive exposure time.".some))

    (for
      t <- posTime
      g <- usingTime(t)
    yield g).value
