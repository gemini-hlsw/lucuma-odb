// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import cats.Comparison.*
import cats.Monad
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.data.State
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.order.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.core.optics.syntax.optional.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.util.AtomBuilder
import monocle.Lens

import java.util.UUID


/**
 * GMOS Long Slit science sequence generation.  GMOS Long Slit divides up the
 * time in nominal one hour blocks where each block is associated with a
 * wavelength dither.  Each block is given a nighttime arc and a flat for that
 * configuration followed by as many science datasets as we can fit in an hour
 * of exposure time, distributed across as many selected offset positions as
 * possible.  Calibrations are considered still valid within a block if they are
 * no longer than 1.5 hours old, so there is extra time to complete the hour of
 * science if necessary.
 *
 * Each block is placed in its own atom, but of course that doesn't preclude
 * stopping early.  All science steps for which there are valid calibrations in
 * the block are counted toward completion, regardless of the order that the
 * calibrations and science steps appear in the execution sequence or whether
 * unrelated steps are executed in the atom.
 */
object Science:

  /**
   * Defines how long a calibration remains valid.  After this amount of time,
   * the calibration must be taken again.
   */
  val CalValidityPeriod: TimeSpan =
    90.minuteTimeSpan

  /**
   * The nominal amount of time to spend at one wavelength dither. This will
   * help determine how many exposures we hope to obtain while processing a
   * single "wavelength block".
   */
  val SciencePeriod: TimeSpan =
    1.hourTimeSpan

  /**
   * Exposure time for a twilight flat observation.
   */
  val TwilightExposureTime: TimeSpan =
    30.secondTimeSpan

  extension (t: Timestamp)
    def plusCalValidityPeriod: Timestamp =
      t +| CalValidityPeriod

    def minusCalValidityPeriod: Timestamp =
      t -| CalValidityPeriod

  // Lens from the step to its telescope offset in q.
  private def protoStepOffset[D]: Lens[ProtoStep[D], Offset] =
    ProtoStep.telescopeConfig andThen TelescopeConfig.offset

  private def protoStepQ[D]: Lens[ProtoStep[D], Offset.Q] =
    protoStepOffset andThen Offset.q

  extension [D](p: ProtoStep[D])
    def q: Offset.Q =
      protoStepQ.get(p)

    def withZeroOffset: ProtoStep[D] =
      protoStepOffset.replace(Offset.Zero)(p)

  /**
   * Science exposure count goals for a given wavelength dither.
   *
   * @param Δλ          the wavelength dither itself
   * @param index       order in which this wavelength block is executed
   * @param requirement number of science steps per spatial offset at this
   *                    wavelength dither
   */
  case class Goal(
    Δλ:          WavelengthDither,
    index:       NonNegInt,
    requirement: Remaining[Offset.Q]
  ):
    def description: NonEmptyString =
      NonEmptyString.unsafeFrom(s"${Δλ.toNanometers.value} nm")

  object Goal:

    def compute(
      dithers:   List[WavelengthDither],
      offsets:   List[Offset.Q],
      expTimeμs: PosLong,
      expCount:  PosInt
    ): NonEmptyList[Goal] =

      def nel[A](as: List[A], default: A): NonEmptyList[A] =
        NonEmptyList.fromList(as).getOrElse(NonEmptyList.one(default))

      val Δλs     = nel(dithers, WavelengthDither.Zero)
      val ΔλCount = Δλs.size

      val sci            = SciencePeriod.toMicroseconds
      val time           = sci min expTimeμs.value
      val maxExpPerBlock = (sci / time).toInt

      // First figure out how many steps per wavelength dither to assign.

      val expCountPerDither =
        NonEmptyList.fromListUnsafe:
          if expCount.value <= (ΔλCount * maxExpPerBlock) then
            // Spread the exposures we have as much as possible over the dithers.
            val base  = expCount.value / ΔλCount
            val extra = expCount.value % ΔλCount
            List.tabulate(ΔλCount): Δλidx =>
              base + (if Δλidx < extra then 1 else 0)
          else
            // Try to fill as many blocks as possible
            val fullBlocks = expCount.value / maxExpPerBlock
            val base       = fullBlocks / ΔλCount * maxExpPerBlock
            List.tabulate(ΔλCount): Δλidx =>
              val extra = Δλidx.comparison(fullBlocks % ΔλCount) match
                case LessThan    => maxExpPerBlock
                case EqualTo     => expCount.value % maxExpPerBlock
                case GreaterThan => 0
              base + extra

      // Now spread the offsets around as well as possible across the dithers.

      val qs      = nel(offsets, Offset.Q.Zero)
      val qCount  = qs.size

      val runningSums =
        NonEmptyList.fromListUnsafe:
          expCountPerDither.toList.scanLeft(0)(_ + _).init

      Δλs
        .zip(expCountPerDither)
        .zip(runningSums)
        .zipWithIndex
        .map { case (((dither, n), sum), idx) =>
          val base   = n / qCount
          val extra  = n % qCount

          // Start with 'base' steps (i.e., exposures) for every offset
          val counts = Array.fill(qCount)(base)

          // Figure out where to put the extras.  We'd like to spread them
          // across dithers as evenly as we can.
          (0 until extra).foreach: j =>
            counts((j + sum) % qCount) += 1

          Goal(dither, NonNegInt.unsafeFrom(idx), Remaining.from(qs.toList.zip(counts)))
        }

  end Goal

  /**
   * A description of the steps (arcs, flats, and science) that are associated
   * with a wavelength goal.  The collections of steps is executed together in
   * a block, or "atom".  Nominally, each block will contain an arc, a flat,
   * and as many science datasets as we can fit in a single hour.
   *
   * @param goal wavelength dither and count of science datasets we seek for
   *             the observation as a whole
   * @param arcs typically a single arc calibration, but smart gcal sometimes
   *             prescribes multiple concrete arcs per smart arc
   * @param flats typically a single flat calibration, but smart gcal sometimes
   *              prescribes multiple concrete flats per smart flat
   * @param science an instance of the science step that should be repeated
   */
  final case class StepDefinition[D](
    goal:    Goal,
    arcs:    List[ProtoStep[D]], // all with offset 0
    flats:   List[ProtoStep[D]], // all with offset 0
    science: ProtoStep[D]        // all with offset 0
  ):
    val arcCounts:  Map[ProtoStep[D], NonNegInt] = StepDefinition.calCounts(arcs)
    val flatCounts: Map[ProtoStep[D], NonNegInt] = StepDefinition.calCounts(flats)
    val allCals: List[ProtoStep[D]] = arcs ++ flats
    val allCalsCounts: Map[ProtoStep[D], NonNegInt]  = arcCounts ++ flatCounts

  object StepDefinition:

    private def calCounts[D](cal: List[ProtoStep[D]]): Map[ProtoStep[D], NonNegInt] =
      cal.groupMapReduce(identity)(_ => 1)(_ + _)
        .view
        .mapValues(NonNegInt.unsafeFrom)
        .toMap

    sealed trait Computer[D, G, L, U] extends GmosSequenceState[D, G, L, U]:

      def setup(config: Config[G, L, U], time: TimeSpan): State[D, Unit] =
        for {
          _ <- optics.exposure    := time
          _ <- optics.grating     := (config.grating, GmosGratingOrder.One, config.centralWavelength).some
          _ <- optics.filter      := config.filter
          _ <- optics.fpu         := GmosFpuMask.builtin.reverseGet(config.fpu).some

          _ <- optics.xBin        := config.xBin
          _ <- optics.yBin        := config.yBin
          _ <- optics.ampReadMode := config.ampReadMode
          _ <- optics.ampGain     := config.ampGain

          _ <- optics.roi         := config.roi
        } yield ()

      /**
       * Creates the BlockDefinition, assuming we find smart gcal definitions
       * that match the `config`.
       *
       * @param expander  smart gcal expander for GMOS (north or south as
       *                  appropriate)
       * @param config    observation configuration
       * @param expTimeμs integration time for science datasets as prescribed
       *                  by ITC
       * @param expCount  total step count prescribed by the ITC
       * @param calRole   calibration role, which determines whether arcs and/or
       *                  flats are needed
       */
      def compute[F[_]: Monad](
        oid:       Observation.Id,
        expander:  SmartGcalExpander[F, D],
        config:    Config[G, L, U],
        expTimeμs: PosLong,
        expCount:  PosInt,
        calRole:   Option[CalibrationRole]
      ): F[Either[OdbError, NonEmptyList[StepDefinition[D]]]] =

        val λ            = config.centralWavelength
        val isTwilight   = calRole.contains(CalibrationRole.Twilight)
        val includeFlats = !isTwilight
        val includeArcs  = calRole.isEmpty
        val goals        = Goal.compute(config.wavelengthDithers, config.spatialOffsets, expTimeμs, expCount)

        def define(g: Goal): EitherT[F, OdbError, StepDefinition[D]] =
          val (smartArc, smartFlat, science) =
            eval:
              for
                _ <- setup(config, TimeSpan.unsafeFromMicroseconds(expTimeμs.value))
                _ <- optics.wavelength := λ.offset(g.Δλ).getOrElse(λ)
                a <- arcStep(TelescopeConfig(Offset.Zero, StepGuideState.Disabled), if isTwilight then ObserveClass.DayCal else ObserveClass.NightCal)
                f <- flatStep(TelescopeConfig(Offset.Zero, StepGuideState.Disabled), if isTwilight then ObserveClass.DayCal else ObserveClass.NightCal)
                s <- scienceStep(TelescopeConfig(Offset.Zero, StepGuideState.Enabled), if isTwilight then ObserveClass.DayCal else ObserveClass.Science)
              yield (a, f, s)

          val defn = for
            fs <- if includeFlats then EitherT(expander.expandStep(smartFlat)).map(_.toList) else EitherT.pure(List.empty)
            as <- if includeArcs then EitherT(expander.expandStep(smartArc)).map(_.toList) else EitherT.pure(List.empty)
          yield StepDefinition(g, as.toList, fs, science)

          defn.leftMap(s => OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $s".some))

        goals.traverse(define).value

    end Computer

    object North extends GmosNorthSequenceState
                    with Computer[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]

    object South extends GmosSouthSequenceState
                    with Computer[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]
  end StepDefinition

  /**
   * Dither collects information associated with generation of steps for a
   * single wavelength dither.
   */
  case class Dither[D](
    definition: StepDefinition[D],
    remaining:  Remaining[Offset.Q]
  ):
    /** The description of this block (as taken from the goal / adjustment). */
    def desc: NonEmptyString =
      definition.goal.description

    /** Decreases the remaining count for each offset in the map. */
    def complete(m: Map[Offset.Q, Int]): Dither[D] =
      copy(remaining = remaining.decrementAll(m))

    /**
     * Generates a full wavelength block.
     */
    def generateFullBlock(blockSize: PosInt): (Dither[D], List[ProtoStep[D]]) =
      // When we generate a step, it is for a particular offset which must be
      // set.  In the step definition, cals and science datasets are stored
      // with zero offsets.
      def setQ(steps: List[(ProtoStep[D], Offset.Q)]): List[ProtoStep[D]] =
        steps.map((step, q) => protoStepQ.replace(q)(step))

      val (qs, remainingʹ) = remaining.take(blockSize.value)
      (
        copy(remaining = remainingʹ),
         qs.headOption match
           case None    => List.empty[ProtoStep[D]]
           case Some(q) => setQ(definition.allCals.tupleRight(q) ++ List.fill(qs.size)(definition.science).zip(qs))
      )

  end Dither

  object Dither:
    def init[D](definition: StepDefinition[D]): Dither[D] =
      Dither(definition, definition.goal.requirement)

  /**
   * The science generator keeps up with an ordered list of block records, and
   * works to compute the number of valid science steps have been obtained for
   * each block as step records are added.  When all step records are accounted
   * for, we ask for the remaining steps to fill out the block and follow it up
   * with all remaining steps.
   */
  private case class ScienceGenerator[S, D](
    estimator:   TimeEstimateCalculator[S, D],
    static:      S,
    atomBuilder: AtomBuilder[D],
    expMicroSec: PosLong,
    dithers:     NonEmptyVector[Dither[D]]
  ) extends SequenceGenerator[D]:

    val sci            = SciencePeriod.toMicroseconds
    val time           = sci min expMicroSec.value
    val maxExpPerBlock = PosInt.unsafeFrom((sci / time).toInt)
    val length: Int    = dithers.length

    override def generate: Stream[Pure, Atom[D]] =
      Stream
        .iterate(0)(pos => (pos + 1) % length)
        .mapAccumulate((dithers, 0, TimeEstimateCalculator.Last.empty[D])) { case ((ds, aix, cs), pos) =>
          val dither      = ds.getUnsafe(pos)
          val (dʹ, steps) = dither.generateFullBlock(maxExpPerBlock)
          val (csʹ, atom) = atomBuilder.buildOption(dither.desc.some, aix, 0, steps).run(cs).value
          val dsʹ         = ds.updatedUnsafe(pos, dʹ)
          ((dsʹ, aix + 1, csʹ), atom)
        }
        .takeThrough { case ((ds, _, _), _) => ds.foldMap(_.remaining.total.value) > 0 }
        .collect { case (_, Some(atom)) => atom}

  end ScienceGenerator

  private object ScienceGenerator:

    private def calibrationObservationConfig[G, L, U](
      c: Config[G, L, U]
    ): Config[G, L, U] =
      val limit   = c.coverage.toPicometers.value.value / 10.0
      val dithers = if c.wavelengthDithers.exists(_.toPicometers.value.abs > limit) then c.wavelengthDithers
                    else List(WavelengthDither.Zero)
      (for {
        _ <- Config.explicitWavelengthDithers := dithers.some
        _ <- Config.explicitSpatialOffsets    := List(Offset.Q.Zero).some
      } yield ()).runS(c).value

    def instantiate[F[_]: Monad, S, D, G, L, U](
      oid:       Observation.Id,
      estimator: TimeEstimateCalculator[S, D],
      static:    S,
      namespace: UUID,
      expander:  SmartGcalExpander[F, D],
      stepDef:   StepDefinition.Computer[D, G, L, U],
      config:    Config[G, L, U],
      time:      Either[OdbError, IntegrationTime],
      calRole:   Option[CalibrationRole]
    ): F[Either[OdbError, SequenceGenerator[D]]] =

      def sequenceUnavailable(m: String): OdbError =
        OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $m".some)

      def extractTime: Either[OdbError, (PosLong, PosInt)] =
        time.flatMap: t =>
          PosLong
            .from(t.exposureTime.toNonNegMicroseconds.value)
            .bimap(
              _  => sequenceUnavailable(s"GMOS Long Slit science requires a positive exposure time."),
              μs => (μs, t.exposureCount)
            )

      // Adjust the config and integration time according to the calibration role.
      val configAndTime = calRole match
        case None                                     =>
          extractTime.tupleLeft(config)

        case Some(CalibrationRole.SpectroPhotometric) =>
          val configʹ = calibrationObservationConfig(config)
          extractTime.map: (μs, _) =>
            (configʹ, (μs, PosInt.unsafeFrom(configʹ.wavelengthDithers.length.max(1))))

        case Some(CalibrationRole.Twilight)           =>
          val configʹ = calibrationObservationConfig(config)
          val μs      = PosLong.unsafeFrom(TwilightExposureTime.toMicroseconds)
          val n       = PosInt.unsafeFrom(configʹ.wavelengthDithers.length.max(1))
          (configʹ, (μs, n)).asRight[OdbError]

        case Some(c)                                  =>
          sequenceUnavailable(s"GMOS Long Slit ${c.tag} not implemented").asLeft

      // If exposure time is longer than the science period, there will never be
      // time enough to do any science steps.
      val configAndTimeʹ = configAndTime.filterOrElse(
        _._2._1.value <= SciencePeriod.toMicroseconds,
        sequenceUnavailable(s"Exposure times over ${SciencePeriod.toMinutes} minutes are not supported.")
      )

      // Compute the generator
      val result = for
        (configʹ, (μs, n)) <- EitherT.fromEither[F](configAndTimeʹ)
        defs               <- EitherT(stepDef.compute(oid, expander, configʹ, μs, n, calRole))
      yield ScienceGenerator(
        estimator,
        static,
        AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science),
        μs,
        defs.map(Dither.init).toNev
      )

      result.widen[SequenceGenerator[D]].value
    end instantiate

  end ScienceGenerator

  def gmosNorth[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[StaticConfig.GmosNorth, GmosNorth],
    static:        StaticConfig.GmosNorth,
    namespace:     UUID,
    expander:      SmartGcalExpander[F, GmosNorth],
    config:        Config.GmosNorth,
    time:          Either[OdbError, IntegrationTime],
    calRole:       Option[CalibrationRole]
  ): F[Either[OdbError, SequenceGenerator[GmosNorth]]] =
    ScienceGenerator.instantiate(observationId, estimator, static, namespace, expander, StepDefinition.North, config, time, calRole)

  def gmosSouth[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[StaticConfig.GmosSouth, GmosSouth],
    static:        StaticConfig.GmosSouth,
    namespace:     UUID,
    expander:      SmartGcalExpander[F, GmosSouth],
    config:        Config.GmosSouth,
    time:          Either[OdbError, IntegrationTime],
    calRole:       Option[CalibrationRole]
  ): F[Either[OdbError, SequenceGenerator[GmosSouth]]] =
    ScienceGenerator.instantiate(observationId, estimator, static, namespace, expander, StepDefinition.South, config, time, calRole)
