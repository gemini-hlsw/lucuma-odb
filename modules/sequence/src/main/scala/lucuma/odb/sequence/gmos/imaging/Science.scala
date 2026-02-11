// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package imaging

import cats.Eq
import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.data.State
import cats.effect.Sync
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import fs2.Chunk
import fs2.Pure
import fs2.Stream
import lucuma.core.data.Zipper
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.Itc.GmosNorthImaging
import lucuma.odb.data.Itc.GmosSouthImaging
import lucuma.odb.data.Itc.Result
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StepRecord
import lucuma.odb.sequence.data.TelescopeConfigGenerator
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

object Science:

  sealed trait ProtoSequences[D, G, L, U] extends GmosSequenceState[D, G, L, U]:

    def setup(config: Config[L]): State[D, Unit] =
      for
        _ <- optics.grating     := none[(G, GmosGratingOrder, Wavelength)]
        _ <- optics.fpu         := none[GmosFpuMask[U]]
        _ <- optics.xBin        := GmosXBinning(config.bin)
        _ <- optics.yBin        := GmosYBinning(config.bin)
        _ <- optics.ampReadMode := config.ampReadMode
        _ <- optics.ampGain     := config.ampGain
        _ <- optics.roi         := config.roi
      yield ()

    def runSetup(config: Config[L]): D =
      setup(config).runS(initialDynamicConfig).value

    def setFilter(filter: L, time: TimeSpan): State[D, Unit] =
      for
        _ <- optics.filter   := filter.some
        _ <- optics.exposure := time
      yield ()

    def grouped(
      config:     Config[L],
      time:       NonEmptyMap[L, Zipper[Result]],
      offsets:    List[TelescopeConfig],
      skyOffsets: List[TelescopeConfig]
    )(using Order[L]): Stream[Pure, ProtoAtom[ProtoStep[D]]] =

      def oneFilter(filter: L): State[D, Stream[Pure, ProtoAtom[ProtoStep[D]]]] =
        val integrationTime = time(filter).get.focus._2
        for
          _   <- setFilter(filter, integrationTime.exposureTime)
          sky <- skyOffsets
                   .traverse: offset =>
                     scienceStep(offset, ObserveClass.NightCal)
          obj <- offsets.take(integrationTime.exposureCount.value)
                   .traverse: offset =>
                     scienceStep(offset, ObserveClass.Science)
        yield
          // If there are no sky positions, then each exposure is an atom.
          // If there are sky positions, then we group them into a single atom.
          if sky.isEmpty then Stream.emits(obj.map(o => ProtoAtom.one(none, o)))
          else Stream.emit(ProtoAtom(none, NonEmptyList.fromListUnsafe(sky ++ obj ++ sky)))

      Stream
        .emits(time.keys.toNonEmptyList.toList.sorted)
        .mapAccumulate(runSetup(config)): (cur, filter) =>
          oneFilter(filter).run(cur).value
        .map(_._2)
        .flatten

    def interleaved(
      config:     Config[L],
      time:       NonEmptyMap[L, Zipper[Result]],
      offsets:    List[TelescopeConfig],
      skyOffsets: List[TelescopeConfig]
    )(using Order[L]): Stream[Pure, ProtoAtom[ProtoStep[D]]] =

      val filterTimes: Map[L, IntegrationTime] =
        time.toNel.toList.map((filter, zipper) => filter -> zipper.focus._2).toMap

      val filters = filterTimes.keys.toList.sorted

      val skyList: List[(L, TelescopeConfig)] =
        val n = skyOffsets.size / filters.size
        filters.flatMap(f => List.fill(n)(f)).zip(skyOffsets)

      def skySteps(skyList: List[(L, TelescopeConfig)]): State[D, List[ProtoStep[D]]] =
        skyList.traverse: (filter, offset) =>
          for
            _ <- setFilter(filter, filterTimes(filter).exposureTime)
            s <- scienceStep(offset, ObserveClass.NightCal)
          yield s

      // number of groups of interleaved filters
      val groupCount: Int =
        filterTimes.values.map(_.exposureCount.value).min

      def oneGroup(n: Int): State[D, Stream[Pure, D]] =
        // exposure count per filter in this group.
        val perFilterCounts =
          filters
            .fproduct: filter =>
              val total = filterTimes(filter).exposureCount.value
              (total / groupCount) + (if total % groupCount > n then 1 else 0)
            .toMap

        filters
          .traverse: filter =>
            for
              _ <- setFilter(filter, filterTimes(filter).exposureTime)
              d <- State.get
            yield Stream.emits(List.fill(perFilterCounts(filter))(d))
          .map(lst => Stream.emits(lst).flatten)

      val groups: State[D, Stream[Pure, Chunk[ProtoStep[D]]]] =
        (0 until groupCount)
          .toList
          .traverse(oneGroup)   // State[D, List[Stream[Pure, D]]]
          .map: lst =>
            Stream
              .emits(lst.zipWithIndex.map((s, i) => s.tupleRight(i)))
              .flatten
              .zip(Stream.emits(offsets)) // Stream[Pure, ((D, Int), TelescopeConfig)]
              .map { case ((d, i), tc) =>
                ProtoStep(d, StepConfig.Science, tc, ObserveClass.Science) -> i
              }
              .groupAdjacentBy(_._2)
              .map(_._2.map(_._1))

      val prog = for
        _ <- setup(config)
        b <- skySteps(skyList)
        g <- groups
        a <- skySteps(skyList.reverse)
      yield g.flatMap: chunk =>
        if skyList.isEmpty then
          Stream.emits(chunk.toList.map(s => ProtoAtom.one(none, s)))
        else
          Stream.emit(ProtoAtom(none, NonEmptyList.fromListUnsafe(b ++ chunk.toList ++ a)))

      prog.runA(initialDynamicConfig).value


  object ProtoSequences:

    object North extends GmosNorthSequenceState
                    with ProtoSequences[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]

    object South extends GmosSouthSequenceState
                    with ProtoSequences[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]

  def makeGenerator[S, D](
      estimator:  TimeEstimateCalculator[S, D],
      static:     S,
      namespace:  UUID,
      protoAtoms: Stream[Pure, ProtoAtom[ProtoStep[D]]]
  ): SequenceGenerator[D] =

    case class ImagingGenerator(
      recordedSteps: Map[ProtoStep[D], Int]
    ) extends SequenceGenerator.Base[D]:

      override def recordStep(step: StepRecord[D])(using Eq[D]): SequenceGenerator[D] =
        if step.successfullyCompleted && step.isScience then
          ImagingGenerator:
            recordedSteps.updatedWith(step.protoStep): count =>
              count.map(_ + 1).orElse(1.some)
        else
          this

      override def generate: Stream[Pure, Atom[D]] =
        val build = AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science)

        protoAtoms
          .zipWithIndex
          .mapAccumulate(TimeEstimateCalculator.Last.empty[D]) { case (cs, (protoAtom, idx)) =>
            build.build(protoAtom.description, idx.toInt, 0, protoAtom.steps).run(cs).value
          }
          .map(_._2)
          .mapAccumulate(recordedSteps): (rec, nextAtom) =>
            val (recʹ, optSteps) = nextAtom.steps.toList.mapAccumulate(rec): (rec, step) =>
              val protoStep = ProtoStep.fromStep(step)
              (
                rec.updatedWith(protoStep): count =>
                  count.map(_ - 1).orElse(none).filter(_ > 0),
                Option.when(!rec.contains(protoStep))(step)
              )
            (
              recʹ,
              NonEmptyList.fromList(optSteps.flatten).map(steps => nextAtom.copy(steps = steps))
            )
          .collect { case (_, Some(atom)) => atom }

    ImagingGenerator(Map.empty)

  object ScienceGenerator:

    def instantiate[F[_]: Sync, S, D, G, L, U](
      state:     ProtoSequences[D, G, L, U],
      estimator: TimeEstimateCalculator[S, D],
      static:    S,
      namespace: UUID,
      config:    Config[L],
      time:      Either[OdbError, NonEmptyMap[L, Zipper[Result]]]
    )(using Order[L]): F[Either[OdbError, SequenceGenerator[D]]] =

      time.traverse: nem =>

        // Pre-generate enough object and sky offsets to cover the sequence.
        def generateOffsets(
          offsets0:   TelescopeConfigGenerator,
          skyCount:   NonNegInt,
          skyOffsets: TelescopeConfigGenerator
        )(f: List[Int] => Int): F[(List[TelescopeConfig], List[TelescopeConfig])] =

          val cnt     = NonNegInt.unsafeFrom(f(nem.toNel.toList.map((_,  zipper) => zipper.focus._2.exposureCount.value)))
          val offsets = offsets0 match
            case TelescopeConfigGenerator.NoGenerator =>
              TelescopeConfigGenerator.Enumerated(NonEmptyList.fromListUnsafe(List.fill(cnt.value)(TelescopeConfig.Default)))
            case g                                    =>
              g

          for
            o <- offsets.generate(cnt)
            s <- skyOffsets.generate(skyCount, StepGuideState.Disabled)
          yield (o, s)

        val protoAtoms = config.variant match
          case Variant.Grouped(_, offsets, skyCount, skyOffsets)  =>
            // The same pattern is repeated for each filter so we need enough
            // offsets to cover the filter with the most exposures.
            generateOffsets(offsets, skyCount, skyOffsets)(_.max).map: (o, s) =>
              state.grouped(config, nem, o, s)

          case Variant.Interleaved(offsets, skyCount, skyOffsets) =>
            // The offset pattern is applied to the sequence as a whole so we
            // need an offset per science exposure. Also there are skyCount sky
            // positions per filter.
            val sc = NonNegInt.unsafeFrom(skyCount.value * nem.length)
            generateOffsets(offsets, sc, skyOffsets)(_.sum).map: (o, s) =>
              state.interleaved(config, nem, o, s)

          case Variant.PreImaging(o1, o2, o3, o4)                 =>
            val offsets    = TelescopeConfigGenerator.Enumerated:
              NonEmptyList.of(o1, o2, o3, o4).map(o => TelescopeConfig(o, StepGuideState.Enabled))
            val skyOffsets = TelescopeConfigGenerator.NoGenerator
            generateOffsets(offsets, NonNegInt.MinValue, skyOffsets)(_.max).map: (o, s) =>
              state.grouped(config, nem, o, s)

        protoAtoms.map: v =>
          makeGenerator(estimator, static, namespace, v)

  def wavelengthOrder[L](c: Config[L])(f: L => Wavelength): Order[L] =
    import WavelengthOrder.*
    c.variant.fold(_.order, _ => Increasing, _ => Increasing) match
      case Increasing => Order.by(f)
      case Decreasing => Order.reverse(Order.by(f))

  def gmosNorth[F[_]: Sync](
    estimator: TimeEstimateCalculator[StaticConfig.GmosNorth, GmosNorth],
    static:    StaticConfig.GmosNorth,
    namespace: UUID,
    config:    Config.GmosNorth,
    time:      Either[OdbError, GmosNorthImaging]
  ): F[Either[OdbError, SequenceGenerator[GmosNorth]]] =
    given Order[GmosNorthFilter] = wavelengthOrder(config)(_.wavelength)
    ScienceGenerator.instantiate(ProtoSequences.North, estimator, static, namespace, config, time.map(_.science))

  def gmosSouth[F[_]: Sync](
    estimator: TimeEstimateCalculator[StaticConfig.GmosSouth, GmosSouth],
    static:    StaticConfig.GmosSouth,
    namespace: UUID,
    config:    Config.GmosSouth,
    time:      Either[OdbError, GmosSouthImaging]
  ): F[Either[OdbError, SequenceGenerator[GmosSouth]]] =
    given Order[GmosSouthFilter] = wavelengthOrder(config)(_.wavelength)
    ScienceGenerator.instantiate(ProtoSequences.South, estimator, static, namespace, config, time.map(_.science))