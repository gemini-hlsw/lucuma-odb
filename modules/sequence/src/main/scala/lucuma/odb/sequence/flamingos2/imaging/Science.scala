// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package flamingos2
package imaging

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
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepGuideState
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig as F2
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.Itc.Flamingos2Imaging
import lucuma.odb.data.Itc.Result
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.TelescopeConfigGenerator
import lucuma.odb.sequence.imaging.ImagingSequence
import lucuma.odb.sequence.imaging.Variant

import java.util.UUID

object Science:

  object ProtoSequences extends SequenceState[F2] with Flamingos2InitialDynamicConfig:

    def setup(config: Config): State[F2, Unit] =
      for
        _ <- F2.decker      := config.decker
        _ <- F2.readMode    := config.readMode
        _ <- F2.reads       := config.reads
        _ <- F2.readoutMode := config.readoutMode
      yield ()

    def runSetup(config: Config): F2 =
      setup(config).runS(initialDynamicConfig).value

    def setFilter(filter: Flamingos2Filter, time: TimeSpan): State[F2, Unit] =
      for
        _ <- F2.filter   := filter
        _ <- F2.exposure := time
      yield ()

    def grouped(
      config:     Config,
      time:       NonEmptyMap[Flamingos2Filter, Zipper[Result]],
      offsets:    List[TelescopeConfig],
      skyOffsets: List[TelescopeConfig]
    )(using Order[Flamingos2Filter]): Stream[Pure, ProtoAtom[ProtoStep[F2]]] =

      def oneFilter(filter: Flamingos2Filter): State[F2, Stream[Pure, ProtoAtom[ProtoStep[F2]]]] =
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
      config:     Config,
      time:       NonEmptyMap[Flamingos2Filter, Zipper[Result]],
      offsets:    List[TelescopeConfig],
      skyOffsets: List[TelescopeConfig]
    )(using Order[Flamingos2Filter]): Stream[Pure, ProtoAtom[ProtoStep[F2]]] =

      val filterTimes: Map[Flamingos2Filter, IntegrationTime] =
        time.toNel.toList.map((filter, zipper) => filter -> zipper.focus._2).toMap

      val filters = filterTimes.keys.toList.sorted

      val skyList: List[(Flamingos2Filter, TelescopeConfig)] =
        val n = skyOffsets.size / filters.size
        filters.flatMap(f => List.fill(n)(f)).zip(skyOffsets)

      def skySteps(skyList: List[(Flamingos2Filter, TelescopeConfig)]): State[F2, List[ProtoStep[F2]]] =
        skyList.traverse: (filter, offset) =>
          for
            _ <- setFilter(filter, filterTimes(filter).exposureTime)
            s <- scienceStep(offset, ObserveClass.NightCal)
          yield s

      // number of groups of interleaved filters
      val groupCount: Int =
        filterTimes.values.map(_.exposureCount.value).min

      def oneGroup(n: Int): State[F2, Stream[Pure, F2]] =
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

      val groups: State[F2, Stream[Pure, Chunk[ProtoStep[F2]]]] =
        (0 until groupCount)
          .toList
          .traverse(oneGroup)   // State[F2, List[Stream[Pure, F2]]]
          .map: lst =>
            Stream
              .emits(lst.zipWithIndex.map((s, i) => s.tupleRight(i)))
              .flatten
              .zip(Stream.emits(offsets)) // Stream[Pure, ((F2, Int), TelescopeConfig)]
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

  object ScienceGenerator:

    def instantiate[F[_]: Sync](
      estimator: StepTimeEstimateCalculator[Flamingos2StaticConfig, F2],
      static:    Flamingos2StaticConfig,
      namespace: UUID,
      config:    Config,
      time:      Either[OdbError, NonEmptyMap[Flamingos2Filter, Zipper[Result]]]
    )(using Order[Flamingos2Filter]): F[Either[OdbError, SequenceGenerator[F2]]] =

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
              ProtoSequences.grouped(config, nem, o, s)

          case Variant.Interleaved(offsets, skyCount, skyOffsets) =>
            // The offset pattern is applied to the sequence as a whole so we
            // need an offset per science exposure. Also there are skyCount sky
            // positions per filter.
            val sc = NonNegInt.unsafeFrom(skyCount.value * nem.length)
            generateOffsets(offsets, sc, skyOffsets)(_.sum).map: (o, s) =>
              ProtoSequences.interleaved(config, nem, o, s)

          case Variant.PreImaging(o1, o2, o3, o4)                 =>
            val offsets    = TelescopeConfigGenerator.Enumerated:
              NonEmptyList.of(o1, o2, o3, o4).map(o => TelescopeConfig(o, StepGuideState.Enabled))
            val skyOffsets = TelescopeConfigGenerator.NoGenerator
            generateOffsets(offsets, NonNegInt.MinValue, skyOffsets)(_.max).map: (o, s) =>
              ProtoSequences.grouped(config, nem, o, s)

        protoAtoms.map: v =>
          ImagingSequence.makeGenerator(estimator, static, namespace, v)

  def flamingos2[F[_]: Sync](
    estimator: StepTimeEstimateCalculator[Flamingos2StaticConfig, F2],
    static:    Flamingos2StaticConfig,
    namespace: UUID,
    config:    Config,
    time:      Either[OdbError, Flamingos2Imaging]
  ): F[Either[OdbError, SequenceGenerator[F2]]] =
    given Order[Flamingos2Filter] = ImagingSequence.wavelengthOrder(config.variant)(_.wavelength)
    ScienceGenerator.instantiate(estimator, static, namespace, config, time.map(_.science))
