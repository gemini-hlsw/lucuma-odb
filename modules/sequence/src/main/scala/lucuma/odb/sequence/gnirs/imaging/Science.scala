// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gnirs
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
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuOther.Acquisition
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepGuideState
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.ItcResult as Result
import lucuma.odb.data.ItcScience.GnirsImaging
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.TelescopeConfigGenerator
import lucuma.odb.sequence.imaging.ImagingSequence
import lucuma.odb.sequence.imaging.Variant

import java.util.UUID

object Science:

  object ProtoSequences extends GnirsSequenceState:

    def setup(config: Config): State[GnirsDynamicConfig, Unit] =
      for
        // The initial dynamic config already has the acquisition decker,
        // acquisition mirror in, and best focus; only the acquisition FPU and the
        // mode's camera and coadds need setting.
        _ <- GnirsDynamicConfig.fpu    := GnirsFpu.Other(Acquisition)
        _ <- GnirsDynamicConfig.camera := config.camera
        _ <- GnirsDynamicConfig.coadds := config.coadds
      yield ()

    def runSetup(config: Config): GnirsDynamicConfig =
      setup(config).runS(initialDynamicConfig).value

    // The read mode follows the exposure time unless explicitly overridden.
    def setFilter(config: Config, filter: GnirsFilter, time: TimeSpan): State[GnirsDynamicConfig, Unit] =
      for
        _ <- GnirsDynamicConfig.filter   := filter
        _ <- GnirsDynamicConfig.exposure := time
        _ <- GnirsDynamicConfig.readMode := config.explicitReadMode.getOrElse(GnirsReadMode.forExposureTime(time))
      yield ()

    def grouped(
      config:     Config,
      time:       NonEmptyMap[GnirsFilter, Zipper[Result]],
      offsets:    List[TelescopeConfig],
      skyOffsets: List[TelescopeConfig]
    )(using Order[GnirsFilter]): Stream[Pure, ProtoAtom[ProtoStep[GnirsDynamicConfig]]] =

      def oneFilter(filter: GnirsFilter): State[GnirsDynamicConfig, Stream[Pure, ProtoAtom[ProtoStep[GnirsDynamicConfig]]]] =
        val integrationTime = time(filter).get.focus._2
        for
          _   <- setFilter(config, filter, integrationTime.exposureTime)
          sky <- skyOffsets
                   .traverse: offset =>
                     scienceStep(offset, ObserveClass.Science)
          obj <- offsets.take(integrationTime.exposureCount.value)
                   .traverse: offset =>
                     scienceStep(offset, ObserveClass.Science)
        yield
          // If there are no sky positions, then each exposure is an atom.
          // If there are sky positions, then we group them into a single atom.
          if sky.isEmpty then Stream.emits(obj.map(ProtoAtom.one(none, _)))
          else Stream.emit(ProtoAtom(none, NonEmptyList.fromListUnsafe(sky ++ obj ++ sky)))

      Stream
        .emits(time.keys.toNonEmptyList.toList.sorted)
        .mapAccumulate(runSetup(config)): (cur, filter) =>
          oneFilter(filter).run(cur).value
        .map(_._2)
        .flatten

    def interleaved(
      config:     Config,
      time:       NonEmptyMap[GnirsFilter, Zipper[Result]],
      offsets:    List[TelescopeConfig],
      skyOffsets: List[TelescopeConfig]
    )(using Order[GnirsFilter]): Stream[Pure, ProtoAtom[ProtoStep[GnirsDynamicConfig]]] =

      val filterTimes: Map[GnirsFilter, IntegrationTime] =
        time.toNel.toList.map((filter, zipper) => filter -> zipper.focus._2).toMap

      val filters = filterTimes.keys.toList.sorted

      val skyList: List[(GnirsFilter, TelescopeConfig)] =
        val n = skyOffsets.size / filters.size
        filters.flatMap(f => List.fill(n)(f)).zip(skyOffsets)

      def skySteps(skyList: List[(GnirsFilter, TelescopeConfig)]): State[GnirsDynamicConfig, List[ProtoStep[GnirsDynamicConfig]]] =
        skyList.traverse: (filter, offset) =>
          for
            _ <- setFilter(config, filter, filterTimes(filter).exposureTime)
            s <- scienceStep(offset, ObserveClass.Science)
          yield s

      // number of groups of interleaved filters
      val groupCount: Int =
        filterTimes.values.map(_.exposureCount.value).min

      def oneGroup(n: Int): State[GnirsDynamicConfig, Stream[Pure, GnirsDynamicConfig]] =
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
              _ <- setFilter(config, filter, filterTimes(filter).exposureTime)
              d <- State.get
            yield Stream.emits(List.fill(perFilterCounts(filter))(d))
          .map(lst => Stream.emits(lst).flatten)

      val groups: State[GnirsDynamicConfig, Stream[Pure, Chunk[ProtoStep[GnirsDynamicConfig]]]] =
        (0 until groupCount)
          .toList
          .traverse(oneGroup)
          .map: lst =>
            Stream
              .emits(lst.zipWithIndex.map(_.tupleRight(_)))
              .flatten
              .zip(Stream.emits(offsets))
              .map:
                case ((d, i), tc) =>
                  ProtoStep(d, StepConfig.Science, tc, ObserveClass.Science) -> i
              .groupAdjacentBy(_._2)
              .map(_._2.map(_._1))

      val prog = for
        _ <- setup(config)
        b <- skySteps(skyList)
        g <- groups
        a <- skySteps(skyList.reverse)
      yield g.flatMap: chunk =>
        if skyList.isEmpty then
          Stream.emits(chunk.toList.map(ProtoAtom.one(none, _)))
        else
          Stream.emit(ProtoAtom(none, NonEmptyList.fromListUnsafe(b ++ chunk.toList ++ a)))

      prog.runA(initialDynamicConfig).value

  object ScienceGenerator:

    def instantiate[F[_]: Sync](
      estimator: StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
      static:    GnirsStaticConfig,
      namespace: UUID,
      config:    Config,
      time:      Either[OdbError, NonEmptyMap[GnirsFilter, Zipper[Result]]]
    )(using Order[GnirsFilter]): F[Either[OdbError, SequenceGenerator[GnirsDynamicConfig]]] =

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

  def gnirs[F[_]: Sync](
    estimator: StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
    static:    GnirsStaticConfig,
    namespace: UUID,
    config:    Config,
    time:      Either[OdbError, GnirsImaging]
  ): F[Either[OdbError, SequenceGenerator[GnirsDynamicConfig]]] =
    given Order[GnirsFilter] = ImagingSequence.wavelengthOrder(config.variant)(_.centralWavelength)
    ScienceGenerator.instantiate(estimator, static, namespace, config, time.map(_.science))
