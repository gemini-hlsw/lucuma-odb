// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import eu.timepit.refined.types.numeric.PosInt
import fs2.Chunk
import fs2.Pure
import fs2.Stream
import lucuma.core.data.Zipper
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuOther.Acquisition
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepGuideState
import lucuma.core.model.Observation
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.itc.IntegrationTime
import lucuma.odb.data.ItcResult as Result
import lucuma.odb.data.ItcScience.GnirsImaging
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.TelescopeConfigGenerator
import lucuma.odb.sequence.imaging.ExposuresPerOffset
import lucuma.odb.sequence.imaging.ImagingSequence
import lucuma.odb.sequence.imaging.Variant

import java.util.UUID

object Science:

  object ProtoSequences extends GnirsSequenceState:

    def setup(config: Config): State[GnirsDynamicConfig, Unit] =
      for
        // The initial dynamic config already has the acquisition decker,
        // acquisition mirror in, and best focus; only the acquisition FPU and the
        // mode's camera need setting.  Coadds come with the filter's ITC result.
        _ <- GnirsDynamicConfig.fpu    := GnirsFpu.Other(Acquisition)
        _ <- GnirsDynamicConfig.camera := config.camera
      yield ()

    def runSetup(config: Config): GnirsDynamicConfig =
      setup(config).runS(initialDynamicConfig).value

    // The explicit read mode if set, else the ITC's (its choice in S/N mode, the requested
    // default in time-and-count mode).
    def setFilter(config: Config, filter: GnirsFilter, time: IntegrationTime, readMode: GnirsReadMode): State[GnirsDynamicConfig, Unit] =
      for
        _ <- GnirsDynamicConfig.filter   := filter
        _ <- GnirsDynamicConfig.exposure := time.exposureTime
        _ <- GnirsDynamicConfig.coadds   := time.coadds
        _ <- GnirsDynamicConfig.readMode := config.explicitReadMode.getOrElse(readMode)
      yield ()

    def grouped(
      config:             Config,
      time:               NonEmptyMap[GnirsFilter, Zipper[Result]],
      readModes:          NonEmptyMap[GnirsFilter, GnirsReadMode],
      offsets:            List[TelescopeConfig],
      skyOffsets:         List[TelescopeConfig],
      exposuresPerOffset: PosInt
    )(using Order[GnirsFilter]): Stream[Pure, ProtoAtom[ProtoStep[GnirsDynamicConfig]]] =

      def oneFilter(filter: GnirsFilter): State[GnirsDynamicConfig, Stream[Pure, ProtoAtom[ProtoStep[GnirsDynamicConfig]]]] =
        val integrationTime = time(filter).get.focus._2
        for
          _   <- setFilter(config, filter, integrationTime, readModes(filter).get)
          sky <- skyOffsets
                   .traverse: offset =>
                     scienceStep(offset, ObserveClass.Science)
          obj <- ExposuresPerOffset.repeat(offsets, exposuresPerOffset, integrationTime.frameCount.value)
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
      config:             Config,
      time:               NonEmptyMap[GnirsFilter, Zipper[Result]],
      readModes:          NonEmptyMap[GnirsFilter, GnirsReadMode],
      offsets:            List[TelescopeConfig],
      skyOffsets:         List[TelescopeConfig],
      exposuresPerOffset: PosInt
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
            _ <- setFilter(config, filter, filterTimes(filter), readModes(filter).get)
            s <- scienceStep(offset, ObserveClass.Science)
          yield s

      val plan: List[List[(GnirsFilter, Int)]] =
        ExposuresPerOffset.interleave(filters.fproduct(filterTimes(_).frameCount.value), exposuresPerOffset)

      // the dynamic config and size of each stop in a group
      def oneGroup(group: List[(GnirsFilter, Int)]): State[GnirsDynamicConfig, List[(GnirsDynamicConfig, Int)]] =
        group.traverse: (filter, size) =>
          for
            _ <- setFilter(config, filter, filterTimes(filter), readModes(filter).get)
            d <- State.get
          yield (d, size)

      val groups: State[GnirsDynamicConfig, Stream[Pure, Chunk[ProtoStep[GnirsDynamicConfig]]]] =
        plan
          .traverse(oneGroup)
          .map: lst =>
            Stream
              .emits(lst.zipWithIndex.flatMap((g, i) => g.tupleRight(i)))
              .zip(Stream.emits(offsets)) // one offset per stop
              .flatMap:
                case (((d, size), i), tc) =>
                  Stream.emits(List.fill(size)(ProtoStep(d, StepConfig.Science, tc, ObserveClass.Science) -> i))
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
      observationId: Observation.Id,
      estimator:     StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
      static:        GnirsStaticConfig,
      namespace:     UUID,
      config:        Config,
      time:          Either[OdbError, NonEmptyMap[GnirsFilter, Zipper[Result]]]
    )(using Order[GnirsFilter]): F[Either[OdbError, SequenceGenerator[GnirsDynamicConfig]]] =

      // Every filter's ITC result carries the read mode its steps take.
      val resolved: Either[OdbError, (NonEmptyMap[GnirsFilter, Zipper[Result]], NonEmptyMap[GnirsFilter, GnirsReadMode])] =
        time.flatMap: nem =>
          nem.toNel
            .traverse((filter, zipper) => itcReadMode(zipper.focus._2).bimap(sequenceError(observationId, _), filter -> _))
            .map(readModes => (nem, readModes.toNem))

      resolved.traverse: (nem, readModes) =>

        // Pre-generate enough object and sky offsets to cover the sequence.
        def generateOffsets(
          offsets0:   TelescopeConfigGenerator,
          skyCount:   NonNegInt,
          skyOffsets: TelescopeConfigGenerator
        )(f: List[Int] => Int): F[(List[TelescopeConfig], List[TelescopeConfig])] =

          val cnt     = NonNegInt.unsafeFrom(f(nem.toNel.toList.map((_,  zipper) => zipper.focus._2.frameCount.value)))
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
          case Variant.Grouped(_, offsets, skyCount, skyOffsets, exposuresPerOffset) =>
            // The same pattern is repeated for each filter so we need enough
            // offsets to cover the filter with the most stops.
            generateOffsets(offsets, skyCount, skyOffsets)(ExposuresPerOffset.groupedOffsetCount(_, exposuresPerOffset)).map: (o, s) =>
              ProtoSequences.grouped(config, nem, readModes, o, s, exposuresPerOffset)

          case Variant.Interleaved(offsets, skyCount, skyOffsets, exposuresPerOffset) =>
            // The offset pattern is applied to the sequence as a whole so we
            // need an offset per stop. Also there are skyCount sky positions
            // per filter.
            val sc = NonNegInt.unsafeFrom(skyCount.value * nem.length)
            generateOffsets(offsets, sc, skyOffsets)(ExposuresPerOffset.interleavedOffsetCount(_, exposuresPerOffset)).map: (o, s) =>
              ProtoSequences.interleaved(config, nem, readModes, o, s, exposuresPerOffset)

          case Variant.PreImaging(o1, o2, o3, o4, exposuresPerOffset) =>
            // Pre-imaging is grouped imaging over the four named positions,
            // cycled as needed.
            val offsets    = TelescopeConfigGenerator.Enumerated:
              NonEmptyList.of(o1, o2, o3, o4).map(o => TelescopeConfig(o, StepGuideState.Enabled))
            val skyOffsets = TelescopeConfigGenerator.NoGenerator
            generateOffsets(offsets, NonNegInt.MinValue, skyOffsets)(ExposuresPerOffset.groupedOffsetCount(_, exposuresPerOffset)).map: (o, s) =>
              ProtoSequences.grouped(config, nem, readModes, o, s, exposuresPerOffset)

        protoAtoms.map: v =>
          ImagingSequence.makeGenerator(estimator, static, namespace, v)

  def gnirs[F[_]: Sync](
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
    static:        GnirsStaticConfig,
    namespace:     UUID,
    config:        Config,
    time:          Either[OdbError, GnirsImaging]
  ): F[Either[OdbError, SequenceGenerator[GnirsDynamicConfig]]] =
    given Order[GnirsFilter] = ImagingSequence.wavelengthOrder(config.variant)(_.centralWavelength)
    ScienceGenerator.instantiate(observationId, estimator, static, namespace, config, time.map(_.science))
