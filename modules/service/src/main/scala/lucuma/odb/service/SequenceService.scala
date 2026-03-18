// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.effect.std.UUIDGen
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pipe
import fs2.Pure
import fs2.Stream
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.AtomDigest
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth as GmosNorthStatic
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth as GmosSouthStatic
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.core.util.TimeSpan
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.mapping.AccessControl.CheckedWithId
import lucuma.odb.logic.Generator.SequenceAtomLimit
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.TimeEstimateCalculator
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StreamingExecutionConfig
import lucuma.odb.sequence.util.AtomBuilder
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.*
import lucuma.odb.util.GmosCodecs.*
import lucuma.odb.util.Igrins2Codecs.*
import skunk.*
import skunk.codec.boolean.bool
import skunk.codec.numeric.int2
import skunk.codec.numeric.int4
import skunk.codec.text.text
import skunk.implicits.*

import java.util.UUID

import Services.Syntax.*

trait SequenceService[F[_]]:

  def insertAtomDigests(
    observationId: Observation.Id,
    digests:       Stream[F, AtomDigest]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def selectAtomDigests(
    which: List[Observation.Id]
  )(using Transaction[F], Services.ServiceAccess): Stream[F, (Observation.Id, Short, AtomDigest)]

  def isMaterialized(
    observationId: Observation.Id,
    sequenceType:  SequenceType
  )(using Transaction[F]): F[Boolean]

  def replaceFlamingos2Sequence(
    observationId:  Observation.Id,
    sequenceType:   SequenceType,
    sequence:       List[ProtoAtom[ProtoStep[Flamingos2DynamicConfig]]],
    namespace:      Option[UUID] = None
  )(using Transaction[F]): F[Result[Stream[Pure, Atom[Flamingos2DynamicConfig]]]]

  def replaceFlamingos2Sequence(
    checked: CheckedWithId[(SequenceType, List[ProtoAtom[ProtoStep[Flamingos2DynamicConfig]]]), Observation.Id]
  )(using Transaction[F]): F[Result[Stream[Pure, Atom[Flamingos2DynamicConfig]]]]

  def replaceGmosNorthSequence(
    observationId:  Observation.Id,
    sequenceType:   SequenceType,
    sequence:       List[ProtoAtom[ProtoStep[GmosNorth]]],
    namespace:      Option[UUID] = None
  )(using Transaction[F]): F[Result[Stream[Pure, Atom[GmosNorth]]]]

  def replaceGmosNorthSequence(
    checked: CheckedWithId[(SequenceType, List[ProtoAtom[ProtoStep[GmosNorth]]]), Observation.Id]
  )(using Transaction[F]): F[Result[Stream[Pure, Atom[GmosNorth]]]]

  def replaceGmosSouthSequence(
    observationId:  Observation.Id,
    sequenceType:   SequenceType,
    sequence:       List[ProtoAtom[ProtoStep[GmosSouth]]],
    namespace:      Option[UUID] = None
  )(using Transaction[F]): F[Result[Stream[Pure, Atom[GmosSouth]]]]

  def replaceGmosSouthSequence(
    checked: CheckedWithId[(SequenceType, List[ProtoAtom[ProtoStep[GmosSouth]]]), Observation.Id]
  )(using Transaction[F]): F[Result[Stream[Pure, Atom[GmosSouth]]]]

  def resetFlamingos2Acquisition(
    observationId: Observation.Id,
    sequence:      Stream[F, Atom[Flamingos2DynamicConfig]]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def resetGmosNorthAcquisition(
    observationId: Observation.Id,
    sequence:      Stream[F, Atom[GmosNorth]]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def resetGmosSouthAcquisition(
    observationId: Observation.Id,
    sequence:      Stream[F, Atom[GmosSouth]]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def insertFlamingos2Sequence(
    observationId: Observation.Id,
    sequenceType:  SequenceType,
    sequence:      Stream[F, Atom[Flamingos2DynamicConfig]]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def insertGmosNorthSequence(
    observationId: Observation.Id,
    sequenceType:  SequenceType,
    sequence:      Stream[F, Atom[GmosNorth]]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def insertGmosSouthSequence(
    observationId: Observation.Id,
    sequenceType:  SequenceType,
    sequence:      Stream[F, Atom[GmosSouth]]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def materializeFlamingos2ExecutionConfig(
    observationId: Observation.Id,
    stream:        StreamingExecutionConfig[F, Flamingos2StaticConfig, Flamingos2DynamicConfig]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def materializeGmosNorthExecutionConfig(
    observationId: Observation.Id,
    stream:        StreamingExecutionConfig[F, GmosNorthStatic, GmosNorth]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def materializeGmosSouthExecutionConfig(
    observationId: Observation.Id,
    stream:        StreamingExecutionConfig[F, GmosSouthStatic, GmosSouth]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def selectFlamingos2Sequence(
    observationId: Observation.Id,
    sequenceType:  SequenceType,
    staticConfig:  Flamingos2StaticConfig
  )(using Transaction[F]): F[Option[Stream[F, Atom[Flamingos2DynamicConfig]]]]

  def selectGmosNorthSequence(
    observationId: Observation.Id,
    sequenceType:  SequenceType,
    staticConfig:  GmosNorthStatic
  )(using Transaction[F]): F[Option[Stream[F, Atom[GmosNorth]]]]

  def selectGmosSouthSequence(
    observationId: Observation.Id,
    sequenceType:  SequenceType,
    staticConfig:  GmosSouthStatic
  )(using Transaction[F]): F[Option[Stream[F, Atom[GmosSouth]]]]

  // IGRINS-2 Spectrosocpy
  def insertIgrins2Sequence(
    observationId: Observation.Id,
    sequenceType:  SequenceType,
    sequence:      Stream[F, Atom[Igrins2DynamicConfig]]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def materializeIgrins2ExecutionConfig(
    observationId: Observation.Id,
    stream:        StreamingExecutionConfig[F, Igrins2StaticConfig, Igrins2DynamicConfig]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def selectIgrins2Sequence(
    observationId: Observation.Id,
    sequenceType:  SequenceType,
    staticConfig:  Igrins2StaticConfig
  )(using Transaction[F]): F[Option[Stream[F, Atom[Igrins2DynamicConfig]]]]

object SequenceService:

  def instantiate[F[_]: Concurrent: UUIDGen](
    estimator: TimeEstimateCalculatorImplementation.ForInstrumentMode
  )(using Services[F]): SequenceService[F] =
    new SequenceService[F]:

      override def insertAtomDigests(
        observationId: Observation.Id,
        digests:       Stream[F, AtomDigest]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        val insert =
          digests
            .zipWithIndex
            .map: (d, i) =>
              (observationId, i.toShort, d)
            .chunkN(256)
            .evalTap: c =>
              val lst = c.toList
              session.execute(Statements.insertAtomDigest(lst))(lst)

        for
          _ <- session.execute(Statements.DeleteAtomDigests)(observationId)
          _ <- insert.compile.drain
        yield ()

      override def selectAtomDigests(
        which: List[Observation.Id]
      )(using Transaction[F], Services.ServiceAccess): Stream[F, (Observation.Id, Short, AtomDigest)] =
        if which.isEmpty then Stream.empty
        else session.stream(Statements.selectAtomDigests(which))(which, 1024)

      /**
       * Marks ongoing steps as abandoned and deletes any steps that are
       * `not_started`.
       */
      private def abandonAndDeleteUnexecuted(
        observationId: Observation.Id,
        sequenceType:  SequenceType
      ): F[Unit] =
        session.execute(Statements.AbandonAndDeleteUnexecuted)(observationId, sequenceType).void

      override def insertFlamingos2Sequence(
        observationId:  Observation.Id,
        sequenceType:   SequenceType,
        sequence:       Stream[F, Atom[Flamingos2DynamicConfig]]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        insertSequence(
          Instrument.Flamingos2,
          observationId,
          sequenceType,
          sequence,
          Flamingos2SequenceService.Statements.InsertDynamic
        )

      override def insertGmosNorthSequence(
        observationId:  Observation.Id,
        sequenceType:   SequenceType,
        sequence:       Stream[F, Atom[GmosNorth]]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        insertSequence(
          Instrument.GmosNorth,
          observationId,
          sequenceType,
          sequence,
          GmosSequenceService.Statements.InsertGmosNorthDynamic
        )

      override def insertGmosSouthSequence(
        observationId:  Observation.Id,
        sequenceType:   SequenceType,
        sequence:       Stream[F, Atom[GmosSouth]]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        insertSequence(
          Instrument.GmosSouth,
          observationId,
          sequenceType,
          sequence,
          GmosSequenceService.Statements.InsertGmosSouthDynamic
        )

      override def insertIgrins2Sequence(
        observationId:  Observation.Id,
        sequenceType:   SequenceType,
        sequence:       Stream[F, Atom[Igrins2DynamicConfig]]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        insertSequence(
          Instrument.Igrins2,
          observationId,
          sequenceType,
          sequence,
          Igrins2SequenceService.Statements.InsertDynamic
        )

      private def insertSequence[D](
        instrument:       Instrument,
        observationId:    Observation.Id,
        sequenceType:     SequenceType,
        sequence:         Stream[F, Atom[D]],
        insertInstConfig: Command[(Step.Id, D)]
      ): F[Unit] =

        val atom: Pipe[F, Atom[D], Nothing] = atomStream =>
          atomStream
            .zipWithIndex
            .map { case (atom, idx) =>
              (atom.id, observationId, sequenceType, instrument, idx.toInt, atom.description.map(_.value))
            }
            .through(session.pipe(Statements.insertAtom))
            .drain

        val step: Pipe[F, Atom[D], Nothing] = atomStream =>
          atomStream
            .flatMap: atom =>
              Stream.emits(
                atom.steps.toList.zipWithIndex.tupleLeft(atom.id).map { case (aid, (step, idx)) =>
                  (step, aid, instrument, idx)
                }
              )
            .through(session.pipe(Statements.insertStep[D]))
            .drain

        val gcal: Pipe[F, Atom[D], Nothing] = atomStream =>
          atomStream
            .flatMap: atom =>
              Stream.emits(
                atom.steps.toList.flatMap: step =>
                  StepConfig.gcal.getOption(step.stepConfig).tupleLeft(step.id)
              )
            .through(session.pipe(Statements.InsertStepConfigGcal))
            .drain

        val instrumentConfig: Pipe[F, Atom[D], Nothing] = atomStream =>
          atomStream
            .flatMap: atom =>
              Stream.emits(
                atom.steps.toList.map: step =>
                  (step.id, step.instrumentConfig)
              )
            .through(session.pipe(insertInstConfig))
            .drain

        sequence
          .broadcastThrough(atom, step, gcal, instrumentConfig)
          .compile
          .drain

      extension [D](p: ProtoStep[D])
        def toStep(sid: Step.Id, estimate: StepEstimate): Step[D] =
          Step(sid, p.value, p.stepConfig, p.telescopeConfig, estimate, p.observeClass, p.breakpoint)

      // Turns a stream of ProtoStep into an atom by running the time estimation
      // and grouping the steps by atom id.
      private def atomPipe[S, D](
        static:    S,
        estimator: TimeEstimateCalculator[S, D]
      ): Pipe[F, (Atom.Id, Option[String], Step.Id, ProtoStep[D]), Atom[D]] =
        _.mapAccumulate(TimeEstimateCalculator.Last.empty[D]) {
          case (last, (aid, desc, sid, protoStep)) =>
            val (lastʹ, estimate) = estimator.estimateOne(static, protoStep).run(last).value
            (lastʹ, (aid, desc, protoStep.toStep(sid, estimate)))
        }
        .map(_._2)
        .groupAdjacentBy(_._1)
        .map: (aid, chunk) =>
          Atom(
            aid,
            chunk.head.flatMap(_._2).flatMap(NonEmptyString.from(_).toOption),
            NonEmptyList.fromListUnsafe(chunk.map(_._3).toList)
          )

      override def isMaterialized(
        observationId: Observation.Id,
        sequenceType:  SequenceType
      )(using Transaction[F]): F[Boolean] =
        session.unique(Statements.IsMaterialized)(observationId, sequenceType)

      /**
       * Marks the sequence as materialized, or if already materialized does
       * nothing.
       *
       * @return `true` if a new row was inserted, `false` if nothing changed
       */
      private def markMaterializedOrDoNothing(
        observationId: Observation.Id,
        sequenceType:  SequenceType
      ): F[Boolean] =
        session.unique(Statements.MarkMaterializedOrDoNothing)(observationId, sequenceType)

      /**
       * Marks the sequence as materialized, or updates the timestamp of the
       * last materialization.
       *
       * @return `true` if a new row was inserted, `false` if an existing row
       *         was updated
       */
      private def markMaterializedOrUpdate(
        observationId: Observation.Id,
        sequenceType:  SequenceType
      ): F[Boolean] =
        session.unique(Statements.MarkMaterializedOrUpdate)(observationId, sequenceType)

      private def atomBuilder[S, D](
        sequenceType: SequenceType,
        static:       S,
        namespace:    Option[UUID],
        estimator:    TimeEstimateCalculator[S, D]
      ): F[AtomBuilder[D]] =
        namespace
          .fold(UUIDGen[F].randomUUID)(_.pure[F])
          .map: uuid =>
            AtomBuilder.instantiate(estimator, static, uuid, sequenceType)

      private def replaceSequence[D](
        instrument:       Instrument,
        observationId:    Observation.Id,
        sequenceType:     SequenceType,
        sequence:         List[ProtoAtom[ProtoStep[D]]],
        insertInstConfig: Command[(Step.Id, D)],
        atomBuilder:      AtomBuilder[D]
      ): ResultT[F, Stream[Pure, Atom[D]]] =

        val checkLength =
          if sequence.lengthIs <= SequenceAtomLimit then ResultT.unit
          else ResultT:
            OdbError
              .InvalidArgument(s"Execution sequences containing over $SequenceAtomLimit atoms are not supported.".some)
              .asFailureF

        val doReplace: F[Stream[Pure, Atom[D]]] =
          val atoms = atomBuilder.buildStream(Stream.emits(sequence))

          for
            _ <- markMaterializedOrUpdate(observationId, sequenceType)
            _ <- abandonAndDeleteUnexecuted(observationId, sequenceType)
            _ <- insertSequence(instrument, observationId, sequenceType, atoms.covary[F], insertInstConfig)
          yield atoms

        checkLength *> ResultT.liftF(doReplace)

      private def selectStatic[S](
        observationId: Observation.Id,
        expected:      String,
        select:        (Observation.Id) => Transaction[F] ?=> F[Option[S]]
      )(using Transaction[F]): ResultT[F, S] =
        ResultT:
          select(observationId)
            .map: static =>
              Result.fromOption(
                static,
                OdbError
                  .InvalidArgument(s"Observation $observationId not found or is not a $expected observation.".some)
                  .asProblem
              )

      override def replaceFlamingos2Sequence(
        observationId: Observation.Id,
        sequenceType:  SequenceType,
        sequence:      List[ProtoAtom[ProtoStep[Flamingos2DynamicConfig]]],
        namespace:     Option[UUID] = None
      )(using Transaction[F]): F[Result[Stream[Pure, Atom[Flamingos2DynamicConfig]]]] =

        (for
          s <- selectStatic(observationId, "Flamingos 2", flamingos2SequenceService.selectStatic)
          b <- ResultT.liftF(atomBuilder(sequenceType, s, namespace, estimator.flamingos2))
          r <- replaceSequence(
                 Instrument.Flamingos2,
                 observationId,
                 sequenceType,
                 sequence,
                 Flamingos2SequenceService.Statements.InsertDynamic,
                 b
               )
        yield r).value

      override def replaceFlamingos2Sequence(
        checked: CheckedWithId[(SequenceType, List[ProtoAtom[ProtoStep[Flamingos2DynamicConfig]]]), Observation.Id]
      )(using Transaction[F]): F[Result[Stream[Pure, Atom[Flamingos2DynamicConfig]]]] =
        checked.foldWithId(OdbError.InvalidArgument().asFailureF[F, Stream[Pure, Atom[Flamingos2DynamicConfig]]]) { case ((sequenceType, sequence), oid) =>
          replaceFlamingos2Sequence(
            oid,
            sequenceType,
            sequence
          )
        }

      override def replaceGmosNorthSequence(
        observationId: Observation.Id,
        sequenceType:  SequenceType,
        sequence:      List[ProtoAtom[ProtoStep[GmosNorth]]],
        namespace:     Option[UUID] = None
      )(using Transaction[F]): F[Result[Stream[Pure, Atom[GmosNorth]]]] =
        (for
          s <- selectStatic(observationId, "GMOS North", gmosSequenceService.selectGmosNorthStatic)
          b <- ResultT.liftF(atomBuilder(sequenceType, s, namespace, estimator.gmosNorth))
          r <- replaceSequence(
                 Instrument.GmosNorth,
                 observationId,
                 sequenceType,
                 sequence,
                 GmosSequenceService.Statements.InsertGmosNorthDynamic,
                 b
               )
        yield r).value

      override def replaceGmosNorthSequence(
        checked: CheckedWithId[(SequenceType, List[ProtoAtom[ProtoStep[GmosNorth]]]), Observation.Id]
      )(using Transaction[F]): F[Result[Stream[Pure, Atom[GmosNorth]]]] =
        checked.foldWithId(OdbError.InvalidArgument().asFailureF[F, Stream[Pure, Atom[GmosNorth]]]) { case ((sequenceType, sequence), oid) =>
          replaceGmosNorthSequence(
            oid,
            sequenceType,
            sequence
          )
        }

      override def replaceGmosSouthSequence(
        observationId: Observation.Id,
        sequenceType:  SequenceType,
        sequence:      List[ProtoAtom[ProtoStep[GmosSouth]]],
        namespace:     Option[UUID] = None
      )(using Transaction[F]): F[Result[Stream[Pure, Atom[GmosSouth]]]] =
        (for
          s <- selectStatic(observationId, "GMOS South", gmosSequenceService.selectGmosSouthStatic)
          b <- ResultT.liftF(atomBuilder(sequenceType, s, namespace, estimator.gmosSouth))
          r <- replaceSequence(
                 Instrument.GmosSouth,
                 observationId,
                 sequenceType,
                 sequence,
                 GmosSequenceService.Statements.InsertGmosSouthDynamic,
                 b
               )
        yield r).value


      override def replaceGmosSouthSequence(
        checked: CheckedWithId[(SequenceType, List[ProtoAtom[ProtoStep[GmosSouth]]]), Observation.Id]
      )(using Transaction[F]): F[Result[Stream[Pure, Atom[GmosSouth]]]] =
        checked.foldWithId(OdbError.InvalidArgument().asFailureF[F, Stream[Pure, Atom[GmosSouth]]]) { case ((sequenceType, sequence), oid) =>
          replaceGmosSouthSequence(
            oid,
            sequenceType,
            sequence
          )
        }

      private def resetAcquisition[D](
        observationId: Observation.Id,
        stream:        Stream[F, Atom[D]]
      )(
        insert: (Observation.Id, SequenceType, Stream[F, Atom[D]]) => F[Unit]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        val reset = for
          _ <- markMaterializedOrUpdate(observationId, SequenceType.Acquisition)
          _ <- abandonAndDeleteUnexecuted(observationId, SequenceType.Acquisition)
          _ <- insert(observationId, SequenceType.Acquisition, stream)
        yield ()

        isMaterialized(observationId, SequenceType.Acquisition).ifM(
          reset,
          Applicative[F].unit
        )

      override def resetFlamingos2Acquisition(
        observationId: Observation.Id,
        stream:        Stream[F, Atom[Flamingos2DynamicConfig]]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        resetAcquisition(observationId, stream)(insertFlamingos2Sequence)

      override def resetGmosNorthAcquisition(
        observationId: Observation.Id,
        stream:        Stream[F, Atom[GmosNorth]]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        resetAcquisition(observationId, stream)(insertGmosNorthSequence)

      override def resetGmosSouthAcquisition(
        observationId: Observation.Id,
        stream:        Stream[F, Atom[GmosSouth]]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        resetAcquisition(observationId, stream)(insertGmosSouthSequence)

      private def materializeExecutionConfig[S, D](
        observationId: Observation.Id,
        stream:        StreamingExecutionConfig[F, S, D]
      )(
        insert: (Observation.Id, SequenceType, Stream[F, Atom[D]]) => F[Unit]
      )(using Services.ServiceAccess): F[Unit] =

        def materialize(sequenceType: SequenceType, s: Stream[F, Atom[D]]): F[Unit] =
          markMaterializedOrDoNothing(observationId, sequenceType).ifM(
            insert(observationId, sequenceType, s),
            Applicative[F].unit
          )

        materialize(SequenceType.Acquisition, stream.acquisition) *>
        materialize(SequenceType.Science, stream.science)

      override def materializeFlamingos2ExecutionConfig(
        observationId: Observation.Id,
        stream:        StreamingExecutionConfig[F, Flamingos2StaticConfig, Flamingos2DynamicConfig]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        materializeExecutionConfig(observationId, stream)(insertFlamingos2Sequence)

      override def materializeGmosNorthExecutionConfig(
        observationId: Observation.Id,
        stream:        StreamingExecutionConfig[F, GmosNorthStatic, GmosNorth]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        materializeExecutionConfig(observationId, stream)(insertGmosNorthSequence)

      override def materializeGmosSouthExecutionConfig(
        observationId: Observation.Id,
        stream:        StreamingExecutionConfig[F, GmosSouthStatic, GmosSouth]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        materializeExecutionConfig(observationId, stream)(insertGmosSouthSequence)

      override def materializeIgrins2ExecutionConfig(
        observationId: Observation.Id,
        stream:        StreamingExecutionConfig[F, Igrins2StaticConfig, Igrins2DynamicConfig]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        materializeExecutionConfig(observationId, stream)(insertIgrins2Sequence)

      private def selectSequence[S, D](
        instrument:    Instrument,
        observationId: Observation.Id,
        sequenceType:  SequenceType,
        query:         Query[(Instrument, Observation.Id, SequenceType), (Atom.Id, Option[String], Step.Id, ProtoStep[D])],
        staticConfig:  S,
        estimator:     TimeEstimateCalculator[S, D]
      )(using Transaction[F]): F[Option[Stream[F, Atom[D]]]] =
        isMaterialized(observationId, sequenceType).ifF(
          session
            .stream(query)((instrument, observationId, sequenceType), 256)
            .through(atomPipe(staticConfig, estimator))
            .some,
          none
        )

      override def selectFlamingos2Sequence(
        observationId: Observation.Id,
        sequenceType:  SequenceType,
        staticConfig:  Flamingos2StaticConfig
      )(using Transaction[F]): F[Option[Stream[F, Atom[Flamingos2DynamicConfig]]]] =
        selectSequence(
          Instrument.Flamingos2,
          observationId,
          sequenceType,
          Statements.SelectFlamingos2Sequence,
          staticConfig,
          estimator.flamingos2
        )

      override def selectGmosNorthSequence(
        observationId: Observation.Id,
        sequenceType:  SequenceType,
        staticConfig:  GmosNorthStatic
      )(using Transaction[F]): F[Option[Stream[F, Atom[GmosNorth]]]] =
        selectSequence(
          Instrument.GmosNorth,
          observationId,
          sequenceType,
          Statements.SelectGmosNorthSequence,
          staticConfig,
          estimator.gmosNorth
        )

      override def selectGmosSouthSequence(
        observationId: Observation.Id,
        sequenceType:  SequenceType,
        staticConfig:  GmosSouthStatic
      )(using Transaction[F]): F[Option[Stream[F, Atom[GmosSouth]]]] =
        selectSequence(
          Instrument.GmosSouth,
          observationId,
          sequenceType,
          Statements.SelectGmosSouthSequence,
          staticConfig,
          estimator.gmosSouth
        )

      override def selectIgrins2Sequence(
        observationId: Observation.Id,
        sequenceType:  SequenceType,
        staticConfig:  Igrins2StaticConfig
      )(using Transaction[F]): F[Option[Stream[F, Atom[Igrins2DynamicConfig]]]] =
        selectSequence(
          Instrument.Igrins2,
          observationId,
          sequenceType,
          Statements.SelectIgrins2Sequence,
          staticConfig,
          estimator.igrins2
        )

  object Statements:

    val AbandonAndDeleteUnexecuted: Command[(Observation.Id, SequenceType)] =
      sql"""
        CALL abandon_ongoing_and_delete_unexecuted_steps($observation_id, $sequence_type)
      """.command

    val insertAtom: Command[(
      Atom.Id,
      Observation.Id,
      SequenceType,
      Instrument,
      Int,
      Option[String]
    )] =
      sql"""
        INSERT INTO t_atom (
          c_atom_id,
          c_observation_id,
          c_sequence_type,
          c_instrument,
          c_atom_index,
          c_description
        ) SELECT
          $atom_id,
          $observation_id,
          $sequence_type,
          $instrument,
          $int4,
          ${text.opt}
      """.command

    def insertStep[D]: Command[(
      Step[D],
      Atom.Id,
      Instrument,
      Int
    )] =
      sql"""
        INSERT INTO t_step (
          c_step_id,
          c_atom_id,
          c_instrument,
          c_step_type,
          c_step_index,
          c_observe_class,
          c_time_estimate,
          c_offset_p,
          c_offset_q,
          c_guide_state,
          c_breakpoint
        ) SELECT
          $step_id,
          $atom_id,
          $instrument,
          $step_type,
          $int4,
          $obs_class,
          $time_span,
          $telescope_config,
          $breakpoint
      """.command.contramap { (step, aid, inst, idx) => (
        step.id,
        aid,
        inst,
        step.stepConfig.stepType,
        idx,
        step.observeClass,
        step.estimate.total,
        step.telescopeConfig,
        step.breakpoint
      )}

    private def insertStepConfigFragment(table: String, columns: List[String]): Fragment[Void] =
      sql"""
        INSERT INTO #$table (
          c_step_id,
          #${encodeColumns(none, columns)}
        )
      """

    private val StepConfigGcalColumns: List[String] =
      List(
        "c_gcal_continuum",
        "c_gcal_ar_arc",
        "c_gcal_cuar_arc",
        "c_gcal_thar_arc",
        "c_gcal_xe_arc",
        "c_gcal_filter",
        "c_gcal_diffuser",
        "c_gcal_shutter"
      )

    val InsertStepConfigGcal: Command[(Step.Id, StepConfig.Gcal)] =
      sql"""
        ${insertStepConfigFragment("t_step_config_gcal", StepConfigGcalColumns)} SELECT
          $step_id,
          $step_config_gcal
      """.command

    private val StepConfigSmartGcalColumns: List[String] =
      List(
        "c_smart_gcal_type"
      )

    val InsertStepConfigSmartGcal: Command[(Step.Id, StepConfig.SmartGcal)] =
      sql"""
        ${insertStepConfigFragment("t_step_config_smart_gcal", StepConfigSmartGcalColumns)} SELECT
          $step_id,
          $step_config_smart_gcal
      """.command

    private val step_config: Codec[StepConfig] =
      (
        step_type               *:
        step_config_gcal.opt    *:
        step_config_smart_gcal.opt
      ).eimap { case (stepType, oGcal, oSmart) =>
        stepType match {
          case StepType.Bias      => StepConfig.Bias.asRight
          case StepType.Dark      => StepConfig.Dark.asRight
          case StepType.Gcal      => oGcal.toRight("Missing gcal step config definition")
          case StepType.Science   => StepConfig.Science.asRight
          case StepType.SmartGcal => oSmart.toRight("Missing smart gcal step config definition")
        }
      } { stepConfig =>
        (stepConfig.stepType,
         StepConfig.gcal.getOption(stepConfig),
         StepConfig.smartGcal.getOption(stepConfig)
        )
      }

    val DeleteAtomDigests: Command[Observation.Id] =
      sql"""
        DELETE FROM t_atom_digest WHERE c_observation_id = $observation_id
      """.command

    val atom_digest: Codec[AtomDigest] = (
      atom_id        *:
      obs_class      *:
      time_span      *:
      time_span      *:
      _step_type     *:
      _gcal_lamp_type
    ).imap { case (a, c, n, p, ss, ls) =>
      AtomDigest(
        a,
        c,
        CategorizedTime(ChargeClass.NonCharged -> n, ChargeClass.Program -> p),
        ss.toSet,
        ls.toSet
      )
    } { (a: AtomDigest) => (
      a.id,
      a.observeClass,
      a.timeEstimate.nonCharged,
      a.timeEstimate.programTime,
      a.stepTypes.toList.sorted,
      a.lampTypes.toList.sorted
    )}

    val atom_digest_row: Codec[(Observation.Id, Short, AtomDigest)] =
      observation_id *: int2 *: atom_digest

    val AtomDigestRowColumns: String =
      """
          c_observation_id,
          c_atom_index,
          c_atom_id,
          c_observe_class,
          c_non_charged_time_estimate,
          c_program_time_estimate,
          c_step_types,
          c_lamp_types
      """

    def insertAtomDigest(ds: List[(Observation.Id, Short, AtomDigest)]): Command[ds.type] =
      val enc = atom_digest_row.values.list(ds)
      sql"""
        INSERT INTO t_atom_digest (
          #$AtomDigestRowColumns
        ) VALUES $enc
      """.command

    def selectAtomDigests(which: List[Observation.Id]): Query[which.type, (Observation.Id, Short, AtomDigest)] =
      sql"""
        SELECT
          #$AtomDigestRowColumns
        FROM
          t_atom_digest
        WHERE
          c_observation_id IN ${observation_id.list(which).values}
        ORDER BY c_observation_id, c_atom_index
      """.query(atom_digest_row)

    def selectSequence[D](
      instrumentTable:   String,
      instrumentColumns: List[String],
      instrumentDecoder: Decoder[D]
    ): Query[(Instrument, Observation.Id, SequenceType), (Atom.Id, Option[String], Step.Id, ProtoStep[D])] =

      val proto_step = (
        instrumentDecoder *:
        step_config       *:
        telescope_config  *:
        obs_class         *:
        breakpoint
      ).to[ProtoStep[D]]

      sql"""
        SELECT
          a.c_atom_id,
          a.c_description,
          s.c_step_id,
          #${encodeColumns("i".some, instrumentColumns)},
          s.c_step_type,
          #${encodeColumns("g".some, StepConfigGcalColumns)},
          #${encodeColumns("r".some, StepConfigSmartGcalColumns)},
          s.c_offset_p,
          s.c_offset_q,
          s.c_guide_state,
          s.c_observe_class,
          s.c_breakpoint

        FROM t_atom a

        JOIN t_step s
          ON s.c_atom_id = a.c_atom_id

        LEFT JOIN t_step_execution se ON se.c_step_id = s.c_step_id

        JOIN #${instrumentTable} i
          ON i.c_step_id = s.c_step_id

        LEFT JOIN t_step_config_gcal g
          ON g.c_step_id = s.c_step_id

        LEFT JOIN t_step_config_smart_gcal r
          ON r.c_step_id = s.c_step_id

        WHERE
          a.c_instrument     = $instrument      AND
          a.c_observation_id = $observation_id  AND
          a.c_sequence_type  = $sequence_type   AND
          (se.c_step_id IS NULL OR se.c_execution_state IN ( 'not_started', 'ongoing' ))
        ORDER BY
          a.c_atom_index,
          s.c_step_index
      """.query(atom_id *: text.opt *: step_id *: proto_step)

    val SelectFlamingos2Sequence: Query[(Instrument, Observation.Id, SequenceType), (Atom.Id, Option[String], Step.Id, ProtoStep[Flamingos2DynamicConfig])] =
      selectSequence(
        "t_flamingos_2_dynamic",
        Flamingos2SequenceService.Statements.Flamingos2DynamicColumns,
        flamingos_2_dynamic
      )

    val SelectGmosNorthSequence: Query[(Instrument, Observation.Id, SequenceType), (Atom.Id, Option[String], Step.Id, ProtoStep[GmosNorth])] =
      selectSequence(
        "t_gmos_north_dynamic",
        GmosSequenceService.Statements.GmosDynamicColumns,
        gmos_north_dynamic
      )

    val SelectGmosSouthSequence: Query[(Instrument, Observation.Id, SequenceType), (Atom.Id, Option[String], Step.Id, ProtoStep[GmosSouth])] =
      selectSequence(
        "t_gmos_south_dynamic",
        GmosSequenceService.Statements.GmosDynamicColumns,
        gmos_south_dynamic
      )

    val SelectIgrins2Sequence: Query[(Instrument, Observation.Id, SequenceType), (Atom.Id, Option[String], Step.Id, ProtoStep[Igrins2DynamicConfig])] =
      selectSequence(
        "t_igrins_2_dynamic",
        Igrins2SequenceService.Statements.Igrins2DynamicColumns,
        igrins_2_dynamic
      )

    val IsMaterialized: Query[(Observation.Id, SequenceType), Boolean] =
      sql"""
        SELECT EXISTS (
          SELECT 1
          FROM   t_sequence_materialization
          WHERE  c_observation_id = $observation_id
            AND  c_sequence_type  = $sequence_type
        )
      """.query(bool)

    val MarkMaterializedOrDoNothing: Query[(Observation.Id, SequenceType), Boolean] =
      sql"""
        WITH ins AS (
          INSERT INTO t_sequence_materialization (
            c_observation_id,
            c_sequence_type,
            c_created,
            c_updated
          )
          VALUES ($observation_id, $sequence_type, now(), now())
          ON CONFLICT DO NOTHING
          RETURNING 1
        )
        SELECT EXISTS (SELECT 1 FROM ins) AS inserted
      """.query(bool)

    val MarkMaterializedOrUpdate: Query[(Observation.Id, SequenceType), Boolean] =
      sql"""
        INSERT INTO t_sequence_materialization (
          c_observation_id,
          c_sequence_type,
          c_created,
          c_updated
        )
        VALUES ($observation_id, $sequence_type, now(), now())
        ON CONFLICT (c_observation_id, c_sequence_type)
        DO UPDATE SET c_updated = now()
        RETURNING (xmax = 0) AS inserted
      """.query(bool)
