// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pipe
import fs2.Pure
import fs2.Stream
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.CloneSequenceMode
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SmartGcalType
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.StepType
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.AtomDigest
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth as GmosNorthStatic
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth as GmosSouthStatic
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.core.util.Uid
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.mapping.AccessControl.CheckedWithId
import lucuma.odb.logic.Generator.SequenceAtomLimit
import lucuma.odb.logic.SmartGcalImplementation
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.SmartGcalExpander
import lucuma.odb.sequence.StepTimeEstimateCalculator
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StreamingExecutionConfig
import lucuma.odb.sequence.data.UnsplittableAtom
import lucuma.odb.sequence.gcalClass
import lucuma.odb.sequence.util.AtomBuilder
import lucuma.odb.sequence.util.StepInsertion
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.*
import lucuma.odb.util.GhostCodecs.*
import lucuma.odb.util.GmosCodecs.*
import lucuma.odb.util.GnirsCodecs.*
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
    customized:     Boolean      = true,
    namespace:      Option[UUID] = None
  )(using Transaction[F]): F[Result[Stream[Pure, Atom[Flamingos2DynamicConfig]]]]

  def replaceFlamingos2Sequence(
    checked: CheckedWithId[(SequenceType, List[ProtoAtom[ProtoStep[Flamingos2DynamicConfig]]]), Observation.Id]
  )(using Transaction[F]): F[Result[Stream[Pure, Atom[Flamingos2DynamicConfig]]]]

  def selectOrComputeGhostStatic(
    observationId: Observation.Id
  )(using Transaction[F]): F[Either[OdbError, GhostStaticConfig]]

  def replaceGhostSequence(
    observationId:  Observation.Id,
    sequenceType:   SequenceType,
    sequence:       List[ProtoAtom[ProtoStep[GhostDynamicConfig]]],
    customized:     Boolean      = true,
    namespace:      Option[UUID] = None
  )(using Transaction[F]): F[Result[Stream[Pure, Atom[GhostDynamicConfig]]]]

  def replaceGhostSequence(
    checked: CheckedWithId[(SequenceType, List[ProtoAtom[ProtoStep[GhostDynamicConfig]]]), Observation.Id]
  )(using Transaction[F]): F[Result[Stream[Pure, Atom[GhostDynamicConfig]]]]

  def replaceGmosNorthSequence(
    observationId:  Observation.Id,
    sequenceType:   SequenceType,
    sequence:       List[ProtoAtom[ProtoStep[GmosNorth]]],
    customized:     Boolean      = true,
    namespace:      Option[UUID] = None
  )(using Transaction[F]): F[Result[Stream[Pure, Atom[GmosNorth]]]]

  def replaceGmosNorthSequence(
    checked: CheckedWithId[(SequenceType, List[ProtoAtom[ProtoStep[GmosNorth]]]), Observation.Id]
  )(using Transaction[F]): F[Result[Stream[Pure, Atom[GmosNorth]]]]

  def replaceGmosSouthSequence(
    observationId:  Observation.Id,
    sequenceType:   SequenceType,
    sequence:       List[ProtoAtom[ProtoStep[GmosSouth]]],
    customized:     Boolean      = true,
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

  def resetGnirsAcquisition(
    observationId: Observation.Id,
    sequence:      Stream[F, Atom[GnirsDynamicConfig]]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def insertFlamingos2Sequence(
    observationId: Observation.Id,
    sequenceType:  SequenceType,
    sequence:      Stream[F, Atom[Flamingos2DynamicConfig]]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def insertGhostSequence(
    observationId:  Observation.Id,
    sequenceType:   SequenceType,
    sequence:       Stream[F, Atom[GhostDynamicConfig]]
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
    stream:        StreamingExecutionConfig[F, Flamingos2StaticConfig, Flamingos2DynamicConfig],
    sequenceTypes: Set[SequenceType] = Enumerated[SequenceType].all.toSet
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def materializeGhostExecutionConfig(
    observationId: Observation.Id,
    stream:        StreamingExecutionConfig[F, GhostStaticConfig, GhostDynamicConfig],
    sequenceTypes: Set[SequenceType] = Enumerated[SequenceType].all.toSet
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def materializeGmosNorthExecutionConfig(
    observationId: Observation.Id,
    stream:        StreamingExecutionConfig[F, GmosNorthStatic, GmosNorth],
    sequenceTypes: Set[SequenceType] = Enumerated[SequenceType].all.toSet
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def materializeGmosSouthExecutionConfig(
    observationId: Observation.Id,
    stream:        StreamingExecutionConfig[F, GmosSouthStatic, GmosSouth],
    sequenceTypes: Set[SequenceType] = Enumerated[SequenceType].all.toSet
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def selectFlamingos2Sequence(
    observationId: Observation.Id,
    sequenceType:  SequenceType,
    staticConfig:  Flamingos2StaticConfig
  )(using Transaction[F]): F[Option[Stream[F, Atom[Flamingos2DynamicConfig]]]]

  def selectGhostSequence(
    observationId: Observation.Id,
    sequenceType:  SequenceType,
    staticConfig:  GhostStaticConfig
  )(using Transaction[F]): F[Option[Stream[F, Atom[GhostDynamicConfig]]]]

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

  def replaceIgrins2Sequence(
    observationId:  Observation.Id,
    sequenceType:   SequenceType,
    sequence:       List[ProtoAtom[ProtoStep[Igrins2DynamicConfig]]],
    customized:     Boolean      = true,
    namespace:      Option[UUID] = None
  )(using Transaction[F]): F[Result[Stream[Pure, Atom[Igrins2DynamicConfig]]]]

  def replaceIgrins2Sequence(
    checked: CheckedWithId[(SequenceType, List[ProtoAtom[ProtoStep[Igrins2DynamicConfig]]]), Observation.Id]
  )(using Transaction[F]): F[Result[Stream[Pure, Atom[Igrins2DynamicConfig]]]]

  def insertIgrins2Sequence(
    observationId: Observation.Id,
    sequenceType:  SequenceType,
    sequence:      Stream[F, Atom[Igrins2DynamicConfig]]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def materializeIgrins2ExecutionConfig(
    observationId: Observation.Id,
    stream:        StreamingExecutionConfig[F, Igrins2StaticConfig, Igrins2DynamicConfig],
    sequenceTypes: Set[SequenceType] = Enumerated[SequenceType].all.toSet
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def selectIgrins2Sequence(
    observationId: Observation.Id,
    sequenceType:  SequenceType,
    staticConfig:  Igrins2StaticConfig
  )(using Transaction[F]): F[Option[Stream[F, Atom[Igrins2DynamicConfig]]]]

  def replaceGnirsSequence(
    observationId:  Observation.Id,
    sequenceType:   SequenceType,
    sequence:       List[ProtoAtom[ProtoStep[GnirsDynamicConfig]]],
    customized:     Boolean      = true,
    namespace:      Option[UUID] = None
  )(using Transaction[F]): F[Result[Stream[Pure, Atom[GnirsDynamicConfig]]]]

  def replaceGnirsSequence(
    checked: CheckedWithId[(SequenceType, List[ProtoAtom[ProtoStep[GnirsDynamicConfig]]]), Observation.Id]
  )(using Transaction[F]): F[Result[Stream[Pure, Atom[GnirsDynamicConfig]]]]

  def insertGnirsSequence(
    observationId: Observation.Id,
    sequenceType:  SequenceType,
    sequence:      Stream[F, Atom[GnirsDynamicConfig]]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def materializeGnirsExecutionConfig(
    observationId: Observation.Id,
    stream:        StreamingExecutionConfig[F, GnirsStaticConfig, GnirsDynamicConfig],
    sequenceTypes: Set[SequenceType] = Enumerated[SequenceType].all.toSet
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def selectGnirsSequence(
    observationId: Observation.Id,
    sequenceType:  SequenceType,
    staticConfig:  GnirsStaticConfig
  )(using Transaction[F]): F[Option[Stream[F, Atom[GnirsDynamicConfig]]]]

  def deleteSequence(
    observationId: Observation.Id
  )(using Transaction[F]): F[Result[Unit]]

  /**
   * Copies the materialized `sequenceTypes` sequences of `source` into `clone`,
   * as pending steps, according to `mode`.  A sequence type that is not
   * materialized, or that has nothing to copy, is left to be generated.
   */
  def cloneSequence(
    source:        Observation.Id,
    clone:         Observation.Id,
    mode:          CloneSequenceMode,
    sequenceTypes: List[SequenceType]
  )(using Transaction[F]): F[Result[Unit]]

  /**
   * Inserts the GCAL steps that the given smart GCAL types expand to into a
   * materialized sequence, immediately after `afterStepId` or, if not
   * specified, after the most recently executed step.  If nothing has been
   * executed, they go before the first step instead.  The smart GCAL lookup is
   * based on the instrument configuration of the step they follow (or precede
   * when they follow nothing).  See `StepInsertion` for the atom in which they
   * are placed.  The sequence is marked as customized.
   *
   * @return ids of the inserted steps, in sequence order
   */
  def insertSmartGcal(
    observationId:   Observation.Id,
    sequenceType:    SequenceType,
    smartGcalTypes:  NonEmptyList[SmartGcalType],
    afterStepId:     Option[Step.Id],
    calibrationRole: Option[CalibrationRole]
  )(using Transaction[F]): F[Result[NonEmptyList[Step.Id]]]

object SequenceService:

  private val BatchSize = 256

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
          Flamingos2SequenceService.Statements.insertDynamics
        )

      override def insertGhostSequence(
        observationId:  Observation.Id,
        sequenceType:   SequenceType,
        sequence:       Stream[F, Atom[GhostDynamicConfig]]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        insertSequence(
          Instrument.Ghost,
          observationId,
          sequenceType,
          sequence,
          GhostSequenceService.Statements.insertDynamics
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
          GmosSequenceService.Statements.insertNorthDynamics
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
          GmosSequenceService.Statements.insertSouthDynamics
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
          Igrins2SequenceService.Statements.insertDynamics
        )

      override def insertGnirsSequence(
        observationId:  Observation.Id,
        sequenceType:   SequenceType,
        sequence:       Stream[F, Atom[GnirsDynamicConfig]]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        insertSequence(
          Instrument.Gnirs,
          observationId,
          sequenceType,
          sequence,
          GnirsSequenceService.Statements.insertDynamics
        )

      private def insertSequence[D](
        instrument:       Instrument,
        observationId:    Observation.Id,
        sequenceType:     SequenceType,
        sequence:         Stream[F, Atom[D]],
        insertInstConfig: (rows: List[(Step.Id, D)]) => Command[rows.type]
      ): F[Unit] =

        val atom: Pipe[F, Atom[D], Nothing] = atomStream =>
          atomStream
            .zipWithIndex
            .map { case (atom, idx) =>
              (atom.id, observationId, sequenceType, instrument, idx.toInt, atom.description.map(_.value))
            }
            .chunkN(BatchSize)
            .evalMap { c =>
              val rows = c.toList
              session.execute(Statements.insertAtoms(rows))(rows)
            }
            .drain

        val step: Pipe[F, Atom[D], Nothing] = atomStream =>
          atomStream
            .flatMap: atom =>
              Stream.emits(
                atom.steps.toList.zipWithIndex.tupleLeft(atom.id).map { case (aid, (step, idx)) =>
                  (step.id, aid, instrument, step.stepConfig.stepType, idx, step.observeClass,
                   step.estimate.total, step.telescopeConfig, step.breakpoint)
                }
              )
            .chunkN(BatchSize)
            .evalMap { c =>
              val rows = c.toList
              session.execute(Statements.insertSteps(rows))(rows)
            }
            .drain

        val gcal: Pipe[F, Atom[D], Nothing] = atomStream =>
          atomStream
            .flatMap: atom =>
              Stream.emits(
                atom.steps.toList.flatMap: step =>
                  StepConfig.gcal.getOption(step.stepConfig).tupleLeft(step.id)
              )
            .chunkN(BatchSize)
            .evalMap { c =>
              val rows = c.toList
              session.execute(Statements.insertGcalConfigs(rows))(rows)
            }
            .drain

        val instrumentConfig: Pipe[F, Atom[D], Nothing] = atomStream =>
          atomStream
            .flatMap: atom =>
              Stream.emits(
                atom.steps.toList.map: step =>
                  (step.id, step.instrumentConfig)
              )
            .chunkN(BatchSize)
            .evalMap { c =>
              val rows = c.toList
              session.execute(insertInstConfig(rows))(rows)
            }
            .drain

        sequence
          .broadcastThrough(atom, step, gcal, instrumentConfig)
          .compile
          .drain

      extension [D](p: ProtoStep[D])
        def toStep(sid: Step.Id, estimate: StepEstimate): Step[D] =
          Step(sid, p.value, p.stepConfig, p.telescopeConfig, estimate, p.observeClass, p.breakpoint)

      // Groups adjacent rows that share an atom id.
      private def groupByAtom[A, B](
        f: (Atom.Id, Option[NonEmptyString], NonEmptyList[A]) => B
      ): Pipe[F, (Atom.Id, Option[String], A), B] =
        _.groupAdjacentBy(_._1)
         .map: (aid, chunk) =>
           f(
             aid,
             chunk.head.flatMap(_._2).flatMap(NonEmptyString.from(_).toOption),
             NonEmptyList.fromListUnsafe(chunk.map(_._3).toList)
           )

      // Turns a stream of ProtoStep into an atom by running the time estimation
      // and grouping the steps by atom id.
      private def atomPipe[S, D](
        static:    S,
        estimator: StepTimeEstimateCalculator[S, D]
      ): Pipe[F, (Atom.Id, Option[String], Step.Id, ProtoStep[D]), Atom[D]] =
        _.mapAccumulate(StepTimeEstimateCalculator.Last.empty[D]) {
          case (last, (aid, desc, sid, protoStep)) =>
            val (lastʹ, estimate) = estimator.estimateOne(static, protoStep).run(last).value
            (lastʹ, (aid, desc, protoStep.toStep(sid, estimate)))
        }
        .map(_._2)
        .through(groupByAtom(Atom(_, _, _)))

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
       * last materialization.  Either way, records whether the sequence is now
       * customized (i.e., explicitly replaced rather than generated).
       *
       * @return `true` if a new row was inserted, `false` if an existing row
       *         was updated
       */
      private def markMaterializedOrUpdate(
        observationId: Observation.Id,
        sequenceType:  SequenceType,
        customized:    Boolean
      ): F[Boolean] =
        session.unique(Statements.MarkMaterializedOrUpdate)(observationId, sequenceType, customized)

      /**
       * Deletes the row in the t_squence_materialized table if it exists.
       *
       * @return `true` if a row was deleted, `false` otherwise.
       */
      private def deleteMaterialized(
        observationId: Observation.Id,
        sequenceType:  SequenceType
      ): F[Boolean] =
        session.unique(Statements.DeleteMaterialized)(observationId, sequenceType)

      private def atomBuilder[S, D](
        sequenceType: SequenceType,
        static:       S,
        namespace:    Option[UUID],
        estimator:    StepTimeEstimateCalculator[S, D]
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
        customized:       Boolean,
        insertInstConfig: (rows: List[(Step.Id, D)]) => Command[rows.type],
        atomBuilder:      AtomBuilder[D]
      )(using Transaction[F]): ResultT[F, Stream[Pure, Atom[D]]] =

        val checkAtomLength: ResultT[F, Unit] =
          if sequence.lengthIs <= SequenceAtomLimit then ResultT.unit
          else ResultT:
            OdbError
              .InvalidArgument(s"Execution sequences containing over $SequenceAtomLimit atoms are not supported.".some)
              .asFailureF

        // Only the science sequence is collapsed into a single atom for
        // unsplittable observations (see StreamingExecutionConfig.unsplit);
        // acquisition sequences are multi-atom by design.
        val checkUnsplittable: ResultT[F, Unit] =
          if sequenceType =!= SequenceType.Science then ResultT.unit
          else ResultT.liftF(observationService.selectIsSplittable(observationId)).flatMap:
            case Some(false) => // means that the observation is not dividable into multiple atoms
              val stepLimit = UnsplittableAtom.StepLimit.value
              val message   = sequence match
                case a :: Nil => Option.when(a.steps.size > stepLimit)(s"An unsplittable observation's atom may not contain more than $stepLimit steps.")
                case Nil      => none
                case _        => s"Unsplittable observations may only contain a single atom.".some
              message.fold(ResultT.unit)(m => ResultT(OdbError.InvalidArgument(m.some).asFailureF))
            case _           =>
              ResultT.unit

        val doReplace: F[Stream[Pure, Atom[D]]] =
          val atoms = atomBuilder.buildStream(Stream.emits(sequence))

          for
            _ <- markMaterializedOrUpdate(observationId, sequenceType, customized)
            _ <- abandonAndDeleteUnexecuted(observationId, sequenceType)
            _ <- insertSequence(instrument, observationId, sequenceType, atoms.covary[F], insertInstConfig)
          yield atoms

        checkAtomLength *> checkUnsplittable *> ResultT.liftF(doReplace)

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
        customized:    Boolean      = true,
        namespace:     Option[UUID] = None
      )(using Transaction[F]): F[Result[Stream[Pure, Atom[Flamingos2DynamicConfig]]]] =

        (for
          s <- selectStatic(observationId, "Flamingos 2", flamingos2SequenceService.selectStaticOrDefault)
          b <- ResultT.liftF(atomBuilder(sequenceType, s, namespace, estimator.flamingos2Step))
          r <- replaceSequence(
                 Instrument.Flamingos2,
                 observationId,
                 sequenceType,
                 sequence,
                 customized,
                 Flamingos2SequenceService.Statements.insertDynamics,
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

      override def selectOrComputeGhostStatic(
        observationId: Observation.Id
      )(using Transaction[F]): F[Either[OdbError, GhostStaticConfig]] =
        for
          s0 <- ghostSequenceService.selectStatic(observationId)
          s  <- s0.fold(ghostIfuService.computeStatic(observationId))(_.asRight[OdbError].pure[F])
        yield s

      override def replaceGhostSequence(
        observationId:  Observation.Id,
        sequenceType:   SequenceType,
        sequence:       List[ProtoAtom[ProtoStep[GhostDynamicConfig]]],
        customized:     Boolean      = true,
        namespace:      Option[UUID] = None
      )(using Transaction[F]): F[Result[Stream[Pure, Atom[GhostDynamicConfig]]]] =
        (for
          s <- ResultT(selectOrComputeGhostStatic(observationId).map(e => Result.fromEither(e.leftMap(_.asProblem))))
          b <- ResultT.liftF(atomBuilder(sequenceType, s, namespace, estimator.ghostStep))
          r <- replaceSequence(
                 Instrument.Ghost,
                 observationId,
                 sequenceType,
                 sequence,
                 customized,
                 GhostSequenceService.Statements.insertDynamics,
                 b
               )
        yield r).value

      override def replaceGhostSequence(
        checked: CheckedWithId[(SequenceType, List[ProtoAtom[ProtoStep[GhostDynamicConfig]]]), Observation.Id]
      )(using Transaction[F]): F[Result[Stream[Pure, Atom[GhostDynamicConfig]]]] =
        checked.foldWithId(OdbError.InvalidArgument().asFailureF[F, Stream[Pure, Atom[GhostDynamicConfig]]]) { case ((sequenceType, sequence), oid) =>
          replaceGhostSequence(
            oid,
            sequenceType,
            sequence
          )
        }

      override def replaceGmosNorthSequence(
        observationId: Observation.Id,
        sequenceType:  SequenceType,
        sequence:      List[ProtoAtom[ProtoStep[GmosNorth]]],
        customized:    Boolean      = true,
        namespace:     Option[UUID] = None
      )(using Transaction[F]): F[Result[Stream[Pure, Atom[GmosNorth]]]] =
        (for
          s <- selectStatic(observationId, "GMOS North", gmosSequenceService.selectGmosNorthStaticOrDefault)
          b <- ResultT.liftF(atomBuilder(sequenceType, s, namespace, estimator.gmosNorthStep))
          r <- replaceSequence(
                 Instrument.GmosNorth,
                 observationId,
                 sequenceType,
                 sequence,
                 customized,
                 GmosSequenceService.Statements.insertNorthDynamics,
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
        customized:    Boolean      = true,
        namespace:     Option[UUID] = None
      )(using Transaction[F]): F[Result[Stream[Pure, Atom[GmosSouth]]]] =
        (for
          s <- selectStatic(observationId, "GMOS South", gmosSequenceService.selectGmosSouthStaticOrDefault)
          b <- ResultT.liftF(atomBuilder(sequenceType, s, namespace, estimator.gmosSouthStep))
          r <- replaceSequence(
                 Instrument.GmosSouth,
                 observationId,
                 sequenceType,
                 sequence,
                 customized,
                 GmosSequenceService.Statements.insertSouthDynamics,
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

      override def replaceIgrins2Sequence(
        observationId: Observation.Id,
        sequenceType:  SequenceType,
        sequence:      List[ProtoAtom[ProtoStep[Igrins2DynamicConfig]]],
        customized:    Boolean      = true,
        namespace:     Option[UUID] = None
      )(using Transaction[F]): F[Result[Stream[Pure, Atom[Igrins2DynamicConfig]]]] =
        (for
          s <- selectStatic(observationId, "IGRINS-2", igrins2SequenceService.selectStaticOrDefault)
          b <- ResultT.liftF(atomBuilder(sequenceType, s, namespace, estimator.igrins2Step))
          r <- replaceSequence(
                 Instrument.Igrins2,
                 observationId,
                 sequenceType,
                 sequence,
                 customized,
                 Igrins2SequenceService.Statements.insertDynamics,
                 b
               )
        yield r).value

      override def replaceIgrins2Sequence(
        checked: CheckedWithId[(SequenceType, List[ProtoAtom[ProtoStep[Igrins2DynamicConfig]]]), Observation.Id]
      )(using Transaction[F]): F[Result[Stream[Pure, Atom[Igrins2DynamicConfig]]]] =
        checked.foldWithId(OdbError.InvalidArgument().asFailureF[F, Stream[Pure, Atom[Igrins2DynamicConfig]]]) { case ((sequenceType, sequence), oid) =>
          replaceIgrins2Sequence(
            oid,
            sequenceType,
            sequence
          )
        }

      override def replaceGnirsSequence(
        observationId: Observation.Id,
        sequenceType:  SequenceType,
        sequence:      List[ProtoAtom[ProtoStep[GnirsDynamicConfig]]],
        customized:    Boolean      = true,
        namespace:     Option[UUID] = None
      )(using Transaction[F]): F[Result[Stream[Pure, Atom[GnirsDynamicConfig]]]] =
        (for
          s <- selectStatic(observationId, "GNIRS", gnirsSequenceService.selectStaticOrDefault)
          b <- ResultT.liftF(atomBuilder(sequenceType, s, namespace, estimator.gnirsStep))
          r <- replaceSequence(
                 Instrument.Gnirs,
                 observationId,
                 sequenceType,
                 sequence,
                 customized,
                 GnirsSequenceService.Statements.insertDynamics,
                 b
               )
        yield r).value

      override def replaceGnirsSequence(
        checked: CheckedWithId[(SequenceType, List[ProtoAtom[ProtoStep[GnirsDynamicConfig]]]), Observation.Id]
      )(using Transaction[F]): F[Result[Stream[Pure, Atom[GnirsDynamicConfig]]]] =
        checked.foldWithId(OdbError.InvalidArgument().asFailureF[F, Stream[Pure, Atom[GnirsDynamicConfig]]]) { case ((sequenceType, sequence), oid) =>
          replaceGnirsSequence(
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
          _ <- markMaterializedOrUpdate(observationId, SequenceType.Acquisition, customized = false)
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

      override def resetGnirsAcquisition(
        observationId: Observation.Id,
        stream:        Stream[F, Atom[GnirsDynamicConfig]]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        resetAcquisition(observationId, stream)(insertGnirsSequence)

      private def materializeExecutionConfig[S, D](
        observationId: Observation.Id,
        stream:        StreamingExecutionConfig[F, S, D],
        sequenceTypes: Set[SequenceType],
        insertStatic:  (Observation.Id, S) => F[Option[Long]]
      )(
        insertSequence: (Observation.Id, SequenceType, Stream[F, Atom[D]]) => F[Unit]
      )(using Services.ServiceAccess): F[Unit] =

        def materializeSequence(sequenceType: SequenceType, s: Stream[F, Atom[D]]): F[Unit] =
          markMaterializedOrDoNothing(observationId, sequenceType).ifM(
            insertSequence(observationId, sequenceType, s),
            Applicative[F].unit
          ).whenA(sequenceTypes.contains(sequenceType))

        insertStatic(observationId, stream.static)                  *>
        materializeSequence(SequenceType.Acquisition, stream.acquisition) *>
        materializeSequence(SequenceType.Science,     stream.science)

      override def materializeFlamingos2ExecutionConfig(
        observationId: Observation.Id,
        stream:        StreamingExecutionConfig[F, Flamingos2StaticConfig, Flamingos2DynamicConfig],
        sequenceTypes: Set[SequenceType]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        materializeExecutionConfig(observationId, stream, sequenceTypes, flamingos2SequenceService.insertStatic)(insertFlamingos2Sequence)

      override def materializeGhostExecutionConfig(
        observationId: Observation.Id,
        stream:        StreamingExecutionConfig[F, GhostStaticConfig, GhostDynamicConfig],
        sequenceTypes: Set[SequenceType]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        materializeExecutionConfig(observationId, stream, sequenceTypes, ghostSequenceService.insertStatic)(insertGhostSequence)

      override def materializeGmosNorthExecutionConfig(
        observationId: Observation.Id,
        stream:        StreamingExecutionConfig[F, GmosNorthStatic, GmosNorth],
        sequenceTypes: Set[SequenceType]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        materializeExecutionConfig(observationId, stream, sequenceTypes, gmosSequenceService.insertGmosNorthStatic)(insertGmosNorthSequence)

      override def materializeGmosSouthExecutionConfig(
        observationId: Observation.Id,
        stream:        StreamingExecutionConfig[F, GmosSouthStatic, GmosSouth],
        sequenceTypes: Set[SequenceType]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        materializeExecutionConfig(observationId, stream, sequenceTypes, gmosSequenceService.insertGmosSouthStatic)(insertGmosSouthSequence)

      override def materializeIgrins2ExecutionConfig(
        observationId: Observation.Id,
        stream:        StreamingExecutionConfig[F, Igrins2StaticConfig, Igrins2DynamicConfig],
        sequenceTypes: Set[SequenceType]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        materializeExecutionConfig(observationId, stream, sequenceTypes, igrins2SequenceService.insertStatic)(insertIgrins2Sequence)

      override def materializeGnirsExecutionConfig(
        observationId: Observation.Id,
        stream:        StreamingExecutionConfig[F, GnirsStaticConfig, GnirsDynamicConfig],
        sequenceTypes: Set[SequenceType]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        materializeExecutionConfig(observationId, stream, sequenceTypes, gnirsSequenceService.insertStatic)(insertGnirsSequence)

      private def selectSequence[S, D](
        instrument:    Instrument,
        observationId: Observation.Id,
        sequenceType:  SequenceType,
        query:         Query[(Instrument, Observation.Id, SequenceType), (Atom.Id, Option[String], Step.Id, ProtoStep[D])],
        staticConfig:  S,
        estimator:     StepTimeEstimateCalculator[S, D]
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
          estimator.flamingos2Step
        )

      override def selectGhostSequence(
        observationId: Observation.Id,
        sequenceType:  SequenceType,
        staticConfig:  GhostStaticConfig
      )(using Transaction[F]): F[Option[Stream[F, Atom[GhostDynamicConfig]]]] =
        selectSequence(
          Instrument.Ghost,
          observationId,
          sequenceType,
          Statements.SelectGhostSequence,
          staticConfig,
          estimator.ghostStep
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
          estimator.gmosNorthStep
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
          estimator.gmosSouthStep
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
          estimator.igrins2Step
        )

      override def selectGnirsSequence(
        observationId: Observation.Id,
        sequenceType:  SequenceType,
        staticConfig:  GnirsStaticConfig
      )(using Transaction[F]): F[Option[Stream[F, Atom[GnirsDynamicConfig]]]] =
        selectSequence(
          Instrument.Gnirs,
          observationId,
          sequenceType,
          Statements.SelectGnirsSequence,
          staticConfig,
          estimator.gnirsStep
        )

      private def deleteMaterializedSequence(observationId: Observation.Id, sequenceType: SequenceType): F[Unit] =
        deleteMaterialized(observationId, sequenceType).ifM(
          abandonAndDeleteUnexecuted(observationId, sequenceType),
          Applicative[F].unit
        )

      override def deleteSequence(
        observationId: Observation.Id
      )(using Transaction[F]): F[Result[Unit]] =
        def doDelete: F[Unit] = for {
          _ <- deleteMaterializedSequence(observationId, SequenceType.Acquisition)
          _ <- deleteMaterializedSequence(observationId, SequenceType.Science)
        } yield ()
        // AccessControl limits this to pre-execution, but we could still have visits. If there
        // are visits, it brings up questions about the static configs and whether we should delete those too.
        // For now, we'll just disallow deleting sequences with visits.
        visitService.hasVisits(observationId).ifM(
          OdbError.InvalidArgument(s"Cannot delete sequence for observation $observationId because it has visits.".some).asFailureF,
          doDelete.map(Result.success)
        )

      override def cloneSequence(
        source:        Observation.Id,
        clone:         Observation.Id,
        mode:          CloneSequenceMode,
        sequenceTypes: List[SequenceType]
      )(using Transaction[F]): F[Result[Unit]] =

        def copy[D](
          instrument:   Instrument,
          sequenceType: SequenceType,
          table:        Statements.DynamicTable[D],
          pendingOnly:  Boolean,
          replace:      List[ProtoAtom[ProtoStep[D]]] => F[Result[Stream[Pure, Atom[D]]]]
        ): F[Result[Unit]] =
          session
            .stream(Statements.selectSequenceForClone(table, pendingOnly))((instrument, source, sequenceType), BatchSize)
            .map((aid, desc, _, step) => (aid, desc, step))
            .through(groupByAtom((_, desc, steps) => ProtoAtom(desc, steps)))
            .compile
            .toList
            .flatMap:
              case Nil   => Result.unit.pure[F]
              case atoms => replace(atoms).map(_.void)

        // The clone inherits the source's customization: a copy of hand-edited
        // steps is customized, a copy of generated steps is not.
        def copySequenceType(instrument: Instrument, sequenceType: SequenceType, pendingOnly: Boolean, customized: Boolean): F[Result[Unit]] =
          instrument match
            case Instrument.Flamingos2 => copy(instrument, sequenceType, Statements.Flamingos2Table, pendingOnly, replaceFlamingos2Sequence(clone, sequenceType, _, customized))
            case Instrument.Ghost      => copy(instrument, sequenceType, Statements.GhostTable,      pendingOnly, replaceGhostSequence(clone, sequenceType, _, customized))
            case Instrument.GmosNorth  => copy(instrument, sequenceType, Statements.GmosNorthTable,  pendingOnly, replaceGmosNorthSequence(clone, sequenceType, _, customized))
            case Instrument.GmosSouth  => copy(instrument, sequenceType, Statements.GmosSouthTable,  pendingOnly, replaceGmosSouthSequence(clone, sequenceType, _, customized))
            case Instrument.Igrins2    => copy(instrument, sequenceType, Statements.Igrins2Table,    pendingOnly, replaceIgrins2Sequence(clone, sequenceType, _, customized))
            case Instrument.Gnirs      => copy(instrument, sequenceType, Statements.GnirsTable,      pendingOnly, replaceGnirsSequence(clone, sequenceType, _, customized))
            case _                     => Result.unit.pure[F]

        def copyIfMaterialized(instrument: Instrument, sequenceType: SequenceType, pendingOnly: Boolean): ResultT[F, Unit] =
          ResultT:
            session.option(Statements.SelectIsCustomized)(source, sequenceType).flatMap:
              case None             => Result.unit.pure[F]  // not materialized
              case Some(customized) => copySequenceType(instrument, sequenceType, pendingOnly, customized)

        def copyAll(pendingOnly: Boolean): F[Result[Unit]] =
          observationService.selectInstrument(source).flatMap:
            case None             => Result.unit.pure[F]
            case Some(instrument) =>
              sequenceTypes.traverse_(copyIfMaterialized(instrument, _, pendingOnly)).value

        mode match
          case CloneSequenceMode.None         => Result.unit.pure[F]
          case CloneSequenceMode.AllSteps     => copyAll(pendingOnly = false)
          case CloneSequenceMode.PendingSteps => copyAll(pendingOnly = true)

      private def insertSmartGcalSteps[S, D](
        instrument:       Instrument,
        observationId:    Observation.Id,
        sequenceType:     SequenceType,
        smartGcalTypes:   NonEmptyList[SmartGcalType],
        afterStepId:      Option[Step.Id],
        gcalClass:        ObserveClass,
        static:           S,
        table:            Statements.DynamicTable[D],
        estimator:        StepTimeEstimateCalculator[S, D],
        expander:         SmartGcalExpander[F, S, D],
        insertInstConfig: (rows: List[(Step.Id, D)]) => Command[rows.type]
      )(using Transaction[F]): ResultT[F, NonEmptyList[Step.Id]] =

        def invalid[A](msg: String): Result[A] =
          OdbError.InvalidArgument(msg.some).asFailure

        // GCAL steps are taken with guiding disabled.  Keeping the reference
        // step's offset avoids an unnecessary offset.
        def expand(reference: ProtoStep[D], sgt: SmartGcalType): ResultT[F, NonEmptyList[ProtoStep[D]]] =
          val smart = ProtoStep(
            reference.value,
            StepConfig.SmartGcal(sgt),
            TelescopeConfig(reference.telescopeConfig.offset, StepGuideState.Disabled),
            gcalClass
          )
          ResultT:
            expander
              .expandStep(static, smart)
              .map(_.fold(m => invalid(s"Cannot insert a smart GCAL ${sgt.tag} step: $m"), Result.success))

        // The unexecuted part of an unsplittable observation's science
        // sequence is a single atom, which may not grow beyond the step limit.
        def checkUnsplittable(rows: List[StepInsertion.Row[D]], target: StepInsertion.Target, count: Int): ResultT[F, Unit] =
          val unstarted        = rows.filterNot(_.isStarted)
          val (atoms, pending) = target match
            case StepInsertion.Target.Existing(aid, _, _) =>
              ((unstarted.map(_.atomId).toSet + aid).size, unstarted.count(_.atomId === aid))
            case StepInsertion.Target.NewAtom(_)          =>
              (unstarted.map(_.atomId).distinct.size + 1, 0)
          val limit            = UnsplittableAtom.StepLimit.value
          val problem          =
            if atoms > 1 then "Unsplittable observations may only contain a single atom.".some
            else Option.when(pending + count > limit)(s"An unsplittable observation's atom may not contain more than $limit steps.")

          problem.filter(_ => sequenceType === SequenceType.Science).fold(ResultT.unit): msg =>
            ResultT(observationService.selectIsSplittable(observationId).map:
              case Some(false) => invalid(msg)
              case _           => Result.unit
            )

        // Makes room for the new steps, returning the atom and index of the
        // first new step.
        def writeAtom(plan: StepInsertion.Plan[D], count: Int): F[(Atom.Id, Int)] =
          val shift =
            if plan.shiftAtoms.isEmpty then Applicative[F].unit
            else session.execute(Statements.shiftAtoms(plan.shiftAtoms))(plan.shiftAtoms).void

          shift *> (plan.target match
            case StepInsertion.Target.Existing(aid, idx, atomIndex) =>
              for
                _ <- atomIndex.traverse_(i => session.execute(Statements.SetAtomIndex)(i, aid))
                _ <- session.execute(Statements.ShiftUnstartedSteps)(count, aid, idx)
              yield (aid, idx)
            case StepInsertion.Target.NewAtom(atomIndex)            =>
              for
                aid <- UUIDGen[F].randomUUID.map(Uid[Atom.Id].isoUuid.reverseGet)
                rows = List((aid, observationId, sequenceType, instrument, atomIndex, none[String]))
                _   <- session.execute(Statements.insertAtoms(rows))(rows)
              yield (aid, 0)
          )

        def writeSteps(aid: Atom.Id, firstIndex: Int, steps: NonEmptyList[(Step.Id, ProtoStep[D], StepEstimate)]): F[Unit] =
          val stepRows = steps.toList.zipWithIndex.map { case ((sid, step, est), i) =>
            (sid, aid, instrument, step.stepConfig.stepType, firstIndex + i, step.observeClass,
             est.total, step.telescopeConfig, step.breakpoint)
          }
          val gcalRows = steps.toList.flatMap((sid, step, _) => StepConfig.gcal.getOption(step.stepConfig).tupleLeft(sid))
          val instRows = steps.toList.map((sid, step, _) => (sid, step.value))
          for
            _ <- session.execute(Statements.insertSteps(stepRows))(stepRows)
            _ <- session.execute(Statements.insertGcalConfigs(gcalRows))(gcalRows).whenA(gcalRows.nonEmpty)
            _ <- session.execute(insertInstConfig(instRows))(instRows)
          yield ()

        for
          _      <- ResultT.liftF(session.unique(Statements.LockObservationExecution)(observationId))
          rows   <- ResultT.liftF(session.execute(Statements.selectInsertionRows(table))((instrument, observationId, sequenceType)))
          visit  <- ResultT.liftF(session.option(Statements.SelectCurrentVisit)(observationId))
          plan   <- ResultT.fromResult(Result.fromEither(StepInsertion.plan(rows, afterStepId, visit).leftMap(m => OdbError.InvalidArgument(m.some).asProblem)))
          steps  <- smartGcalTypes.flatTraverse(expand(plan.reference, _))
          _      <- checkUnsplittable(rows, plan.target, steps.length)
          last    = plan.previous.fold(StepTimeEstimateCalculator.Last.empty[D])(StepTimeEstimateCalculator.Last.empty[D].next)
          ests    = steps.traverse(estimator.estimateOne(static, _)).runA(last).value
          ids    <- ResultT.liftF(steps.traverse(_ => UUIDGen[F].randomUUID.map(Uid[Step.Id].isoUuid.reverseGet)))
          target <- ResultT.liftF(writeAtom(plan, steps.length))
          _      <- ResultT.liftF(writeSteps(target._1, target._2, ids.zip(steps).zip(ests).map { case ((i, s), e) => (i, s, e) }))
          _      <- ResultT.liftF(markMaterializedOrUpdate(observationId, sequenceType, customized = true))
        yield ids

      override def insertSmartGcal(
        observationId:   Observation.Id,
        sequenceType:    SequenceType,
        smartGcalTypes:  NonEmptyList[SmartGcalType],
        afterStepId:     Option[Step.Id],
        calibrationRole: Option[CalibrationRole]
      )(using Transaction[F]): F[Result[NonEmptyList[Step.Id]]] =

        val expander = SmartGcalImplementation.fromService(smartGcalService)

        def insert[S, D](
          instrument:       Instrument,
          static:           ResultT[F, S],
          table:            Statements.DynamicTable[D],
          estimator:        StepTimeEstimateCalculator[S, D],
          expander:         SmartGcalExpander[F, S, D],
          insertInstConfig: (rows: List[(Step.Id, D)]) => Command[rows.type]
        ): ResultT[F, NonEmptyList[Step.Id]] =
          static.flatMap: s =>
            insertSmartGcalSteps(
              instrument,
              observationId,
              sequenceType,
              smartGcalTypes,
              afterStepId,
              calibrationRole.gcalClass,
              s,
              table,
              estimator,
              expander,
              insertInstConfig
            )

        val ghostStatic: ResultT[F, GhostStaticConfig] =
          ResultT(selectOrComputeGhostStatic(observationId).map(e => Result.fromEither(e.leftMap(_.asProblem))))

        ResultT(observationService.selectInstrument(observationId).map(Result.success))
          .flatMap:
            case Some(Instrument.Flamingos2) =>
              insert(Instrument.Flamingos2, selectStatic(observationId, "Flamingos 2", flamingos2SequenceService.selectStaticOrDefault), Statements.Flamingos2Table, estimator.flamingos2Step, expander.flamingos2, Flamingos2SequenceService.Statements.insertDynamics)
            case Some(Instrument.Ghost)      =>
              insert(Instrument.Ghost, ghostStatic, Statements.GhostTable, estimator.ghostStep, expander.ghost, GhostSequenceService.Statements.insertDynamics)
            case Some(Instrument.GmosNorth)  =>
              insert(Instrument.GmosNorth, selectStatic(observationId, "GMOS North", gmosSequenceService.selectGmosNorthStaticOrDefault), Statements.GmosNorthTable, estimator.gmosNorthStep, expander.gmosNorth, GmosSequenceService.Statements.insertNorthDynamics)
            case Some(Instrument.GmosSouth)  =>
              insert(Instrument.GmosSouth, selectStatic(observationId, "GMOS South", gmosSequenceService.selectGmosSouthStaticOrDefault), Statements.GmosSouthTable, estimator.gmosSouthStep, expander.gmosSouth, GmosSequenceService.Statements.insertSouthDynamics)
            case Some(Instrument.Igrins2)    =>
              insert(Instrument.Igrins2, selectStatic(observationId, "IGRINS-2", igrins2SequenceService.selectStaticOrDefault), Statements.Igrins2Table, estimator.igrins2Step, expander.igrins2, Igrins2SequenceService.Statements.insertDynamics)
            case Some(Instrument.Gnirs)      =>
              insert(Instrument.Gnirs, selectStatic(observationId, "GNIRS", gnirsSequenceService.selectStaticOrDefault), Statements.GnirsTable, estimator.gnirsStep, expander.gnirs, GnirsSequenceService.Statements.insertDynamics)
            case Some(i)                     =>
              ResultT(OdbError.InvalidArgument(s"Smart GCAL steps cannot be inserted into a ${i.longName} sequence.".some).asFailureF)
            case None                        =>
              ResultT(OdbError.InvalidArgument(s"Observation $observationId has no instrument.".some).asFailureF)
          .value

  object Statements:

    val AbandonAndDeleteUnexecuted: Command[(Observation.Id, SequenceType)] =
      sql"""
        CALL abandon_ongoing_and_delete_unexecuted_steps($observation_id, $sequence_type)
      """.command

    private val atom_row: Codec[(
      Atom.Id,
      Observation.Id,
      SequenceType,
      Instrument,
      Int,
      Option[String]
    )] =
      atom_id *: observation_id *: sequence_type *: instrument *: int4 *: text.opt

    def insertAtoms(rows: List[(
      Atom.Id,
      Observation.Id,
      SequenceType,
      Instrument,
      Int,
      Option[String]
    )]): Command[rows.type] =
      val enc = atom_row.values.list(rows)
      sql"""
        INSERT INTO t_atom (
          c_atom_id,
          c_observation_id,
          c_sequence_type,
          c_instrument,
          c_atom_index,
          c_description
        ) VALUES $enc
      """.command

    private val step_row: Codec[(
      Step.Id,
      Atom.Id,
      Instrument,
      StepType,
      Int,
      ObserveClass,
      TimeSpan,
      TelescopeConfig,
      Breakpoint
    )] =
      step_id *: atom_id *: instrument *: step_type *: int4 *: obs_class *: time_span *:
        telescope_config *: breakpoint

    def insertSteps(rows: List[(
      Step.Id,
      Atom.Id,
      Instrument,
      StepType,
      Int,
      ObserveClass,
      TimeSpan,
      TelescopeConfig,
      Breakpoint
    )]): Command[rows.type] =
      val enc = step_row.values.list(rows)
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
        ) VALUES $enc
      """.command

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

    private val gcal_row: Codec[(Step.Id, StepConfig.Gcal)] =
      step_id *: step_config_gcal

    def insertGcalConfigs(rows: List[(Step.Id, StepConfig.Gcal)]): Command[rows.type] =
      val enc = gcal_row.values.list(rows)
      sql"""
        ${insertStepConfigFragment("t_step_config_gcal", StepConfigGcalColumns)} VALUES $enc
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
      atom_id         *:
      obs_class       *:
      time_span       *:
      time_span       *:
      _step_type      *:
      _gcal_lamp_type *:
      int4_nonneg     *:
      int4_pos
    ).imap { case (a, c, n, p, ss, ls, nonNeg, pos) =>
      AtomDigest(
        a,
        c,
        CategorizedTime(ChargeClass.NonCharged -> n, ChargeClass.Program -> p),
        ss.toSet,
        ls.toSet,
        nonNeg,
        pos
      )
    } { (a: AtomDigest) => (
      a.id,
      a.observeClass,
      a.timeEstimate.nonCharged,
      a.timeEstimate.programTime,
      a.stepTypes.toList.sorted,
      a.lampTypes.toList.sorted,
      a.stepIndex,
      a.stepCount
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
          c_lamp_types,
          c_step_index,
          c_step_count
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

    final case class DynamicTable[D](
      name:    String,
      columns: List[String],
      decoder: Decoder[D]
    )

    val Flamingos2Table: DynamicTable[Flamingos2DynamicConfig] =
      DynamicTable("t_flamingos_2_dynamic", Flamingos2SequenceService.Statements.Flamingos2DynamicColumns, flamingos_2_dynamic)

    val GhostTable: DynamicTable[GhostDynamicConfig] =
      DynamicTable("t_ghost_dynamic", GhostSequenceService.Statements.DynamicColumns, ghost_dynamic)

    val GmosNorthTable: DynamicTable[GmosNorth] =
      DynamicTable("t_gmos_north_dynamic", GmosSequenceService.Statements.GmosDynamicColumns, gmos_north_dynamic)

    val GmosSouthTable: DynamicTable[GmosSouth] =
      DynamicTable("t_gmos_south_dynamic", GmosSequenceService.Statements.GmosDynamicColumns, gmos_south_dynamic)

    val Igrins2Table: DynamicTable[Igrins2DynamicConfig] =
      DynamicTable("t_igrins_2_dynamic", Igrins2SequenceService.Statements.Igrins2DynamicColumns, igrins_2_dynamic)

    val GnirsTable: DynamicTable[GnirsDynamicConfig] =
      DynamicTable("t_gnirs_dynamic", GnirsSequenceService.Statements.GnirsDynamicColumns, gnirs_dynamic)

    private def protoStep[D](table: DynamicTable[D]): Decoder[ProtoStep[D]] =
      (
        table.decoder     *:
        step_config       *:
        telescope_config  *:
        obs_class         *:
        breakpoint
      ).to[ProtoStep[D]]

    // The columns decoded by `protoStep`.
    private def protoStepColumns[D](table: DynamicTable[D]): String =
      s"""
          ${encodeColumns("i".some, table.columns)},
          s.c_step_type,
          ${encodeColumns("g".some, StepConfigGcalColumns)},
          ${encodeColumns("r".some, StepConfigSmartGcalColumns)},
          s.c_offset_p,
          s.c_offset_q,
          s.c_guide_state,
          s.c_observe_class,
          s.c_breakpoint
      """

    // Joins the tables holding the columns decoded by `protoStep`.
    private def stepsFrom[D](table: DynamicTable[D]): String =
      s"""
        FROM t_atom a

        JOIN t_step s
          ON s.c_atom_id = a.c_atom_id

        LEFT JOIN t_step_execution se ON se.c_step_id = s.c_step_id

        JOIN ${table.name} i
          ON i.c_step_id = s.c_step_id

        LEFT JOIN t_step_config_gcal g
          ON g.c_step_id = s.c_step_id

        LEFT JOIN t_step_config_smart_gcal r
          ON r.c_step_id = s.c_step_id
      """

    private def selectSequence[D](
      table:      DynamicTable[D],
      stepFilter: String,
      orderBy:    String
    ): Query[(Instrument, Observation.Id, SequenceType), (Atom.Id, Option[String], Step.Id, ProtoStep[D])] =

      sql"""
        SELECT
          a.c_atom_id,
          a.c_description,
          s.c_step_id,
          #${protoStepColumns(table)}

        #${stepsFrom(table)}

        WHERE
          a.c_instrument     = $instrument      AND
          a.c_observation_id = $observation_id  AND
          a.c_sequence_type  = $sequence_type
          #$stepFilter
        ORDER BY
          #$orderBy
      """.query(atom_id *: text.opt *: step_id *: protoStep(table))

    /**
     * Selects every step of a sequence, executed or not, along with what is
     * needed to work out where new steps may be inserted.
     */
    def selectInsertionRows[D](
      table: DynamicTable[D]
    ): Query[(Instrument, Observation.Id, SequenceType), StepInsertion.Row[D]] =
      sql"""
        SELECT
          a.c_atom_id,
          a.c_atom_index,
          s.c_step_id,
          s.c_step_index,
          se.c_execution_state,
          se.c_execution_order,
          se.c_visit_id,
          #${protoStepColumns(table)}

        #${stepsFrom(table)}

        WHERE
          a.c_instrument     = $instrument      AND
          a.c_observation_id = $observation_id  AND
          a.c_sequence_type  = $sequence_type
      """.query(
        atom_id                  *:
        int4                     *:
        step_id                  *:
        int4                     *:
        step_execution_state.opt *:
        int4.opt                 *:
        visit_id.opt             *:
        protoStep(table)
      ).to[StepInsertion.Row[D]]

    /**
     * Shifts the step index of the unstarted steps of an atom, starting at a
     * given index, to make room for inserted steps.
     */
    val ShiftUnstartedSteps: Command[(Int, Atom.Id, Int)] =
      sql"""
        UPDATE t_step s
           SET c_step_index = s.c_step_index + $int4
         WHERE s.c_atom_id     = $atom_id
           AND s.c_step_index >= $int4
           AND NOT EXISTS (
             SELECT 1
               FROM t_step_execution se
              WHERE se.c_step_id = s.c_step_id
                AND se.c_execution_state <> 'not_started'
           )
      """.command

    /** Increments the index of the given atoms. */
    def shiftAtoms(atoms: List[Atom.Id]): Command[atoms.type] =
      sql"""
        UPDATE t_atom
           SET c_atom_index = c_atom_index + 1
         WHERE c_atom_id IN (${atom_id.list(atoms)})
      """.command

    val SetAtomIndex: Command[(Int, Atom.Id)] =
      sql"""
        UPDATE t_atom
           SET c_atom_index = $int4
         WHERE c_atom_id    = $atom_id
      """.command

    /** The observation's most recently recorded visit, if any. */
    val SelectCurrentVisit: Query[Observation.Id, Visit.Id] =
      sql"""
        SELECT c_visit_id
          FROM t_visit
         WHERE c_observation_id = $observation_id
         ORDER BY c_recorded_time DESC
         LIMIT 1
      """.query(visit_id)

    /**
     * Takes the lock that serializes changes to an observation's execution
     * state, such as incoming step events.
     */
    val LockObservationExecution: Query[Observation.Id, Int] =
      sql"""
        SELECT 1 FROM lock_observation_execution($observation_id)
      """.query(int4)

    private def selectRemainingSequence[D](
      table: DynamicTable[D]
    ): Query[(Instrument, Observation.Id, SequenceType), (Atom.Id, Option[String], Step.Id, ProtoStep[D])] =
      selectSequence(
        table,
        "AND (se.c_step_id IS NULL OR se.c_execution_state IN ( 'not_started', 'ongoing' ))",
        "a.c_atom_index, s.c_step_index"
      )

    /**
     * Selects a sequence to copy into a clone.  Atoms are ordered by their
     * first executed step, then by atom index, and steps by execution order,
     * then by step index, so unexecuted atoms and steps follow executed ones.
     */
    def selectSequenceForClone[D](
      table:       DynamicTable[D],
      pendingOnly: Boolean
    ): Query[(Instrument, Observation.Id, SequenceType), (Atom.Id, Option[String], Step.Id, ProtoStep[D])] =
      selectSequence(
        table,
        if pendingOnly then "AND (se.c_step_id IS NULL OR se.c_execution_state = 'not_started')" else "",
        """
          (
            SELECT MIN(ae.c_execution_order)
            FROM t_step ast
            JOIN t_step_execution ae ON ae.c_step_id = ast.c_step_id
            WHERE ast.c_atom_id = a.c_atom_id
          ) NULLS LAST,
          a.c_atom_index,
          se.c_execution_order NULLS LAST,
          s.c_step_index
        """
      )

    val SelectFlamingos2Sequence: Query[(Instrument, Observation.Id, SequenceType), (Atom.Id, Option[String], Step.Id, ProtoStep[Flamingos2DynamicConfig])] =
      selectRemainingSequence(Flamingos2Table)

    val SelectGhostSequence: Query[(Instrument, Observation.Id, SequenceType), (Atom.Id, Option[String], Step.Id, ProtoStep[GhostDynamicConfig])] =
      selectRemainingSequence(GhostTable)

    val SelectGmosNorthSequence: Query[(Instrument, Observation.Id, SequenceType), (Atom.Id, Option[String], Step.Id, ProtoStep[GmosNorth])] =
      selectRemainingSequence(GmosNorthTable)

    val SelectGmosSouthSequence: Query[(Instrument, Observation.Id, SequenceType), (Atom.Id, Option[String], Step.Id, ProtoStep[GmosSouth])] =
      selectRemainingSequence(GmosSouthTable)

    val SelectIgrins2Sequence: Query[(Instrument, Observation.Id, SequenceType), (Atom.Id, Option[String], Step.Id, ProtoStep[Igrins2DynamicConfig])] =
      selectRemainingSequence(Igrins2Table)

    val SelectGnirsSequence: Query[(Instrument, Observation.Id, SequenceType), (Atom.Id, Option[String], Step.Id, ProtoStep[GnirsDynamicConfig])] =
      selectRemainingSequence(GnirsTable)

    val IsMaterialized: Query[(Observation.Id, SequenceType), Boolean] =
      sql"""
        SELECT EXISTS (
          SELECT 1
          FROM   t_sequence_materialization
          WHERE  c_observation_id = $observation_id
            AND  c_sequence_type  = $sequence_type
        )
      """.query(bool)

    /** Whether a materialized sequence is customized; no row if it is not materialized. */
    val SelectIsCustomized: Query[(Observation.Id, SequenceType), Boolean] =
      sql"""
        SELECT c_customized
        FROM   t_sequence_materialization
        WHERE  c_observation_id = $observation_id
          AND  c_sequence_type  = $sequence_type
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

    val MarkMaterializedOrUpdate: Query[(Observation.Id, SequenceType, Boolean), Boolean] =
      sql"""
        INSERT INTO t_sequence_materialization (
          c_observation_id,
          c_sequence_type,
          c_created,
          c_updated,
          c_customized
        )
        VALUES ($observation_id, $sequence_type, now(), now(), $bool)
        ON CONFLICT (c_observation_id, c_sequence_type)
        DO UPDATE SET c_updated = now(), c_customized = EXCLUDED.c_customized
        RETURNING (xmax = 0) AS inserted
      """.query(bool)

    val DeleteMaterialized: Query[(Observation.Id, SequenceType), Boolean] =
      sql"""
        WITH deleted_rows AS (
           DELETE FROM t_sequence_materialization
           WHERE c_observation_id = $observation_id
             AND c_sequence_type  = $sequence_type
           RETURNING 1
        )
        SELECT EXISTS (SELECT 1 FROM deleted_rows) AS deleted
      """.query(bool)
