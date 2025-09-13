// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import cats.Functor
import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.Context
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import grackle.Query
import grackle.Query.*
import grackle.Result
import grackle.ResultT
import grackle.Type
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Access
import lucuma.core.model.Observation
import lucuma.core.model.ObservationWorkflow
import lucuma.core.model.Program
import lucuma.core.model.ProgramNote
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.AllocationInput
import lucuma.odb.graphql.input.AttachmentPropertiesInput
import lucuma.odb.graphql.input.CallForProposalsPropertiesInput
import lucuma.odb.graphql.input.CloneObservationInput
import lucuma.odb.graphql.input.CloneTargetInput
import lucuma.odb.graphql.input.CreateCallForProposalsInput
import lucuma.odb.graphql.input.CreateObservationInput
import lucuma.odb.graphql.input.CreateProgramInput
import lucuma.odb.graphql.input.CreateProgramNoteInput
import lucuma.odb.graphql.input.CreateTargetInput
import lucuma.odb.graphql.input.EditAsterismsPatchInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.ObservationTimesInput
import lucuma.odb.graphql.input.ProgramNotePropertiesInput
import lucuma.odb.graphql.input.ProgramPropertiesInput
import lucuma.odb.graphql.input.ProgramReferencePropertiesInput
import lucuma.odb.graphql.input.ResetAcquisitionInput
import lucuma.odb.graphql.input.SetAllocationsInput
import lucuma.odb.graphql.input.SetGuideTargetNameInput
import lucuma.odb.graphql.input.SetObservationWorkflowStateInput
import lucuma.odb.graphql.input.SetProgramReferenceInput
import lucuma.odb.graphql.input.TargetPropertiesInput
import lucuma.odb.graphql.input.UpdateAsterismsInput
import lucuma.odb.graphql.input.UpdateAttachmentsInput
import lucuma.odb.graphql.input.UpdateCallsForProposalsInput
import lucuma.odb.graphql.input.UpdateObservationsInput
import lucuma.odb.graphql.input.UpdateObservationsTimesInput
import lucuma.odb.graphql.input.UpdateProgramNotesInput
import lucuma.odb.graphql.input.UpdateProgramsInput
import lucuma.odb.graphql.input.UpdateTargetsInput
import lucuma.odb.graphql.mapping.AccessControl.CheckedWithId
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.syntax.observationWorkflowState.*
import lucuma.odb.util.Codecs.*
import org.http4s.client.Client
import skunk.AppliedFragment
import skunk.Encoder
import skunk.Transaction
import skunk.syntax.stringcontext.*

object AccessControl:

  /**
   * A token whose presence indicates that an operation of type `A` has been checked and
   * is approved for execution by the current user, on the current request. In general
   * the affected ids are not known beforehand; however in some cases they *are* known
   * beforehand (see `CheckedWithIds`).
   */
  sealed trait Checked[+A]:
    
    def isEmpty: Boolean =
      fold(true)((_, _) => false)

    def fold[B](ifEmpty: => B)(ifNonEmpty: (A, AppliedFragment) => B): B =
      this match
        case Checked.Empty => ifEmpty
        case Checked.NonEmpty(set, which) => ifNonEmpty(set, which)
        case Checked.NonEmptyWithIds(set, ids, enc) => ifNonEmpty(set, sql"${enc.nel(ids)}"(ids))
        case Checked.NonEmptyWithId(set, id, enc) => ifNonEmpty(set, sql"$enc"(id))

  /** Specialization of `Checked` that knows the affected ids beforehand. */
  sealed abstract class CheckedWithIds[+A,+B] extends Checked[A] {

    def idsOrEmpty: List[B] =
      foldWithIds(Nil)((_, nel) => nel.toList)

    def foldWithIds[C](ifEmpty: => C)(ifNonEmpty: (A, NonEmptyList[B]) => C): C =
      this match
        case Checked.Empty => ifEmpty
        case Checked.NonEmptyWithIds(set, ids, _) => ifNonEmpty(set, ids)
        case Checked.NonEmptyWithId(set, id, _) => ifNonEmpty(set, NonEmptyList.one(id))

  }

  /** Specialization of `Checked` that knows there is at most a single affected id. */
  sealed abstract class CheckedWithId[+A,+B] extends CheckedWithIds[A,B] {
    def foldWithId[C](ifEmpty: => C)(ifNonEmpty: (A, B) => C): C =
      this match
        case Checked.NonEmptyWithId(set, id, _) => ifNonEmpty(set, id)
        case Checked.Empty => ifEmpty

  }

  object Checked {

    /** An approved operation defined over ids returned by `IN` expression `which`. */    
    sealed abstract case class NonEmpty[A](SET: A, which: AppliedFragment) extends Checked[A]

    /** An approved operation defined over `ids`, encodable via `enc`. */    
    sealed abstract case class NonEmptyWithIds[A,B](SET: A, ids: NonEmptyList[B], enc: Encoder[B]) extends CheckedWithIds[A,B]

    /** An approved operation defined over `id`, encodable via `enc`. */    
    sealed abstract case class NonEmptyWithId[A,B](SET: A, id: B, enc: Encoder[B]) extends CheckedWithId[A,B]

    /** An approved operation that is known to have no effect. */
    case object Empty extends CheckedWithId[Nothing, Nothing]

  }

  /** Construct a `CheckedWithIds` (requires SuperUser access). */
  def unchecked[A,B](SET: A, ids: List[B], enc: Encoder[B])(using SuperUserAccess): CheckedWithIds[A,B] =
    NonEmptyList.fromList(ids).fold(Checked.Empty): ids =>
      new Checked.NonEmptyWithIds(SET, ids, enc) {}

  /** Construct a `CheckedWithId` (requires SuperUser access). */
  def unchecked[A,B](SET: A, id: B, enc: Encoder[B])(using SuperUserAccess): CheckedWithId[A,B] =
      new Checked.NonEmptyWithId(SET, id, enc) {}

  /** Construct a `Checked` (requires SuperUser access). */
  def unchecked[A](SET: A, which: AppliedFragment)(using SuperUserAccess): Checked[A] =
    new Checked.NonEmpty(SET, which) {}

trait AccessControl[F[_]] extends Predicates[F] {

  def user: User
  def timeEstimateCalculator: TimeEstimateCalculatorImplementation.ForInstrumentMode
  def itcClient: ItcClient[F]
  def commitHash: CommitHash
  def httpClient: Client[F]
  def emailConfig: Config.Email

  // We do this a lot
  extension [F[_]: Functor, G[_]: Functor, A](fga: F[G[A]])
    def nestMap[B](fab: A => B): F[G[B]] = fga.map(_.map(fab))
    def nestAs[B](b: B): F[G[B]] = fga.map(_.as(b))

  /**
   * Construct an `AppliedFragment` that selects the ids of observations that are writable
   * by the current user and satisfy the supplied filters *without regard to workflow state*.
   */
  def writableOids(
    includeDeleted:      Option[Boolean],
    WHERE:               Option[Predicate],
    includeCalibrations: Boolean
  ): Result[AppliedFragment] =
    idSelectFromPredicate(
      ObservationType,
      and(List(
        Predicates.observation.program.isWritableBy(user),
        Predicates.observation.existence.includeDeleted(includeDeleted.getOrElse(false)),
        if (includeCalibrations) True else Predicates.observation.calibrationRole.isNull(true),
        WHERE.getOrElse(True)
      ))
    )


  /**
   * Select and return the ids of observations that are editable by the current user and meet
   * all the specified filters.
   */
  private def selectForObservationUpdateImpl(
    includeDeleted:      Option[Boolean],
    WHERE:               Option[Predicate],
    includeCalibrations: Boolean,
    allowedStates:       Set[ObservationWorkflowState]
  )(using Services[F], NoTransaction[F]): F[Result[List[Observation.Id]]] =
    Services.asSuperUser:
      writableOids(includeDeleted, WHERE, includeCalibrations)
        .flatTraverse: which =>
          observationWorkflowService.filterState(
            which, 
            allowedStates,
            commitHash,
            itcClient,
            timeEstimateCalculator
          )

  /**
   * Select and return the ids of observations that are clonable by the current user and meet
   * all the specified filters.
   */
  private def selectForObservationCloneImpl(
    includeDeleted:      Option[Boolean],
    WHERE:               Option[Predicate],
    includeCalibrations: Boolean,
  )(using Services[F], NoTransaction[F]): F[Result[List[Observation.Id]]] =
    writableOids(includeDeleted, WHERE, includeCalibrations)
      .flatTraverse: af =>
        session
          .prepareR(af.fragment.query(observation_id))
          .use: pq =>
            pq.stream(af.argument, 1024)
              .compile
              .toList
              .map: ids =>
                Result.success(ids)

  /** Verify that `oid` is writable. */
  private def verifyWritable(
    oid: Observation.Id
  )(using Services[F], NoTransaction[F]): F[Result[Unit]] =
    selectForObservationCloneImpl(
      includeDeleted = None,
      WHERE = Some(Predicates.observation.id.eql(oid)),
      includeCalibrations = false
    ).map: res =>
      res.flatMap:
        case List(`oid`) => Result.unit
        case Nil         => OdbError.NotAuthorized(user.id).asFailure
        case _           => Result.internalError("Unpossible: selectForObservationCloneImpl returned multiple ids, or the wrong id")

  /**
   * Construct a `SELECT` statement that returns the `"id"` fields of all objects
   * of type `Type` that match the given `Predicate`. Note that the query generated
   * here is not subject to elaboration; the predicate passed here must express
   * exactly what is needed.
   */
  protected def idSelectFromPredicate(tpe: Type, pred: Predicate): Result[Fragment] =
    Context(Path.from(tpe)).flatMap: ctx =>
      val q = Filter(pred, Select("id", None, Query.Empty))
      MappedQuery(q, ctx).flatMap(_.fragment)

  /**
   * Select and return the ids of programs that are editable by the current user and meet
   * all the specified filters.
   */
  private def selectForProgramUpdateImpl(
    includeDeleted: Option[Boolean],
    WHERE:          Option[Predicate]
  )(using Services[F], NoTransaction[F]): F[Result[List[Program.Id]]] =
    idSelectFromPredicate(
      ProgramType,
      and(List(
        Predicates.program.isWritableBy(user),
        Predicates.program.existence.includeDeleted(includeDeleted.getOrElse(false)),
        WHERE.getOrElse(True)
      ))
    ).flatTraverse: frag =>
      session.prepareR(frag.fragment.query(program_id)).use: pq =>
        pq.stream(frag.argument, 1024)
          .compile
          .toList
          .map(Result.success)

  /**
   * Compute the subset of `pids` that identify programs which are editable by the current user.
   */
  private def selectForProgramUpdateImpl(
    includeDeleted: Option[Boolean],
    pids:           List[Program.Id]
  )(using Services[F], NoTransaction[F]): F[Result[List[Program.Id]]] =
    selectForProgramUpdateImpl(
      includeDeleted, 
      Some(Predicates.program.id.in(pids))
    )

  /**
   * Select and return the ids of targets that are editable by the current user and meet
   * all the specified filters.
   */
  private def selectForTargetUpdateImpl(
    includeDeleted:      Option[Boolean],
    WHERE:               Option[Predicate],
    allowedStates:       Set[ObservationWorkflowState]
  )(using Services[F], NoTransaction[F]): F[Result[List[Target.Id]]] =
    Services.asSuperUser:
      idSelectFromPredicate(
        TargetType,
        and(List(
          Predicates.target.program.isWritableBy(user),
          Predicates.target.existence.includeDeleted(includeDeleted.getOrElse(false)),
          WHERE.getOrElse(True)
        ))
      ).flatTraverse: which =>
        observationWorkflowService.filterTargets(
          which,
          allowedStates,
          commitHash,
          itcClient,
          timeEstimateCalculator
        )

  /**
   * Given an operation that defines a set of targets and a proposed edit, select and filter this
   * set based on access control policies and return a checked edit that is valid for execution.
   */
  def selectForUpdate(
    input: UpdateTargetsInput,
  )(using Services[F], NoTransaction[F]): F[Result[AccessControl.CheckedWithIds[TargetPropertiesInput.Edit, Target.Id]]] =
    selectForTargetUpdateImpl(
      input.includeDeleted,
      input.WHERE,
      ObservationWorkflowState.preExecutionSet,      
    ).nestMap: tids =>
      Services.asSuperUser:
        AccessControl.unchecked(input.SET, tids, target_id)

  /** Overload of `selectForObservationUpdateImpl` that takes a list of oids instead of a `Predicate`.  */
  private def selectForObservationUpdateImpl(
    includeDeleted:      Option[Boolean],
    oids:                List[Observation.Id],
    includeCalibrations: Boolean,
    allowedStates:       Set[ObservationWorkflowState]
  )(using Services[F], NoTransaction[F]): F[Result[List[Observation.Id]]] = 
    selectForObservationUpdateImpl(
      includeDeleted,
      Some(Predicates.observation.id.in(oids)),
      includeCalibrations,
      allowedStates
    )

  /**
   * Given an operation that defines a set of observations and a proposed edit, select and filter this
   * set based on access control policies and return a checked edit that is valid for execution.
   */
  def selectForUpdate(
    input: UpdateObservationsInput, 
    includeCalibrations: Boolean
  )(using Services[F], 
          NoTransaction[F]
  ): F[Result[AccessControl.CheckedWithIds[ObservationPropertiesInput.Edit, Observation.Id]]] =
    {

      // Which workflow states would permit the proposed update (also taking user into account)?
      val allowedStates: Set[ObservationWorkflowState] =
        val SET = input.SET
        if
          (SET.posAngleConstraint.isDefined && user.role.access <= Access.Pi) || // staff are allowed to do this
          SET.subtitle.isDefined            ||
          SET.scienceBand.isDefined         ||
          SET.targetEnvironment.isDefined   ||
          SET.constraintSet.isDefined       ||
          SET.timingWindows.isDefined       ||
          SET.attachments.isDefined         ||
          SET.scienceRequirements.isDefined ||
          SET.observingMode.isDefined       ||
          SET.existence.isDefined           ||
          SET.observerNotes.isDefined
        then ObservationWorkflowState.preExecutionSet // ok prior to execution
        else ObservationWorkflowState.fullSet         // always ok

      selectForObservationUpdateImpl(
        input.includeDeleted, 
        input.WHERE, 
        includeCalibrations, 
        allowedStates
      ).nestMap: oids =>
        Services.asSuperUser:
          AccessControl.unchecked(input.SET, oids, observation_id)

    }
      
  /**
   * Given an operation that defines a set of observations and a proposed edit, select and filter this
   * set based on access control policies and return a checked edit that is valid for execution.
   */
  def selectForUpdate(
    input: UpdateAsterismsInput,
    includeCalibrations: Boolean
  )(using Services[F], 
          NoTransaction[F]
  ): F[Result[AccessControl.CheckedWithIds[EditAsterismsPatchInput, Observation.Id]]] =
    selectForObservationUpdateImpl(
      input.includeDeleted, 
      input.WHERE, 
      includeCalibrations, 
      ObservationWorkflowState.preExecutionSet // not allowed once we start executing
    ).nestMap: oids =>
      Services.asSuperUser:
        AccessControl.unchecked(input.SET, oids, observation_id)

  /**
   * Given an operation that defines a set of observations and a proposed edit, select and filter this
   * set based on access control policies and return a checked edit that is valid for execution.
   */
  def selectForUpdate(
    input: UpdateObservationsTimesInput,
    includeCalibrations: Boolean
  )(using Services[F], 
          NoTransaction[F]
  ): F[Result[AccessControl.CheckedWithIds[ObservationTimesInput, Observation.Id]]]  =
    selectForObservationUpdateImpl(
      input.includeDeleted, 
      input.WHERE, 
      includeCalibrations, 
      ObservationWorkflowState.allButComplete // allowed unless we're complete
    ).nestMap: oids =>
      Services.asSuperUser:
        AccessControl.unchecked(input.SET, oids, observation_id)

  /**
   * Given an operation that defines a set of observations and a proposed edit, select and filter this
   * set based on access control policies and return a checked edit that is valid for execution.
   */
  def selectForUpdate(
    input: SetGuideTargetNameInput,
    includeCalibrations: Boolean
  )(using Services[F],
          NoTransaction[F]
  ): F[Result[AccessControl.CheckedWithId[SetGuideTargetNameInput, Observation.Id]]] =
    Services.asSuperUser:
      observationService.resolveOid(input.observationId, input.observationRef).flatMap: r =>
        r.flatTraverse: oid =>
          selectForObservationUpdateImpl(
            None,
            List(oid),
            includeCalibrations,
            if user.role.access <= Access.Pi 
              then ObservationWorkflowState.preExecutionSet
              else ObservationWorkflowState.allButComplete
          ).map: r =>
            r.flatMap:
              case Nil => Result(AccessControl.Checked.Empty)
              case List(x) => Services.asSuperUser(Result(AccessControl.unchecked(input, x, observation_id)))
              case _ => Result.internalError("Unpossible: got more than one oid back")

  // Resolve to a Program.Id if the corresponding program is writable by the user.
  def resolvePidWritable(
    pid:  Option[Program.Id],
    prop: Option[ProposalReference],
    prog: Option[ProgramReference]
  )(using Services[F]): F[Result[Option[Program.Id]]] =
    programService(emailConfig, httpClient)
      .resolvePid(pid, prop, prog)
      .flatMap: r =>
        r.flatTraverse: pid =>
          selectForProgramUpdateImpl(None, List(pid))
            .map: r =>
              r.flatMap:
                case List(pid) => Result(Some(pid))
                case Nil       => Result(None)
                case _         => Result.internalError("Unpossible: resovePidWritable returned multiple ids")

  def selectForUpdate(
    input: CreateObservationInput,
  )(using Services[F]): F[Result[AccessControl.CheckedWithId[ObservationPropertiesInput.Create, Program.Id]]] = {

    // Compute our ObservationPropertiesInput.Create and set/verify the science band
    def props(pid: Program.Id): F[Result[ObservationPropertiesInput.Create]] =
      Services.asSuperUser:
        val props = input.SET.getOrElse(ObservationPropertiesInput.Create.Default)
        props.scienceBand match
          case None =>
            allocationService
              .selectScienceBands(pid)
              .map(_.toList)
              .map:
                case List(b) => Result(props.copy(scienceBand = Some(b)))
                case _       => Result(props)
          case Some(band) =>
            allocationService.validateBand(band, List(pid)).map(_.as(props))

    // Put it together
    ResultT(resolvePidWritable(input.programId, input.proposalReference, input.programReference))
      .flatMap:
        case None => ResultT.pure(AccessControl.Checked.Empty)
        case Some(pid) =>
          ResultT(props(pid))
            .map: props =>
              Services.asSuperUser:
                AccessControl.unchecked(props, pid, program_id)
      .value

  } 

  def selectForUpdate(
    input: CreateTargetInput,
  )(using Services[F]): F[Result[AccessControl.CheckedWithId[TargetPropertiesInput.Create, Program.Id]]] = {
   ResultT(resolvePidWritable(input.programId, input.proposalReference, input.programReference))
      .map:
        case None => AccessControl.Checked.Empty
        case Some(pid) =>
          Services.asSuperUser:
            AccessControl.unchecked(input.SET, pid, program_id)
      .value

  } 

  def selectForClone(
    input: CloneObservationInput
  )(using Services[F]): F[Result[AccessControl.CheckedWithId[Option[ObservationPropertiesInput.Edit], Observation.Id]]] = {

    val ensureWritable: F[Result[Option[Observation.Id]]] =
      Services.asSuperUser:
        observationService
          .resolveOid(input.observationId, input.observationRef)
          .flatMap: r =>
            r.flatTraverse: oid =>
              selectForObservationCloneImpl(
                includeDeleted = None,
                WHERE = Some(Predicates.observation.id.eql(oid)),
                includeCalibrations = false
              ).map: res =>
                res.flatMap:
                  case List(oid) => Result(Some(oid))
                  case Nil       => Result(None)
                  case _         => Result.internalError("Unpossible: selectForObservationCloneImpl returned multiple ids")

    ensureWritable.nestMap:
      case None      => AccessControl.Checked.Empty
      case Some(oid) =>
        Services.asSuperUser:
          AccessControl.unchecked(input.SET, oid, observation_id)

  }

  def selectForUpdate(
    input: ResetAcquisitionInput,
  )(using Services[F]): F[Result[AccessControl.CheckedWithId[Unit, Observation.Id]]] =
     requireStaffAccess:
      Services.asSuperUser:
        observationService
          .resolveOid(input.observationId, input.observationRef)
          .nestMap: oid =>
            AccessControl.unchecked((), oid, observation_id)

  def selectForUpdate(
    input: CreateProgramNoteInput
  )(using Services[F]): F[Result[AccessControl.CheckedWithId[ProgramNotePropertiesInput.Create, Program.Id]]] =
    (
      if input.SET.isPrivate && user.role.access < Access.Staff then
        ResultT.pure(AccessControl.Checked.Empty)
      else
        ResultT(resolvePidWritable(input.programId, input.proposalReference, input.programReference))
          .flatMap:
            case None      => ResultT.pure(AccessControl.Checked.Empty)
            case Some(pid) =>
              Services.asSuperUser:
                ResultT.pure(AccessControl.unchecked(input.SET, pid, program_id))
    ).value

  @annotation.nowarn("msg=unused implicit parameter")
  def selectForUpdate(
    input: SetProgramReferenceInput
  )(using Services[F], Transaction[F]): F[Result[AccessControl.CheckedWithId[ProgramReferencePropertiesInput, Program.Id]]] =
    programService(emailConfig, httpClient)
      .resolvePid(input.programId, input.proposalReference, input.programReference)
      .flatMap: r =>
        r.flatTraverse: pid =>
          requireStaffAccess: // this is the only access control check
            Services.asSuperUser:
              Result(AccessControl.unchecked(input.SET, pid, program_id)).pure[F]

  @annotation.nowarn("msg=unused implicit parameter")
  def selectForUpdate(
    input: UpdateProgramsInput
  )(using Services[F], Transaction[F]): F[Result[AccessControl.Checked[ProgramPropertiesInput.Edit]]] =
    idSelectFromPredicate(
      ProgramType,
      and(List(
        Predicates.program.isWritableBy(user),
        Predicates.program.existence.includeDeleted(input.includeDeleted.getOrElse(false)),
        input.WHERE.getOrElse(True)
      ))
    ) .map: frag =>
        Services.asSuperUser:
          AccessControl.unchecked(input.SET, frag)
      .pure[F]

  def selectForUpdate(
    input: CreateProgramInput
  ): F[Result[AccessControl.Checked[Option[ProgramPropertiesInput.Create]]]] =
    Services.asSuperUser:
      Result(AccessControl.unchecked(input.SET, AppliedFragment.empty)).pure[F] // always ok, for now

  def selectForUpdate(
    input: SetAllocationsInput
  )(using Services[F]): F[Result[AccessControl.CheckedWithId[List[AllocationInput], Program.Id]]] =
    requireStaffAccess:
      programService(emailConfig, httpClient)
        .resolvePid(input.programId, input.proposalReference, input.programReference)
        .map: r =>
          r.map: pid =>
            Services.asSuperUser:
              AccessControl.unchecked(input.allocations, pid, program_id)

//    requireStaffAccess: // this is the only check
//      Services.asSuperUser:
//        Result(AccessControl.unchecked(input.allocations, input.programId, program_id)).pure[F]

  @annotation.nowarn("msg=unused implicit parameter")
  def selectForUpdate(
    input: UpdateAttachmentsInput
  )(using Services[F]): F[Result[AccessControl.Checked[AttachmentPropertiesInput.Edit]]] =
    idSelectFromPredicate(
      AttachmentType,
      and(List(
        Predicates.attachment.program.isWritableBy(user),
        input.WHERE.getOrElse(True)
      ))
    ).traverse: af =>
      Services.asSuperUser:
        AccessControl.unchecked(input.SET, af).pure[F]

  def selectForUpdate(
    input: CreateCallForProposalsInput
  )(using Services[F]): F[Result[AccessControl.Checked[CallForProposalsPropertiesInput.Create]]] =
    requireStaffAccess: // this is the only check
      Services.asSuperUser:
        Result(AccessControl.unchecked(input.SET, AppliedFragment.empty)).pure[F]

  def selectForUpdate(
    input: UpdateCallsForProposalsInput
  )(using Services[F]): F[Result[AccessControl.Checked[CallForProposalsPropertiesInput.Edit]]] =
    requireStaffAccess: // this is the only check
      idSelectFromPredicate(
        CallForProposalsType,
        and(List(
          Predicates.callForProposals.existence.includeDeleted(input.includeDeleted.getOrElse(false)),
          input.WHERE.getOrElse(True)
        ))
      ).flatTraverse: which =>
        Services.asSuperUser:
          Result(AccessControl.unchecked(input.SET, which)).pure[F]

  private def selectForProgramNoteUpdateImpl(
    includeDeleted: Option[Boolean],
    WHERE:          Option[Predicate]
  )(using Services[F], NoTransaction[F]): F[Result[List[ProgramNote.Id]]] =
    idSelectFromPredicate(
      ProgramNoteType,
      and(List(
        Predicates.programNote.isWritableBy(user),
        Predicates.programNote.existence.includeDeleted(includeDeleted.getOrElse(false)),
        WHERE.getOrElse(True)
      ))
    ).flatTraverse: af =>
      session.prepareR(af.fragment.query(program_note_id)).use: pq =>
        pq.stream(af.argument, 1024)
          .compile
          .toList
          .map(Result.success)

  def selectForUpdate(
    input: UpdateProgramNotesInput
  )(using Services[F]): F[Result[AccessControl.CheckedWithIds[ProgramNotePropertiesInput.Edit, ProgramNote.Id]]] =
    if input.SET.isPrivate.contains(true) && user.role.access < Access.Staff then
      Result(AccessControl.Checked.Empty).pure
    else
      selectForProgramNoteUpdateImpl(input.includeDeleted, input.WHERE)
        .map(_.map { nids =>
           Services.asSuperUser:
             AccessControl.unchecked(input.SET, nids, program_note_id)
        })

  def selectForUpdate(
    input: CloneTargetInput
  )(using Services[F], NoTransaction[F]): F[Result[CheckedWithId[CloneTargetInput, Program.Id]]] =

    val editableProgram: ResultT[F, (Target.Id, Program.Id)] =
      ResultT:
        MappedQuery(
          Filter(
            And(
              Predicates.target.id.eql(input.targetId),
              Predicates.target.program.isWritableBy(user)
            ),
            Group(List(
              Select("id", None),
              Select("program", Select("id", None))
            ))
          ),
          Context(QueryType, List("targets"), List("targets"), List(TargetType))
        ).flatMap(_.fragment)
          .flatTraverse: af =>
            // N.B. the ordering of return columns here is not obvious
            session.prepareR(af.fragment.query(program_id *: target_id)).use: pq =>
              pq.option(af.argument).map:
                case None => OdbError.InvalidTarget(input.targetId).asFailure
                case Some(pair) => Result(pair.reverse)

    def editableObservations(pid: Program.Id): ResultT[F, List[Observation.Id]] =
      input.REPLACE_IN.map(_.toList).fold(ResultT.pure(Nil)): oids =>
        ResultT:
          selectForObservationUpdateImpl(
            includeDeleted = None,
            WHERE = Some(
              And(
                Predicates.observation.id.in(oids),
                Predicates.observation.program.id.eql(pid)
              )
            ),
            includeCalibrations = false,
            allowedStates = ObservationWorkflowState.preExecutionSet
          ) 

    editableProgram
      .flatMap: (tid, pid) =>
        editableObservations(pid)
          .map: oids =>
            Services.asSuperUser:
              AccessControl.unchecked(CloneTargetInput(tid, input.SET, NonEmptyList.fromList(oids)), pid, program_id)
      .value


  def selectForUpdate(input: SetObservationWorkflowStateInput)(using Services[F], NoTransaction[F]): F[Result[CheckedWithId[(ObservationWorkflow, ObservationWorkflowState), Observation.Id]]] =
    verifyWritable(input.observationId) >>
    Services.asSuperUser:
      observationWorkflowService.getWorkflows(List(input.observationId), commitHash, itcClient, timeEstimateCalculator)
        .map: res =>
          res.map(_(input.observationId)).flatMap: w =>
            if w.state === input.state || w.validTransitions.contains(input.state)
            then Result(AccessControl.unchecked((w, input.state), input.observationId, observation_id))
            else Result.failure(OdbError.InvalidWorkflowTransition(w.state, input.state).asProblem)

}
