// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import cats.Functor
import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.Context
import grackle.Predicate
import grackle.Predicate.*
import grackle.Query
import grackle.Query.*
import grackle.Result
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Access
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.input.EditAsterismsPatchInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.ObservationTimesInput
import lucuma.odb.graphql.input.SetGuideTargetNameInput
import lucuma.odb.graphql.input.UpdateAsterismsInput
import lucuma.odb.graphql.input.UpdateObservationsInput
import lucuma.odb.graphql.input.UpdateObservationsTimesInput
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.syntax.observationWorkflowState.*
import lucuma.odb.util.Codecs.*
import skunk.AppliedFragment
import skunk.Encoder
import skunk.syntax.stringcontext.*

object AccessControl:

  /**
   * A token whose presence indicates that an operation of type `A` has been checked and
   * is approved for execution by the current user, on the current request. In general
   * the affected ids are not known beforehand; however in some cases they *are* known
   * beforehand (see `CheckedWithIds`).
   */
  sealed trait Checked[+A]:
    
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

  // We do this a lot
  extension [F[_]: Functor, G[_]: Functor, A](fga: F[G[A]])
    def nestMap[B](fab: A => B): F[G[B]] = fga.map(_.map(fab))
    def nestAs[B](b: B): F[G[B]] = fga.map(_.as(b))

  /**
   * Select and return the ids of observations that are editable by the current user and meet
   * all the specified filters.
   */
  private def selectForObservationUpdateImpl(
    includeDeleted:      Option[Boolean],
    WHERE:               Option[Predicate],
    includeCalibrations: Boolean,
    allowedStates:       Set[ObservationWorkflowState]
  )(using Services[F], NoTransaction[F]): F[Result[List[Observation.Id]]] =  {

    def observationIdWhereClause(
      includeDeleted:      Option[Boolean],
      WHERE:               Option[Predicate],
      includeCalibrations: Boolean
    ): Result[AppliedFragment] = {
      val whereObservation: Predicate =
        and(List(
          Predicates.observation.program.isWritableBy(user),
          Predicates.observation.existence.includeDeleted(includeDeleted.getOrElse(false)),
          if (includeCalibrations) True else Predicates.observation.calibrationRole.isNull(true),
          WHERE.getOrElse(True)
        ))
      MappedQuery(
        Filter(whereObservation, Select("id", None, Query.Empty)),
        Context(QueryType, List("observations"), List("observations"), List(ObservationType))
      ).flatMap(_.fragment)
    }

    Services.asSuperUser:
      observationIdWhereClause(includeDeleted, WHERE, includeCalibrations)
        .flatTraverse: which =>
          observationWorkflowService.filterState(
            which, 
            allowedStates,
            commitHash,
            itcClient,
            timeEstimateCalculator
          )

  }

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
  ): F[Result[AccessControl.CheckedWithId[SetGuideTargetNameInput, Observation.Id]]]  =
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
    
}
