// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.Context
import grackle.Predicate
import grackle.Predicate.*
import grackle.Query
import grackle.Query.*
import grackle.Result
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.input.EditAsterismsPatchInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.ObservationTimesInput
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

  /**
   * A `Checked` that knows the affect ids beforehand.
   */
  sealed abstract class CheckedWithIds[+A,+B] extends Checked[A] {

    def idsOrEmpty: List[B] =
      foldWithIds(Nil)((_, nel) => nel.toList)

    def foldWithIds[C](ifEmpty: => C)(ifNonEmpty: (A, NonEmptyList[B]) => C): C =
      this match
        case Checked.Empty => ifEmpty
        case Checked.NonEmptyWithIds(set, ids, _) => ifNonEmpty(set, ids)

  }

  object Checked {

    /** An approved operation defined over ids returned by `IN` expression `which`. */    
    sealed abstract case class NonEmpty[A](SET: A, which: AppliedFragment) extends Checked[A]

    /** An approved operation defined over `ids`, encodable via `enc`. */    
    sealed abstract case class NonEmptyWithIds[A,B](SET: A, ids: NonEmptyList[B], enc: Encoder[B]) extends CheckedWithIds[A,B]

    /** An approved operation that is known to have no effect. */
    case object Empty extends CheckedWithIds[Nothing, Nothing]

  }

  def unchecked[A,B](SET: A, ids: List[B], enc: Encoder[B])(using SuperUserAccess): CheckedWithIds[A,B] =
    NonEmptyList.fromList(ids).fold(Checked.Empty): ids =>
      new Checked.NonEmptyWithIds(SET, ids, enc) {}

  def unchecked[A](SET: A, which: AppliedFragment)(using SuperUserAccess): Checked[A] =
    new Checked.NonEmpty(SET, which) {}

trait AccessControl[F[_]] extends Predicates[F] {

  def user: User
  def timeEstimateCalculator: TimeEstimateCalculatorImplementation.ForInstrumentMode
  def itcClient: ItcClient[F]
  def commitHash: CommitHash

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

  def selectForUpdate(
    input: UpdateObservationsInput, 
    includeCalibrations: Boolean
  )(using Services[F], 
          NoTransaction[F]
  ): F[Result[AccessControl.CheckedWithIds[ObservationPropertiesInput.Edit, Observation.Id]]] =
    Services.asSuperUser {

      def allowedStates(SET: ObservationPropertiesInput.Edit): Set[ObservationWorkflowState] =
        if
          SET.subtitle.isDefined            ||
          SET.scienceBand.isDefined         ||
          SET.posAngleConstraint.isDefined  ||
          SET.targetEnvironment.isDefined   ||
          SET.constraintSet.isDefined       ||
          SET.timingWindows.isDefined       ||
          SET.attachments.isDefined         ||
          SET.scienceRequirements.isDefined ||
          SET.observingMode.isDefined       ||
          SET.existence.isDefined           ||
          SET.observerNotes.isDefined
        then ObservationWorkflowState.preExecutionSet
        else ObservationWorkflowState.fullSet

      selectForObservationUpdateImpl(
        input.includeDeleted, 
        input.WHERE, 
        includeCalibrations, 
        allowedStates(input.SET)
      ).map(_.map(AccessControl.unchecked(input.SET, _, observation_id)))

    }
      
  def selectForUpdate(
    input: UpdateAsterismsInput,
    includeCalibrations: Boolean
  )(using Services[F], 
          NoTransaction[F]
  ): F[Result[AccessControl.CheckedWithIds[EditAsterismsPatchInput, Observation.Id]]] =
    Services.asSuperUser:
      selectForObservationUpdateImpl(
        input.includeDeleted, 
        input.WHERE, 
        includeCalibrations, 
        ObservationWorkflowState.preExecutionSet // not allowed once we start executing
      ).map(_.map(AccessControl.unchecked(input.SET, _, observation_id)))

  def selectForUpdate(
    input: UpdateObservationsTimesInput,
    includeCalibrations: Boolean
  )(using Services[F], 
          NoTransaction[F]
  ): F[Result[AccessControl.CheckedWithIds[ObservationTimesInput, Observation.Id]]]  =
    Services.asSuperUser:
      selectForObservationUpdateImpl(
        input.includeDeleted, 
        input.WHERE, 
        includeCalibrations, 
        ObservationWorkflowState.allButComplete // allowed unless we're complete
      ).map(_.map(AccessControl.unchecked(input.SET, _, observation_id)))

}
