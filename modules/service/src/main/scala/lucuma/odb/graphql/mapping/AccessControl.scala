// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import cats.syntax.all.*
import lucuma.odb.graphql.predicate.Predicates
import grackle.Predicate
import grackle.Predicate.*
import grackle.Result
import skunk.AppliedFragment
import grackle.Query
import grackle.Query.*
import grackle.Context
import lucuma.core.enums.ObservationWorkflowState
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.service.NoTransaction
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.itc.client.ItcClient
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.syntax.observationWorkflowState.*
import lucuma.odb.graphql.input.UpdateObservationsInput
import lucuma.odb.graphql.input.UpdateAsterismsInput
import lucuma.odb.graphql.input.UpdateObservationsTimesInput
import lucuma.odb.graphql.input.EditAsterismsPatchInput

object AccessControl {

  sealed abstract case class ApprovedUpdate[A,B](SET: A, ids: List[B])

}

trait AccessControl[F[_]] extends Predicates[F] {

  def user: User
  def timeEstimateCalculator: TimeEstimateCalculatorImplementation.ForInstrumentMode
  def itcClient: ItcClient[F]
  def commitHash: CommitHash

  private def observationIdSelect(
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

  private def observationIdSelectWithWorkflowState(
    includeDeleted:      Option[Boolean],
    WHERE:               Option[Predicate],
    includeCalibrations: Boolean,
    allowedStates:       Set[ObservationWorkflowState]
  )(using Services[F], NoTransaction[F]): F[Result[List[Observation.Id]]] = 
    Services.asSuperUser:
      observationIdSelect(includeDeleted, WHERE, includeCalibrations)
        .flatTraverse: which =>
          observationWorkflowService.filterState(
            which, 
            allowedStates,
            commitHash,
            itcClient,
            timeEstimateCalculator
          )

  def selectForUpdate(input: UpdateObservationsInput)(using Services[F], NoTransaction[F]): F[Result[AccessControl.ApprovedUpdate[ObservationPropertiesInput.Edit, Observation.Id]]] =
    Services.asSuperUser:

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

      observationIdSelectWithWorkflowState(
        input.includeDeleted, 
        input.WHERE, 
        false, 
        allowedStates(input.SET)
      ).map(_.map(new AccessControl.ApprovedUpdate(input.SET, _) {}))
      
  def selectForUpdate(input: UpdateAsterismsInput)(using Services[F], NoTransaction[F]): F[Result[AccessControl.ApprovedUpdate[EditAsterismsPatchInput, Observation.Id]]] =
    Services.asSuperUser:
      observationIdSelectWithWorkflowState(
        input.includeDeleted, 
        input.WHERE, 
        false, 
        ObservationWorkflowState.preExecutionSet // not allowed once we start executing
      ).map(_.map(new AccessControl.ApprovedUpdate(input.SET, _) {}))

  // TODO
  def selectForUpdate(input: UpdateObservationsTimesInput): Result[AppliedFragment] = 
    observationIdSelect(input.includeDeleted, input.WHERE, true)


}
