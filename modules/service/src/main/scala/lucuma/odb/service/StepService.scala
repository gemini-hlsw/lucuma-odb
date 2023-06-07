// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.applicative.*
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

trait StepService[F[_]] {

  def insertGmosNorth(
    visitId:    Visit.Id,
    instrument: GmosNorth,
    step:       StepConfig
  )(using Transaction[F]): F[StepService.InsertStepResponse]

  def insertGmosSouth(
    visitId:    Visit.Id,
    instrument: GmosSouth,
    step:       StepConfig
  )(using Transaction[F]): F[StepService.InsertStepResponse]

}

object StepService {

  sealed trait InsertStepResponse extends Product with Serializable

  object InsertStepResponse {

    case class NotAuthorized(
      user: User
    ) extends InsertStepResponse

    case class VisitNotFound(
      vid:        Visit.Id,
      instrument: Instrument
    ) extends InsertStepResponse

    case class Success(
      sid: Step.Id
    ) extends InsertStepResponse
  }

  def instantiate[F[_]: Concurrent](using Services[F]): StepService[F] =
    new StepService[F] with ExecutionUserCheck {
      override def insertGmosNorth(
        visitId:    Visit.Id,
        instrument: GmosNorth,
        step:       StepConfig
      )(using Transaction[F]): F[StepService.InsertStepResponse] =
        InsertStepResponse.NotAuthorized(user).pure[F]

      override def insertGmosSouth(
        visitId:    Visit.Id,
        instrument: GmosSouth,
        step:       StepConfig
      )(using Transaction[F]): F[StepService.InsertStepResponse] =
        InsertStepResponse.NotAuthorized(user).pure[F]

    }

}
