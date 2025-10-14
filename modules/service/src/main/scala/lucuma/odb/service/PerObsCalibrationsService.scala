// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.Config
import lucuma.odb.service.CalibrationsService.ObsExtract
import lucuma.odb.service.Services.ServiceAccess
import org.http4s.client.Client
import skunk.Transaction

trait PerObsCalibrationsService[F[_]]:
  def generatePerObsCalibrations(
    pid: Program.Id,
    f2ScienceObs: List[ObsExtract[CalibrationConfigSubset]]
  )(using Transaction[F], ServiceAccess): F[List[Observation.Id]]

object PerObsCalibrationsService:
  def instantiate[F[_]: MonadCancelThrow](emailConfig: Config.Email, httpClient: Client[F])(using Services[F]): PerObsCalibrationsService[F] =
    new PerObsCalibrationsService[F] with CalibrationObservations:
      override def generatePerObsCalibrations(
        pid: Program.Id,
        f2ScienceObs: List[ObsExtract[CalibrationConfigSubset]]
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] =
        List.empty.pure[F]
