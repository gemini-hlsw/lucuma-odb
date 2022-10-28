// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Sync
import cats.syntax.functorFilter.*
import cats.syntax.traverse.*
import edu.gemini.grackle.Result
import lucuma.core.model.Observation
import lucuma.odb.data.ObservingModeType
import lucuma.odb.graphql.input.ObservingModeInput
import skunk.Session
import skunk.Transaction

sealed trait ObservingModeServices[F[_]] {

  def createFunction(
    input: ObservingModeInput.Create
  ): Result[(List[Observation.Id], Transaction[F]) => F[Unit]]

  def deleteFunction(
    mode: ObservingModeType
  ): (List[Observation.Id], Transaction[F]) => F[Unit]

  def updateFunction(
    input: ObservingModeInput.Edit
  ): Result[(List[Observation.Id], Transaction[F]) => F[Unit]]

  def createViaUpdateFunction(
    input: ObservingModeInput.Edit
  ): Result[(List[Observation.Id], Transaction[F]) => F[Unit]]

}

object ObservingModeServices {

  def fromSession[F[_]: Sync](
    session: Session[F]
  ): ObservingModeServices[F] =
    new ObservingModeServices[F] {

      lazy val gmosLongSlitService: GmosLongSlitService[F] =
        GmosLongSlitService.fromSession(session)

      override def createFunction(
        input: ObservingModeInput.Create
      ): Result[(List[Observation.Id], Transaction[F]) => F[Unit]] =
        List(
          input.gmosNorthLongSlit.map(gmosLongSlitService.insertNorth),
          input.gmosSouthLongSlit.map(gmosLongSlitService.insertSouth)
        ).flattenOption match {
          case List(f) => Result(f)
          case Nil     => Result.failure("No observing mode creation parameters were provided.")
          case _       => Result.failure("Only one observing mode's creation parameters may be provided.")
        }

      override def deleteFunction(
        mode: ObservingModeType
      ): (List[Observation.Id], Transaction[F]) => F[Unit] =
        mode match {
          case ObservingModeType.GmosNorthLongSlit => gmosLongSlitService.deleteNorth
          case ObservingModeType.GmosSouthLongSlit => gmosLongSlitService.deleteSouth
        }

      override def updateFunction(
        input: ObservingModeInput.Edit
      ): Result[(List[Observation.Id], Transaction[F]) => F[Unit]] =
        List(
          input.gmosNorthLongSlit.map(gmosLongSlitService.updateNorth),
          input.gmosSouthLongSlit.map(gmosLongSlitService.updateSouth)
        ).flattenOption match {
          case List(f) => Result(f)
          case Nil     => Result.failure("No observing mode edit parameters were provided.")
          case _       => Result.failure("Only one observing mode's edit parameters may be provided.")
        }

      override def createViaUpdateFunction(
        input: ObservingModeInput.Edit
      ): Result[(List[Observation.Id], Transaction[F]) => F[Unit]] =
        List(
          input.gmosNorthLongSlit.map(m => m.toCreate.map(gmosLongSlitService.insertNorth)),
          input.gmosSouthLongSlit.map(m => m.toCreate.map(gmosLongSlitService.insertSouth))
        ).flattenOption match {
          case List(f) => f
          case Nil     => Result.failure("No observing mode edit parameters were provided.")
          case _       => Result.failure("Only one observing mode's edit parameters may be provided.")
        }
    }

}
