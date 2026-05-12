// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.odb.graphql.query.ExecutionTestSupport

trait TelluricCalibrationsTestSupport:
  self: ExecutionTestSupport =>

  case class ObsInfo(id: Observation.Id, groupId: Option[Group.Id], groupIndex: Option[Int], calibrationRole: Option[CalibrationRole]) derives Decoder

  protected def setExposureTime(oid: Observation.Id, totalMinutes: Int): IO[Unit] =
    val perExposureMinutes = totalMinutes / 6
    query(
      pi,
      s"""mutation {
        updateObservations(input: {
          WHERE: { id: { EQ: "$oid" } }
          SET: {
            observingMode: {
              flamingos2LongSlit: {
                exposureTimeMode: {
                  timeAndCount: {
                    time: { minutes: $perExposureMinutes },
                    count: 6,
                    at: { nanometers: 1390 }
                  }
                }
              }
            }
          }
        }) {
          observations { id }
        }
      }"""
    ).void

  protected def queryObservation(oid: Observation.Id): IO[ObsInfo] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              id
              groupId
              groupIndex
              calibrationRole
            }
          }"""
    ).flatMap { c =>
      c.hcursor.downField("observation").as[ObsInfo]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  protected def queryObservationExists(oid: Observation.Id): IO[Boolean] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              id
            }
          }"""
    ).map(_.hcursor.downField("observation").as[ObsInfo].isRight)

  protected def queryObservationsInGroup(gid: Group.Id): IO[List[ObsInfo]] =
    query(
      serviceUser,
      s"""query {
            group(groupId: "$gid") {
              elements {
                observation {
                  id
                  groupId
                  groupIndex
                  calibrationRole
                }
              }
            }
          }"""
    ).map: c =>
      c.hcursor
        .downField("group")
        .downField("elements")
        .values
        .toList
        .flatten
        .flatMap(_.hcursor.downField("observation").as[Option[ObsInfo]].toOption)
        .flatten

  protected def queryGroupExists(gid: Group.Id): IO[Boolean] =
    query(
      serviceUser,
      s"""query {
            group(groupId: "$gid") {
              id
            }
          }"""
    ).map(_.hcursor.downField("group").focus.exists(!_.isNull))
