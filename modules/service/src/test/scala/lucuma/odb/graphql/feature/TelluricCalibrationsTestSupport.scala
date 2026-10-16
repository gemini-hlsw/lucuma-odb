// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.catalog.clients.SimbadClientMock
import lucuma.catalog.clients.TelluricTargetsClientMock
import lucuma.catalog.telluric.TelluricStar
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.enums.Band
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TelluricCalibrationOrder
import lucuma.core.math.BrightnessUnits.BrightnessMeasure
import lucuma.core.math.BrightnessUnits.Integrated
import lucuma.core.math.BrightnessValue
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.model.TelluricType
import lucuma.odb.TestCoordinates.coords
import lucuma.odb.graphql.query.ExecutionTestSupport
import lucuma.refined.*

import scala.collection.immutable.SortedMap

trait TelluricCalibrationsTestSupport extends ExecutionTestSupport:

  case class ObsInfo(id: Observation.Id, groupId: Option[Group.Id], groupIndex: Option[Int], calibrationRole: Option[CalibrationRole]) derives Decoder

  val mockStarBefore = TelluricStar(
    id = "12345",
    spType = TelluricType.A0V,
    coordinates = coords(123.456, 45.678),
    distance = 100.5,
    hmag = 7.5,
    score = 0.95,
    order = TelluricCalibrationOrder.Before,
    sed = None
  )

  val mockStarAfter = TelluricStar(
    id = "12346",
    spType = TelluricType.A0V,
    coordinates = coords(123.789, 45.123),
    distance = 98.2,
    hmag = 7.8,
    score = 0.92,
    order = TelluricCalibrationOrder.After,
    sed = None
  )

  val mockBrightnesses = SortedMap[Band, BrightnessMeasure[Integrated]](
    Band.J -> Band.J.defaultUnits[Integrated].withValueTagged(BrightnessValue.unsafeFrom(14.74))
  )

  val mockTarget = Target.Sidereal(
    name = "HIP 12345".refined,
    tracking = SiderealTracking(
      baseCoordinates = mockStarBefore.coordinates,
      epoch = lucuma.core.math.Epoch.J2000,
      properMotion = None,
      radialVelocity = None,
      parallax = None
    ),
    sourceProfile = SourceProfile.Point(
      SpectralDefinition.BandNormalized(None, mockBrightnesses)
    ),
    catalogInfo = None
  )

  val mockTelluricJson: Json = Json.obj(
    "data" -> Json.obj(
      "search" -> Json.arr(
        Json.obj(
          "ID" -> mockStarBefore.id.asJson,
          "spType" -> Json.fromString(mockStarBefore.spType.tag),
          "RA" -> mockStarBefore.coordinates.ra.toAngle.toDoubleDegrees.asJson,
          "Dec" -> mockStarBefore.coordinates.dec.toAngle.toSignedDoubleDegrees.asJson,
          "Distance" -> mockStarBefore.distance.asJson,
          "Hmag" -> mockStarBefore.hmag.asJson,
          "Score" -> mockStarBefore.score.asJson,
          "Order" -> Json.fromString(mockStarBefore.order.tag)
        ),
        Json.obj(
          "ID" -> mockStarAfter.id.asJson,
          "spType" -> Json.fromString(mockStarAfter.spType.tag),
          "RA" -> mockStarAfter.coordinates.ra.toAngle.toDoubleDegrees.asJson,
          "Dec" -> mockStarAfter.coordinates.dec.toAngle.toSignedDoubleDegrees.asJson,
          "Distance" -> mockStarAfter.distance.asJson,
          "Hmag" -> mockStarAfter.hmag.asJson,
          "Score" -> mockStarAfter.score.asJson,
          "Order" -> Json.fromString(mockStarAfter.order.tag)
        )
      )
    )
  )

  override protected def telluricClient: IO[TelluricTargetsClient[IO]] =
    TelluricTargetsClientMock.fromJson(mockTelluricJson, SimbadClientMock.withSingleTarget(mockTarget))

  protected def setExposureTime(oid: Observation.Id, totalMinutes: Int, count: Int = 6): IO[Unit] =
    val perExposureMinutes = totalMinutes / count
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
                    count: $count,
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

  protected def queryScienceBand(oid: Observation.Id): IO[Option[ScienceBand]] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              scienceBand
            }
          }"""
    ).flatMap: c =>
      c.hcursor.downFields("observation", "scienceBand").as[Option[ScienceBand]]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

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
