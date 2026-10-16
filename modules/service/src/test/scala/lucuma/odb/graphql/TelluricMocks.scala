// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import io.circe.Json
import io.circe.syntax.*
import lucuma.catalog.clients.SimbadClientMock
import lucuma.catalog.clients.TelluricTargetsClientMock
import lucuma.catalog.telluric.TelluricStar
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.enums.Band
import lucuma.core.enums.TelluricCalibrationOrder
import lucuma.core.math.BrightnessUnits.BrightnessMeasure
import lucuma.core.math.BrightnessUnits.Integrated
import lucuma.core.math.BrightnessValue
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.model.TelluricType
import lucuma.odb.TestCoordinates.coords
import lucuma.refined.*

import scala.collection.immutable.SortedMap

// The telluric client every suite gets unless it overrides it: a before and an
// after star, resolving to one Simbad target with a brightness.
object TelluricMocks:

  val starBefore = TelluricStar(
    id = "12345",
    spType = TelluricType.A0V,
    coordinates = coords(123.456, 45.678),
    distance = 100.5,
    hmag = 7.5,
    score = 0.95,
    order = TelluricCalibrationOrder.Before,
    sed = None
  )

  val starAfter = TelluricStar(
    id = "12346",
    spType = TelluricType.A0V,
    coordinates = coords(123.789, 45.123),
    distance = 98.2,
    hmag = 7.8,
    score = 0.92,
    order = TelluricCalibrationOrder.After,
    sed = None
  )

  val brightnesses = SortedMap[Band, BrightnessMeasure[Integrated]](
    Band.J -> Band.J.defaultUnits[Integrated].withValueTagged(BrightnessValue.unsafeFrom(14.74))
  )

  val target = Target.Sidereal(
    name = "HIP 12345".refined,
    tracking = SiderealTracking(
      baseCoordinates = starBefore.coordinates,
      epoch = lucuma.core.math.Epoch.J2000,
      properMotion = None,
      radialVelocity = None,
      parallax = None
    ),
    sourceProfile = SourceProfile.Point(
      SpectralDefinition.BandNormalized(None, brightnesses)
    ),
    catalogInfo = None
  )

  private def starJson(star: TelluricStar): Json =
    Json.obj(
      "ID" -> star.id.asJson,
      "spType" -> Json.fromString(star.spType.tag),
      "RA" -> star.coordinates.ra.toAngle.toDoubleDegrees.asJson,
      "Dec" -> star.coordinates.dec.toAngle.toSignedDoubleDegrees.asJson,
      "Distance" -> star.distance.asJson,
      "Hmag" -> star.hmag.asJson,
      "Score" -> star.score.asJson,
      "Order" -> Json.fromString(star.order.tag)
    )

  val searchJson: Json =
    Json.obj("data" -> Json.obj("search" -> Json.arr(starJson(starBefore), starJson(starAfter))))

  val client: IO[TelluricTargetsClient[IO]] =
    TelluricTargetsClientMock.fromJson(searchJson, SimbadClientMock.withSingleTarget(target))
