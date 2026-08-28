// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.catalog.telluric.TelluricStar
import lucuma.core.enums.Site
import lucuma.core.enums.TelluricCalibrationOrder
import lucuma.core.enums.TwilightType
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension
import lucuma.core.model.ObservingNight
import lucuma.core.model.TelluricType
import lucuma.core.util.Timestamp
import munit.FunSuite

import java.time.Instant

class SelectSingleByLstRuleSuite extends FunSuite:

  private val site: Site           = Site.GS
  private val obsInstant: Instant  = Instant.parse("2026-04-23T02:00:00Z")
  private val obsTime: Timestamp   = Timestamp.unsafeFromInstantTruncated(obsInstant)
  private val dec: Declination     = Declination.fromDoubleDegrees(-30.0).get

  // Compute the expected twilight LSTs at the site for the chosen instant.
  private val night       = ObservingNight.fromSiteAndInstant(site, obsInstant)
  private val tbn         = night.twilightBoundedUnsafe(TwilightType.Nautical)
  private val lstEvening  = TelluricTargetsService.lstHoursAt(site, tbn.start)
  private val lstMorning  = TelluricTargetsService.lstHoursAt(site, tbn.end)

  // Mid-LST of the observable night, handling wrap-around.
  private val nightSpan   = ((lstMorning - lstEvening) % 24 + 24) % 24
  private val midLst      = ((lstEvening + nightSpan / 2.0) % 24 + 24) % 24

  private def raFromHours(h: Double): RightAscension =
    val normalized = ((h % 24) + 24) % 24
    RightAscension.fromHourAngle.get(HourAngle.fromDoubleHours(normalized))

  private def coordsAtRa(raHours: Double): Coordinates =
    Coordinates(raFromHours(raHours), dec)

  private def star(order: TelluricCalibrationOrder, score: Double, id: String): TelluricStar =
    TelluricStar(
      id          = id,
      spType      = TelluricType.Hot,
      coordinates = Coordinates(raFromHours(0.0), dec),
      distance    = 1.0,
      hmag        = 7.0,
      score       = score,
      order       = order,
      sed         = None
    )

  private val beforeStar = star(TelluricCalibrationOrder.Before, 0.5, "1")
  private val afterStar  = star(TelluricCalibrationOrder.After,  0.9, "2")

  private val pair: List[(TelluricStar, Unit)] =
    List((beforeStar, ()), (afterStar, ()))

  test("only Before present returns Before"):
    val r = TelluricTargetsService.selectSingleByLstRule[Unit](
      List((beforeStar, ())), coordsAtRa(10.0), site, Some(obsTime)
    )
    assertEquals(r.map(_._1.id), Some(beforeStar.id))

  test("only After present returns After"):
    val r = TelluricTargetsService.selectSingleByLstRule[Unit](
      List((afterStar, ())), coordsAtRa(10.0), site, Some(obsTime)
    )
    assertEquals(r.map(_._1.id), Some(afterStar.id))

  test("both present but no obsTime returns None (falls back upstream)"):
    val r = TelluricTargetsService.selectSingleByLstRule[Unit](
      pair, coordsAtRa(10.0), site, None
    )
    assertEquals(r, None)

  test("middle RA picks the lowest-score candidate"):
    // Midway between evening and morning LST — safely > 0.75 h from each.
    val midRa = midLst
    val r = TelluricTargetsService.selectSingleByLstRule[Unit](
      pair, coordsAtRa(midRa), site, Some(obsTime)
    )
    // beforeStar has score 0.5 (lower), afterStar has score 0.9
    assertEquals(r.map(_._1.id), Some(beforeStar.id))

  test("RA close to evening twilight picks the After candidate"):
    // Within 0.75 h of the evening twilight LST.
    val raNearEvening = lstEvening + 0.25
    val r = TelluricTargetsService.selectSingleByLstRule[Unit](
      pair, coordsAtRa(raNearEvening), site, Some(obsTime)
    )
    assertEquals(r.map(_._1.id), Some(afterStar.id))

  test("RA close to morning twilight picks the Before candidate"):
    // Within 0.75 h of the morning twilight LST.
    val raNearMorning = lstMorning - 0.25
    val r = TelluricTargetsService.selectSingleByLstRule[Unit](
      pair, coordsAtRa(raNearMorning), site, Some(obsTime)
    )
    assertEquals(r.map(_._1.id), Some(beforeStar.id))

  test("middle RA picks After when After has lower score"):
    val betterAfter = afterStar.copy(score = 0.1)
    val pair2       = List((beforeStar, ()), (betterAfter, ()))
    val midRa       = midLst
    val r = TelluricTargetsService.selectSingleByLstRule[Unit](
      pair2, coordsAtRa(midRa), site, Some(obsTime)
    )
    assertEquals(r.map(_._1.id), Some(betterAfter.id))
