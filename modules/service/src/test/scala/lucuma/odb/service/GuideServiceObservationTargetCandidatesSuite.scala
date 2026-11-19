// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.ags.GuideStarCandidate
import lucuma.ags.GuideStarId
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.Integrated
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.math.Region
import lucuma.core.model.Ephemeris
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import munit.FunSuite

import scala.collection.immutable.SortedMap

object ObservationTargetFixtures:

  /** A sidereal target at the origin whose only brightness is the given R magnitude. */
  def siderealWithR(name: String, rMagnitude: Int): Target.Sidereal =
    Target.Sidereal(
      name          = NonEmptyString.unsafeFrom(name),
      tracking      = SiderealTracking(Coordinates.Zero, Epoch.J2000, none, none, none),
      sourceProfile = SourceProfile.Point(
        SpectralDefinition.BandNormalized(
          none,
          SortedMap(
            Band.R -> Band.R.defaultUnits[Integrated].withValueTagged(BrightnessValue.unsafeFrom(BigDecimal(rMagnitude)))
          )
        )
      ),
      catalogInfo   = none
    )

class GuideServiceObservationTargetCandidatesSuite extends FunSuite:
  import ObservationTargetFixtures.*

  private val VegaId: Target.Id   = Target.Id.fromLong(1L).get
  private val AltairId: Target.Id = Target.Id.fromLong(2L).get
  private val CometId: Target.Id  = Target.Id.fromLong(3L).get
  private val TooId: Target.Id    = Target.Id.fromLong(4L).get

  private val Vega: Target.Sidereal   = siderealWithR("Vega", 11)
  private val Altair: Target.Sidereal = siderealWithR("Altair", 12)

  private val Comet: Target.Nonsidereal =
    Target.Nonsidereal(
      NonEmptyString.unsafeFrom("Comet"),
      Ephemeris.Key.UserSupplied(1L),
      Vega.sourceProfile
    )

  private val Too: Target.Opportunity =
    Target.Opportunity(NonEmptyString.unsafeFrom("ToO"), Region.Full, Vega.sourceProfile)

  private val CometCoordinates: Coordinates =
    Coordinates.fromHmsDms.getOption("10:00:00 +20:00:00").get

  private val NoCoordinates: Target.Id => Option[Coordinates] =
    _ => none

  test("sidereal targets are kept, as observation target candidates with their ids"):
    val result: List[(Target, GuideStarCandidate)] =
      GuideService.observationTargetCandidates(List(VegaId -> Vega, AltairId -> Altair), NoCoordinates)
    assertEquals(result.map(_._1), List(Vega, Altair))
    assertEquals(
      result.map(_._2.id),
      List(GuideStarId.ObservationTarget(VegaId.some), GuideStarId.ObservationTarget(AltairId.some))
    )
    assertEquals(
      result.map(_._2),
      List(
        GuideStarCandidate.observationTarget(VegaId, Vega.tracking, Vega.sourceProfile),
        GuideStarCandidate.observationTarget(AltairId, Altair.tracking, Altair.sourceProfile)
      )
    )

  test("a non-sidereal target with a position is kept, fixed at it"):
    val coordinatesOf: Target.Id => Option[Coordinates] =
      id => Option.when(id === CometId)(CometCoordinates)
    val result: List[(Target, GuideStarCandidate)] =
      GuideService.observationTargetCandidates(List(CometId -> Comet, VegaId -> Vega), coordinatesOf)
    assertEquals(result.map(_._1), List(Comet, Vega))
    assertEquals(
      result.headOption.map(_._2),
      GuideStarCandidate.observationTarget(CometId, SiderealTracking.const(CometCoordinates), Comet.sourceProfile).some
    )

  test("a non-sidereal target without a position is dropped"):
    val result: List[(Target, GuideStarCandidate)] =
      GuideService.observationTargetCandidates(List(CometId -> Comet, VegaId -> Vega), NoCoordinates)
    assertEquals(result.map(_._1), List(Vega))
    assertEquals(result.map(_._2.id), List(GuideStarId.ObservationTarget(VegaId.some)))

  test("an opportunity target is dropped"):
    val result: List[(Target, GuideStarCandidate)] =
      GuideService.observationTargetCandidates(List(TooId -> Too), _ => CometCoordinates.some)
    assertEquals(result, Nil)
