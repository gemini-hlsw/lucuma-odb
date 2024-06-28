// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.apply.*
import cats.syntax.parallel.*

import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType.Nautical
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension
import lucuma.core.math.skycalc.ImprovedSkyCalc
import lucuma.core.model.ObservingNight
import lucuma.core.util.TimestampInterval
import lucuma.odb.graphql.binding.*

import java.time.Duration
import java.time.Instant
import java.time.ZonedDateTime

object CoordinateLimitsInput {

  // From Andy: A simple suggestion would be to set the RA limits to half an
  // hour after the LST at evening twilight on the first night of the semester
  // and half an hour before the LST at morning twilight on the last night of
  // the semester.
  private val Buffer = Duration.ofMinutes(30L)

  def defaultRaLimits(site: Site, time: TimestampInterval): (RightAscension, RightAscension) = {

    def nightAt(instant: Instant): ObservingNight =
      ObservingNight.fromSiteAndInstant(site, instant)

    // Twilight on the evening of the first night
    val start    = nightAt(time.start.toInstant).twilightBoundedUnsafe(Nautical).start

    // Twilight in the morning of the last night
    val end      = nightAt(time.end.toInstant).previous.twilightBoundedUnsafe(Nautical).end

    // LST at start and end
    val sc       = ImprovedSkyCalc(site.place)
    val lstStart = sc.getLst(start.plus(Buffer))
    val lstEnd   = sc.getLst(end.minus(Buffer))

    def toRa(z: ZonedDateTime): RightAscension =
      val h = z.getHour
      val m = z.getMinute
      val s = z.getSecond
      val n = z.getNano

      // Round to nearest half-hour and convert to RA
      RightAscension.fromHourAngle.get(HourAngle.fromDoubleHours(
        Math.round((h + m / 60.0 + (s + n/1000000000.0) / 3600.0) * 2.0) / 2.0
      ))

    (toRa(lstStart), toRa(lstEnd))
  }

  private val NorthLowerLimit = Declination.fromStringSignedDMS.unsafeGet("-37:00:00.000000")
  private val SouthUpperLimit = Declination.fromStringSignedDMS.unsafeGet("+28:00:00.000000")

  def defaultDecLimits(site: Site): (Declination, Declination) =
    site match {
      case Site.GN => (NorthLowerLimit, Declination.Max)
      case Site.GS => (Declination.Min, SouthUpperLimit)
    }

  case class Create(
    raStart:  RightAscension,
    raEnd:    RightAscension,
    decStart: Declination,
    decEnd:   Declination
  )

  object Create:
    val Binding: Matcher[(Site, TimestampInterval) => Create] =
      ObjectFieldsBinding.rmap {
        case List(
          RightAscensionInput.Binding.Option("raStart", rRaStart),
          RightAscensionInput.Binding.Option("raEnd",   rRaEnd),
          DeclinationInput.Binding.Option("decStart",   rDecStart),
          DeclinationInput.Binding.Option("decEnd",     rDecEnd)
        ) =>
          (
            rRaStart,
            rRaEnd,
            rDecStart,
            rDecEnd
          ).parMapN { (raStart, raEnd, decStart, decEnd) =>
            (site: Site, time: TimestampInterval) => {
              val defaultRa  = (raStart, raEnd).tupled.getOrElse(defaultRaLimits(site, time))
              val defaultDec = (decStart, decEnd).tupled.getOrElse(defaultDecLimits(site))

              Create(
                raStart.getOrElse(defaultRa._1),
                raEnd.getOrElse(defaultRa._2),
                decStart.getOrElse(defaultDec._1),
                decEnd.getOrElse(defaultDec._2)
              )
            }
          }
      }

    def default(site: Site, time: TimestampInterval): Create =
      val ra  = defaultRaLimits(site, time)
      val dec = defaultDecLimits(site)
      Create(ra._1, ra._2, dec._1, dec._2)

  case class Edit(
    raStart:  Option[RightAscension],
    raEnd:    Option[RightAscension],
    decStart: Option[Declination],
    decEnd:   Option[Declination]
  )

  object Edit:
    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          RightAscensionInput.Binding.Option("raStart", rRaStart),
          RightAscensionInput.Binding.Option("raEnd",   rRaEnd),
          DeclinationInput.Binding.Option("decStart",   rDecStart),
          DeclinationInput.Binding.Option("decEnd",     rDecEnd)
        ) =>
          (
            rRaStart,
            rRaEnd,
            rDecStart,
            rDecEnd
          ).parMapN(Edit.apply)

      }
}