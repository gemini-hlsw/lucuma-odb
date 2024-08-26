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
import lucuma.core.util.DateInterval
import lucuma.odb.graphql.binding.*

import java.time.Duration
import java.time.Instant
import java.time.LocalDate
import java.time.LocalTime
import java.time.ZoneOffset.UTC

object CoordinateLimitsInput {

  // From Andy: A simple suggestion would be to set the RA limits to half an
  // hour after the LST at evening twilight on the first night of the semester
  // and half an hour before the LST at morning twilight on the last night of
  // the semester.
  private val Buffer = Duration.ofMinutes(30L)

  private def toRa(t: LocalTime): RightAscension =
    val h = t.getHour
    val m = t.getMinute
    val s = t.getSecond
    val n = t.getNano

    // Round to nearest half-hour and convert to RA
    RightAscension.fromHourAngle.get(HourAngle.fromDoubleHours(
      Math.round((h + m / 60.0 + (s + n/1000000000.0) / 3600.0) * 2.0) / 2.0
    ))

  def defaultRaLimits(site: Site, dateInterval: DateInterval): (RightAscension, RightAscension) =

    if (dateInterval.isEmpty) (RightAscension.Zero, RightAscension.Zero)
    else {
      def nightAt(date: LocalDate): ObservingNight =
        ObservingNight.fromSiteAndLocalDate(site, date)

      // Twilight on the evening of the first night
      val start    = nightAt(dateInterval.start).twilightBoundedUnsafe(Nautical).start

      // Twilight in the morning of the last night
      val end      = nightAt(dateInterval.end).previous.twilightBoundedUnsafe(Nautical).end

      // LST at start and end
      val sc       = ImprovedSkyCalc(site.place)
      def lstAt(instant: Instant): LocalTime =
        sc.getLst(instant).withZoneSameInstant(UTC).toLocalTime

      val lstStart = lstAt(start.plus(Buffer))
      val lstEnd   = lstAt(end.minus(Buffer))

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
    val Binding: Matcher[(Site, DateInterval) => Create] =
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
            (site: Site, date: DateInterval) => {
              val defaultRa  = (raStart, raEnd).tupled.getOrElse(defaultRaLimits(site, date))
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

    def default(site: Site, date: DateInterval): Create =
      val ra  = defaultRaLimits(site, date)
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