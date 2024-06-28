// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.apply.*
import cats.syntax.parallel.*

import lucuma.core.math.Declination
import lucuma.core.math.Place
import lucuma.core.math.RightAscension
import lucuma.core.util.TimestampInterval
import lucuma.odb.graphql.binding.*

object CoordinateLimitsInput {

  def defaultRaLimits(place: Place, time: TimestampInterval): (RightAscension, RightAscension) = {
    (RightAscension.Zero, RightAscension.Zero)
  }

  def defaultDecLimits(place: Place): (Declination, Declination) = {
    (Declination.Zero, Declination.Zero)
  }

  case class Create(
    raStart:  RightAscension,
    raEnd:    RightAscension,
    decStart: Declination,
    decEnd:   Declination
  )

  object Create:
    val Binding: Matcher[(Place, TimestampInterval) => Create] =
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
            (place: Place, time: TimestampInterval) => {
              val defaultRa  = (raStart, raEnd).tupled.getOrElse(defaultRaLimits(place, time))
              val defaultDec = (decStart, decEnd).tupled.getOrElse(defaultDecLimits(place))

              Create(
                raStart.getOrElse(defaultRa._1),
                raEnd.getOrElse(defaultRa._2),
                decStart.getOrElse(defaultDec._1),
                decEnd.getOrElse(defaultDec._2)
              )
            }
          }
      }

    def default(place: Place, time: TimestampInterval): Create =
      val ra  = defaultRaLimits(place, time)
      val dec = defaultDecLimits(place)
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