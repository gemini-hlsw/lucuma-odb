// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.Site
import lucuma.core.util.DateInterval
import lucuma.odb.graphql.binding.*

import java.time.Duration

object SiteCoordinateLimitsInput {

  case class Create(
    north: CoordinateLimitsInput.Create,
    south: CoordinateLimitsInput.Create
  )

  object Create:
    val Binding: Matcher[(DateInterval, Duration) => Create] =
      ObjectFieldsBinding.rmap {
        case List(
          CoordinateLimitsInput.Create.Binding.Option("north", rNorth),
          CoordinateLimitsInput.Create.Binding.Option("south", rSouth)
        ) => (rNorth, rSouth).parMapN { (north, south) =>
          (date: DateInterval, buffer: Duration) =>
            val n = north.getOrElse(CoordinateLimitsInput.Create.default)
            val s = south.getOrElse(CoordinateLimitsInput.Create.default)
            Create(n(Site.GN, date, buffer), s(Site.GS, date, buffer))
        }
      }

    def default(date: DateInterval, buffer: Duration): Create =
      val north = CoordinateLimitsInput.Create.default(Site.GN, date, buffer)
      val south = CoordinateLimitsInput.Create.default(Site.GS, date, buffer)
      Create(north, south)

  case class Edit(
    north: Option[CoordinateLimitsInput.Edit],
    south: Option[CoordinateLimitsInput.Edit]
  )

  object Edit:
    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          CoordinateLimitsInput.Edit.Binding.Option("north", rNorth),
          CoordinateLimitsInput.Edit.Binding.Option("south", rSouth),
        ) => (rNorth, rSouth).parMapN(Edit.apply)
      }
}

