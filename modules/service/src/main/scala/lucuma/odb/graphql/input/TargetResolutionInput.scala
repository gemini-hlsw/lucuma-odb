// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import grackle.Result
import lucuma.odb.graphql.binding.*

/**
 * What a Target of Opportunity turned out to be. Mirrors the one-of shape the target subtypes
 * already use, one level down: exactly one of sidereal / nonsidereal.
 */
object TargetResolutionInput {

  type Create = SiderealInput.Create | NonsiderealInput.Create
  type Edit   = SiderealInput.Edit | NonsiderealInput.Edit

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap:
      case List(
        SiderealInput.CreateBinding.Option("sidereal", rSidereal),
        NonsiderealInput.CreateBinding.Option("nonsidereal", rNonsidereal)
      ) => (rSidereal, rNonsidereal).parTupled.flatMap:
        case (Some(s), None) => Result(s)
        case (None, Some(n)) => Result(n)
        case _               => Matcher.validationFailure("Exactly one of sidereal, nonsidereal must be specified as a resolution.")

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap:
      case List(
        SiderealInput.EditBinding.Option("sidereal", rSidereal),
        NonsiderealInput.EditBinding.Option("nonsidereal", rNonsidereal)
      ) => (rSidereal, rNonsidereal).parTupled.flatMap:
        case (Some(s), None) => Result(s)
        case (None, Some(n)) => Result(n)
        case _               => Matcher.validationFailure("Exactly one of sidereal, nonsidereal must be specified as a resolution.")

}
