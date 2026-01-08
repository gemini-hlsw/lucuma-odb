// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.Ephemeris
import lucuma.odb.graphql.binding.*
import lucuma.core.data.PerSite
import lucuma.core.enums.Site

object UserSuppliedEphemerisInput:

  val Binding: Matcher[PerSite[List[Ephemeris.UserSupplied.Element]]] =
    ObjectFieldsBinding.rmap:
      case List(
        UserSuppliedEphemerisElementInput.Binding.List("gn", rNorth),
        UserSuppliedEphemerisElementInput.Binding.List("gs", rSouth),
      ) =>
        (rNorth, rSouth).mapN: (gn, gs) =>
          PerSite.unfold:
            case Site.GN => gn
            case Site.GS => gs

