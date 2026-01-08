// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.Ephemeris
import lucuma.odb.graphql.binding.*

object UserSuppliedEphemerisElementInput {

  val Binding: Matcher[Ephemeris.UserSupplied.Element] =
    ObjectFieldsBinding.rmap {
      case List(
        TimestampBinding("when", rWhen),
        CoordinatesInput.Create.Binding("coordinates", rCoords),
        OffsetInput.Binding("offset", rOffset),
      ) =>
        (rWhen, rCoords, rOffset).mapN: (w, c, o) =>
          Ephemeris.UserSupplied.Element(w.toInstant, c, o)

    }
}

