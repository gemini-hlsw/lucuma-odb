// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.LocalObservingNight
import lucuma.core.model.ObservingNight
import lucuma.odb.graphql.binding.*

/** An observing night at a site, identified by the local date on which it ends. */
object ObservingNightInput:

  val Binding: Matcher[ObservingNight] =
    ObjectFieldsBinding.rmap:
      case List(
        SiteBinding("site", rSite),
        DateBinding("date", rDate)
      ) =>
        (rSite, rDate).parMapN((s, d) => ObservingNight(s, LocalObservingNight(d)))
