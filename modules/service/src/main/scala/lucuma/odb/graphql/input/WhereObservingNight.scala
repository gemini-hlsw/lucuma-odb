// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.*
import org.typelevel.cats.time.given

import java.time.LocalDate

/** Matches an `ObservingNight`. */
object WhereObservingNight:

  def binding(path: Path): Matcher[Predicate] =
    val SiteBinding_ = WhereEq.binding(path / "site", SiteBinding)
    val DateBinding_ = WhereOrder.binding[LocalDate](path / "date", DateBinding)
    ObjectFieldsBinding.rmap:
      case List(
        SiteBinding_.Option("site", rSite),
        DateBinding_.Option("date", rDate)
      ) =>
        (rSite, rDate).parMapN((site, date) => and(List(site, date).flatten))
