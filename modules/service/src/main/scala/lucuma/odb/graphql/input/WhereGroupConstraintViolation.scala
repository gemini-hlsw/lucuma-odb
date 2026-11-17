// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.*

/** Matches a `GroupConstraintViolation`: its code and where it happened. */
object WhereGroupConstraintViolation:

  def binding(path: Path): Matcher[Predicate] =
    val CodeBinding    = WhereEq.binding(path / "code", GroupConstraintViolationCodeBinding)
    val ElementBinding = WhereGroupElementId.binding(path / "element")
    val NightBinding   = WhereObservingNight.binding(path / "night")
    ObjectFieldsBinding.rmap:
      case List(
        CodeBinding.Option("code", rCode),
        ElementBinding.Option("element", rElement),
        NightBinding.Option("night", rNight)
      ) =>
        (rCode, rElement, rNight).parMapN((code, element, night) => and(List(code, element, night).flatten))
