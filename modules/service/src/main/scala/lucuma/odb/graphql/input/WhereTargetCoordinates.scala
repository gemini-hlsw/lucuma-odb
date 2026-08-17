// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Result
import lucuma.odb.data.Cone
import lucuma.odb.graphql.ConeFilter
import lucuma.odb.graphql.binding.*

/** The `WhereCone` (`targetCoordinates`) binding shared by the WHERE inputs
 *  that support cone searches.
 *
 *  The cone's candidate lookup is an effect the elaborator cannot run, so this
 *  yields a placeholder that `ConeFilter.resolve` swaps for `id IN (…)` before
 *  execution. Parsing it as an ordinary binding means variables are already
 *  substituted here. `entity` selects the candidate lookup.
 */
object WhereTargetCoordinates:

  def binding(
    idPath:    Path,
    entity:    ConeFilter.ConeEntity,
    queryName: String,
    allowCone: Boolean
  ): Matcher[Predicate] =
    if !allowCone then
      _ => Left(s"`targetCoordinates` is only supported when querying $queryName.")
    else
      ObjectFieldsBinding.rmap:
        case List(
          CoordinatesInput.Create.Binding("center", rCenter),
          AngleInput.Binding("distance", rDistance)
        ) => (rCenter, rDistance).parTupled.flatMap: (c, d) =>
          Cone.from(c, d) match
            case Some(cone) => Result(ConeFilter.ConePredicate(idPath, cone, entity))
            case None       => Matcher.validationFailure("The `distance` must be an angle between 0° and 180°.")
