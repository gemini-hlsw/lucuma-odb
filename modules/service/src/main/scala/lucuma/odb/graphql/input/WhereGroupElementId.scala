// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.odb.graphql.binding.*

/**
 * Matches a `GroupElementId`.  The ids are compared through the hidden
 * `groupIdValue` and `observationIdValue` fields, whose codecs aren't
 * optional, so that a plain value can be compared; an id the element doesn't
 * have is null, and matches nothing.
 */
object WhereGroupElementId:

  def binding(path: Path): Matcher[Predicate] =
    val GroupIdBinding_       = WhereOrder.binding[Group.Id](path / "groupIdValue", GroupIdBinding)
    val ObservationIdBinding_ = WhereOrder.binding[Observation.Id](path / "observationIdValue", ObservationIdBinding)
    ObjectFieldsBinding.rmap:
      case List(
        GroupIdBinding_.Option("groupId", rGroupId),
        ObservationIdBinding_.Option("observationId", rObservationId)
      ) =>
        (rGroupId, rObservationId).parMapN((g, o) => and(List(g, o).flatten))
