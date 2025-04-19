// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package predicate

import grackle.Path
import lucuma.core.model.ExecutionEvent

class ExecutionEventPredicates(path: Path) {
  lazy val id: LeafPredicates[ExecutionEvent.Id] =
    LeafPredicates[ExecutionEvent.Id](path / "id")

  lazy val observation = new ObservationPredicates(path / "observation")
}
