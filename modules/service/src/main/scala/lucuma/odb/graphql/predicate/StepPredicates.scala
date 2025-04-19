// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package predicate

import grackle.Path
import lucuma.core.model.sequence.Step

class StepPredicates(path: Path) {
  lazy val id: LeafPredicates[Step.Id] =
    LeafPredicates[Step.Id](path / "id")
}
