// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import lucuma.core.model.Visit

class VisitPredicates(path: Path) {
  lazy val id          = LeafPredicates[Visit.Id](path / "id")
  lazy val observation = new ObservationPredicates(path / "observation")
}
