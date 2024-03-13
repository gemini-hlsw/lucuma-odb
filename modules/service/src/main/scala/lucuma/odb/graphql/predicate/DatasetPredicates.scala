// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package predicate

import grackle.Path
import lucuma.core.model.sequence.Dataset
import lucuma.odb.data.DatasetReference

class DatasetPredicates(path: Path) {
  lazy val id             = LeafPredicates[Dataset.Id](path / "id")
  lazy val observation    = new ObservationPredicates(path / "observation")
  lazy val referenceLabel = LeafPredicates[DatasetReference](path / "reference" / "label")
}
