// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package predicate

import edu.gemini.grackle.Path

class DatasetPredicates(path: Path) {
  lazy val id = new DatasetIdPredicates(path / "id")
}
