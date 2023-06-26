// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import edu.gemini.grackle.Path

class AddConditionsEntryResultPredicates(path: Path) {
  val conditionsEntry = ConditionsEntryPredicates(path / "conditionsEntry")
}