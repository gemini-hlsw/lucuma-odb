// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate.*
import lucuma.odb.data.Existence
import edu.gemini.grackle.Path

class ExistencePredicates(path: Path) extends LeafPredicates[Existence](path) {

  def includeDeleted(b: Boolean): Predicate =
    if (b) True else Eql(path, Const[Existence](Existence.Present))

}
