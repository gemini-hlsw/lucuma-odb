// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicates

import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate.*
import lucuma.odb.data.Existence

class ExistencePredicates(path: List[String]) extends LeafPredicates[Existence](path) {

  def includeDeleted(b: Boolean): Predicate =
    if (b) True else Eql(UniquePath(path), Const[Existence](Existence.Present))

}
