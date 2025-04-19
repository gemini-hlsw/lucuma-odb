// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.data.Existence

class ExistencePredicates(path: Path) extends LeafPredicates[Existence](path) {

  def includeDeleted(b: Boolean): Predicate =
    if (b) True else Eql(path, Const[Existence](Existence.Present))

}
