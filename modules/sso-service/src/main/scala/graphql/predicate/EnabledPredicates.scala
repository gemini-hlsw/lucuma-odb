// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql
package predicate

import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.predicate.LeafPredicates

class EnabledPredicates(path: Path) extends LeafPredicates[Boolean](path) {

  def includeDisabled(b: Boolean): Predicate =
    if (b) True else Eql(path, Const(true))

}
