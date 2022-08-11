// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package predicates

import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.model.Target
import lucuma.odb.data.Existence

import mapping.TargetMapping

trait TargetPredicates[F[_]] extends TargetMapping[F] { this: SkunkMapping[F] =>

  object TargetPredicates {

    def includeDeleted(b: Boolean): Predicate =
      if (b) True else Eql(UniquePath(List("existence")), Const[Existence](Existence.Present))

    def hasTargetId(tid: Target.Id): Predicate =
      Eql(UniquePath(List("id")), Const(tid))

  }

}
