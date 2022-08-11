// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package predicates

import mapping.ObservationMapping

import edu.gemini.grackle.Path.ListPath
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.model.Access._
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.Existence

trait ObservationPredicates[F[_]] extends ObservationMapping[F] { this: SkunkMapping[F] =>

    object ObservationPredicates {

      def includeDeleted(b: Boolean): Predicate =
        if (b) True else Eql(UniquePath(List("existence")), Const[Existence](Existence.Present))

      def hasObservationId(oid: Observation.Id): Predicate =
        Eql(UniquePath(List("id")), Const(oid))

      def inObservationIds(oids: List[Observation.Id]): Predicate =
        In(UniquePath(List("id")), oids)

      def isVisibleTo(user: User): Predicate =
        user.role.access match {
          case Guest | Pi =>
            Or(
              Contains(ListPath(List("program", "users", "userId")), Const(user.id)), // user is linked, or
              Eql(UniquePath(List("program", "piUserId")), Const(user.id))            // user is the PI
            )
          case Ngo => ???
          case Staff | Admin | Service => True
        }

      def isWritableBy(user: User): Predicate =
        isVisibleTo(user)

    }

}
