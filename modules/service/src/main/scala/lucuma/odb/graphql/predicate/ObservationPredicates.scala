// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package predicates

import edu.gemini.grackle.Path.ListPath
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate.*
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.model.Access.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.Existence

import mapping.ObservationMapping

trait ObservationPredicates[F[_]] extends ObservationMapping[F]
                                     with ProgramPredicates[F] { this: SkunkMapping[F] =>

    object ObservationPredicates {

      def includeDeleted(b: Boolean): Predicate =
        if (b) True else Eql(UniquePath(List("existence")), Const[Existence](Existence.Present))

      def hasObservationId(oid: Observation.Id): Predicate =
        Eql(UniquePath(List("id")), Const(oid))

      def inObservationIds(oids: List[Observation.Id]): Predicate =
        In(UniquePath(List("id")), oids)

      def isVisibleTo(user: User): Predicate =
        ProgramPredicates.isVisibleTo(user, List("program"))

      def isWritableBy(user: User): Predicate =
        isVisibleTo(user)

    }

}
