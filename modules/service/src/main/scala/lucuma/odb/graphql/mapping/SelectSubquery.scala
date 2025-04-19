// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import cats.Order
import cats.syntax.option.*
import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Predicate.True
import grackle.Predicate.and
import grackle.Query.FilterOrderByOffsetLimit
import grackle.Query.OrderSelection
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import lucuma.core.model.User
import lucuma.odb.graphql.predicate.LeafPredicates
import lucuma.odb.graphql.predicate.ProgramPredicates

trait SelectSubquery {

  def user: User

  def selectWithOffsetAndLimit[A: Order](
    rOFFSET:         Result[Option[A]],
    rLIMIT:          Result[Option[NonNegInt]],
    typeRef:         TypeRef,
    offsetField:     String,
    offsetPredicate: LeafPredicates[A],
    progPredicates:  ProgramPredicates
  ): Elab[Unit] =
    Elab.transformChild { child =>
      (rOFFSET, rLIMIT).parTupled.flatMap { (OFFSET, LIMIT) =>
        val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
        ResultMapping.selectResult(child, limit) { q =>
          FilterOrderByOffsetLimit(
            pred = and(List(
              OFFSET.map(offsetPredicate.gtEql).getOrElse(True),
              progPredicates.isVisibleTo(user)
            )).some,
            oss    = List(OrderSelection[A](typeRef / offsetField)).some,
            offset = None,
            limit  = (limit + 1).some,
            child  = q
          )
        }
      }
    }

}
