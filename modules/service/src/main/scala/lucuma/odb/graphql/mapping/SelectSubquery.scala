// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import cats.Order
import cats.syntax.option.*
import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Predicate.and
import grackle.Query.FilterOrderByOffsetLimit
import grackle.Query.OrderSelection
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import lucuma.odb.graphql.predicate.LeafPredicates
import lucuma.odb.graphql.predicate.ProgramPredicates

trait SelectSubquery {

  def selectWithOffsetAndLimit[A: Order](
    rOFFSET:         Result[Option[A]],
    rOFFSET_INCLUDE: Result[Boolean],
    rLIMIT:          Result[Option[NonNegInt]],
    typeRef:         TypeRef,
    offsetField:     String,
    offsetPredicate: LeafPredicates[A],
    progPredicates:  ProgramPredicates
  ): Elab[Unit] =
    UserEnv.elab: user =>
      Elab.transformChild { child =>
        (rOFFSET, rOFFSET_INCLUDE, rLIMIT).parTupled.flatMap { (OFFSET, OFFSET_INCLUDE, LIMIT) =>
          val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
          ResultMapping.selectResult(child, limit) { q =>
            FilterOrderByOffsetLimit(
              pred = and(List(
                offsetPredicate.offset(OFFSET, OFFSET_INCLUDE),
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
