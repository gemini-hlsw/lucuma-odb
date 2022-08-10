// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package mutation

import input.UpdateProgramsInput

import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.syntax.all._
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.skunk.SkunkMapping
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.User
import lucuma.odb.graphql.snippet.predicates.ProgramPredicates
import lucuma.odb.graphql.util.MutationCompanionOps
import lucuma.odb.service.ProgramService
import skunk.AppliedFragment

trait UpdateProgramsMutation[F[_]: MonadCancelThrow]
  extends MutationCompanionOps[F]
     with ProgramPredicates[F]
  { this: SkunkMapping[F] =>

  def programService: Resource[F, ProgramService[F]]

  def user: User

  val UpdateProgramsMutation: Mutation =
    Mutation.simple { (child, env) =>
      env.getR[UpdateProgramsInput]("input").flatTraverse { input =>

        // Our predicate for selecting programs to update
        val filterPredicate = and(List(
          ProgramPredicates.isWritableBy(user),
          ProgramPredicates.includeDeleted(input.includeDeleted.getOrElse(false)),
          input.WHERE.getOrElse(True),
        ))

        // An applied fragment that selects all program ids that satisfy `filterPredicate`
        val idSelect: Result[AppliedFragment] =
          Result.fromOption(
            MappedQuery(Filter(filterPredicate, Select("id", Nil, Empty)), Cursor.Context(ProgramType)).map(_.fragment),
            "Could not construct a subquery for the provided WHERE condition." // shouldn't happen
          )

        // Update the specified programs and then return a query for the same set of programs.
        idSelect.traverse { which =>
          programService.use(_.updatePrograms(input.SET, which)).as(Filter(filterPredicate, child))
        }

      }
    }

}