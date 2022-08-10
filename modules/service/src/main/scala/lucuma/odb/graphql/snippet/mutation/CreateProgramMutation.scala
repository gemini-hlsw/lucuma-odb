// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package mutation

import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.syntax.all._
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.skunk.SkunkMapping
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.odb.graphql.snippet.predicates.ProgramPredicates
import lucuma.odb.graphql.util.MutationCompanionOps
import lucuma.odb.service.ProgramService

trait CreateProgramMutation[F[_]: MonadCancelThrow]
  extends MutationCompanionOps[F]
     with ProgramPredicates[F]
  { this: SkunkMapping[F] =>

  def programService: Resource[F, ProgramService[F]]

  val CreateProgramMutation: Mutation =
    Mutation.simple { (child, env) =>
      env.getR[Option[NonEmptyString]]("name").flatTraverse { name =>
        programService.use(_.insertProgram(name)).map { id =>
          Result(Unique(Filter(ProgramPredicates.hasProgramId(id), child)))
        }
      }
    }

}