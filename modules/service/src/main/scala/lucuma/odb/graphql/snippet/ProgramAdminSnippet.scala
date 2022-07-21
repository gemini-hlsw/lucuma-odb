// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet

import cats.effect.MonadCancelThrow
import cats.effect.kernel.Resource
import cats.syntax.all._
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.QueryInterpreter
import edu.gemini.grackle.Result
import edu.gemini.grackle.ScalarType
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.Value
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.odb.graphql.util._
import lucuma.odb.service.ProgramService
import natchez.Trace
import skunk.Session

object ProgramAdminSnippet {

  def apply[F[_]: MonadCancelThrow: Trace](
    m: SnippetMapping[F] with ComputeMapping[F],
    sessionPool: Resource[F, Session[F]],
    user: User,
  ): m.Snippet = {
    import m.{ ObjectMapping, ComputeRoot, Snippet, schema }

    val pool = sessionPool.map(ProgramService.fromSessionAndUser(_, user))

    val MutationType = schema.ref("Mutation")

    def deleteProgram(env: Cursor.Env): F[Result[Boolean]] =
      env.get[Program.Id]("id") match {
        case Some(id) => pool.use { s => s.deleteProgram(id).map(Result(_)) }
        case None     => QueryInterpreter.mkErrorResult[Boolean](s"Implementation error: expected 'id' in $env.").pure[F]
      }

    val typeMappings =
      List(
        ObjectMapping(
          tpe = MutationType,
          fieldMappings = List(
            ComputeRoot("deleteProgram", ScalarType.BooleanType, deleteProgram),
          )
        ),
      )

    val elaborator = Map[TypeRef, PartialFunction[Select, Result[Query]]](
      MutationType -> {
        case Select("deleteProgram", List(Binding("id", Value.StringValue(sid))), child) =>
          Gid[Program.Id].fromString.getOption(sid) match {
            case Some(id) =>
              Environment(
                Cursor.Env("id" -> id),
                Select("deleteProgram", Nil, child)
              ).rightIor
            case None =>
              Result.failure(s"$sid is not a valid program id.")
          }
      }
    )

    Snippet(typeMappings, elaborator)

  }


}