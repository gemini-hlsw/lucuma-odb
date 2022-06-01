package lucuma.odb.graphql
package snippet

import cats.effect.MonadCancelThrow
import cats.effect.kernel.Resource
import cats.syntax.all._
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Path._
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.Value._
import edu.gemini.grackle.skunk.SkunkMapping
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import lucuma.core.model
import lucuma.core.model.Access._
import lucuma.core.model.User
import lucuma.odb.data._
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.graphql.util.Bindings._
import lucuma.odb.graphql.util._
import lucuma.odb.service.ProgramService
import lucuma.odb.util.Codecs._
import skunk.Session
import natchez.Trace
import lucuma.odb.service.ProgramService.LinkUserResponse._
import io.circe.Json
import lucuma.odb.graphql.snippet.input.CreateTargetInput

object TargetSnippet {

  val schema = unsafeLoadSchema(this)

  val QueryType      = schema.ref("Query")
  val MutationType   = schema.ref("Mutation")
  val TargetType     = schema.ref("Target")

  def apply[F[_]: MonadCancelThrow: Trace](
    m: SnippetMapping[F] with SkunkMapping[F] with MutationCompanionOps[F],
    sessionPool: Resource[F, Session[F]],
    user: User,
    topics: OdbMapping.Topics[F],
  ): m.Snippet = {

    import m.{ TableDef, ObjectMapping, Snippet, SqlRoot, SqlField, SqlObject, Join, Mutation, LeafMapping, MutationCompanionOps }

    val elaborator = Map[TypeRef, PartialFunction[Select, Result[Query]]](
      MutationType -> {
        case Select("createTarget", List(
          ProgramIdBinding("programId", rPid),
          CreateTargetInput.Binding("input", rInput),
        ), child) =>
          ???
      }
    )

    Snippet(schema, ???, elaborator)

  }

}
