// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import _root_.skunk.AppliedFragment
import _root_.skunk.Session
import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.{Unique => _, _}
import cats.effect.std.Supervisor
import cats.Monoid
import cats.syntax.all._
import com.github.vertical_blank.sqlformatter.SqlFormatter
import edu.gemini.grackle._
import edu.gemini.grackle.QueryCompiler.SelectElaborator
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.skunk.SkunkMonitor
import edu.gemini.grackle.sql.SqlMapping
import fs2.concurrent.Topic
import lucuma.core.model.User
import lucuma.odb.graphql.snippet._
import lucuma.odb.graphql.snippet.mapping._
import lucuma.odb.graphql.topic.ProgramTopic
import lucuma.odb.graphql.util._
import lucuma.odb.service.ProgramService
import natchez.Trace
import org.tpolecat.sourcepos.SourcePos
import org.typelevel.log4cats.Logger
import scala.io.AnsiColor
import scala.io.Source

object OdbMapping {

  case class Topics[F[_]](
    program: Topic[F, ProgramTopic.Element]
  )

  object Topics {
    def apply[F[_]: Concurrent: Logger](pool: Resource[F, Session[F]]): Resource[F, Topics[F]] =
      for {
        sup <- Supervisor[F]
        ses <- pool
        pro <- Resource.eval(ProgramTopic(ses, 1024, sup))
      } yield Topics(pro)
  }

  // Loads a GraphQL file from the classpath, relative to this Class.
  def unsafeLoadSchema(fileName: String): Schema = {
    val stream = getClass.getResourceAsStream(fileName)
    val src  = Source.fromInputStream(stream, "UTF-8")
    try Schema(src.getLines().mkString("\n")).toEither.fold(x => sys.error(s"Invalid schema: $fileName: ${x.toList.mkString(", ")}"), identity)
    finally src.close()
  }

  private implicit def monoidPartialFunction[A, B]: Monoid[PartialFunction[A, B]] =
    Monoid.instance(PartialFunction.empty, _ orElse _)

  def apply[F[_]: Sync: Trace: Logger](
    database:     Resource[F, Session[F]],
    monitor:  SkunkMonitor[F],
    user0:     User,
    topics:   Topics[F],
  ):  F[Mapping[F]] =
    Trace[F].span(s"Creating mapping for ${user0.displayName} (${user0.id}, ${user0.role})") {
      database.use(enumSchema(_)).map { enums =>
        new SkunkMapping[F](database, monitor) with SnippetMapping
          // mappings
          with CreateProgramResultMapping[F]
          with LeafMappings[F]
          with MutationMapping[F]
          with NonNegDurationMapping[F]
          with PlannedTimeSummaryMapping[F]
          with ProgramMapping[F]
          with ProgramUserMapping[F]
          with QueryMapping[F]
          with UserMapping[F]
        {

          // Our schema
          val schema = unsafeLoadSchema("OdbSchema.graphql") |+| enums

          // Current user (needed by some mixins)
          val user = user0

          // Our services
          lazy val programService = pool.map(ProgramService.fromSessionAndUser(_, user))

          // Our combined type mappings
          val typeMappings: List[TypeMapping] =
            List(
              CreateProgramResultMapping,
              MutationMapping,
              NonNegDurationMapping,
              PlannedTimeSummaryMapping,
              ProgramMapping,
              ProgramUserMapping,
              QueryMapping,
              UserMapping,
            ) ++ LeafMappings

          // Our combined select elaborator
          override val selectElaborator: SelectElaborator =
            SelectElaborator(
              List(
                MutationElaborator,
                ProgramElaborator,
                QueryElaborator,
              ).combineAll
            )

          // Overide `fetch` to log the query. This is optional.
          override def fetch(fragment: AppliedFragment, codecs: List[(Boolean, Codec)]): F[Vector[Array[Any]]] = {
            Logger[F].info {
              val formatted = SqlFormatter.format(fragment.fragment.sql)
              val cleanedUp = formatted.replaceAll("\\$ (\\d+)", "\\$$1") // turn $ 42 into $42
              val colored   = cleanedUp.linesIterator.map(s => s"${AnsiColor.GREEN}$s${AnsiColor.RESET}").mkString("\n")
              s"\n\n$colored\n\n"
            } *>
            super.fetch(fragment, codecs)
          }

        }
      }
    }

  def enumSchema[F[_]: Applicative](s: Session[F]): F[Schema] =
    List(FilterTypeSnippet.enumType(s), PartnerSnippet.enumType(s)).sequence.map { tpes =>
      new Schema {
        def pos: SourcePos = SourcePos.instance
        def types: List[NamedType] = tpes
        def directives: List[Directive] = Nil
      }
    }

}