// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import _root_.skunk.Session
import cats.data.NonEmptyList
import cats.effect.{Unique => _, _}
import cats.syntax.all._
import edu.gemini.grackle._
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.skunk.SkunkMonitor
import lucuma.core.model.User
import natchez.Trace
import lucuma.odb.graphql.snippet._
import lucuma.core.model.GuestRole
import lucuma.core.model.ServiceRole
import lucuma.core.model.StandardRole.Admin
import lucuma.core.model.StandardRole.Ngo
import lucuma.core.model.StandardRole.Pi
import lucuma.core.model.StandardRole.Staff
import org.tpolecat.sourcepos.SourcePos
import cats.Applicative
import lucuma.odb.graphql.util._

object OdbMapping {

  case class Channels[F[_]]()

  object Channels {
    def apply[F[_]](pool: Resource[F, Session[F]]): Resource[F, Channels[F]] =
      pool.map { _ =>
        Channels()
      }
  }

  def apply[F[_]: Sync: Trace](
    pool:     Resource[F, Session[F]],
    monitor:  SkunkMonitor[F],
    user:     User,
  ):  F[Mapping[F]] =
    Trace[F].span(s"Creating mapping for ${user.displayName} (${user.id}, ${user.role})") {
      pool.use(enumSchema(_)).map { enums =>
        val m: Mapping[F] = new SkunkMapping[F](pool, monitor) with SnippetMapping[F] with ComputeMapping[F] {

          val snippet: Snippet =
            user.role match {

              case Admin(_) | ServiceRole(_) =>
                NonEmptyList.of(
                  BaseSnippet(this),
                  Snippet(enums, Nil),
                  FilterTypeSnippet(this),
                  PartnerSnippet(this),
                  UserSnippet(this),
                  ProgramSnippet(this, pool, user),
                  ProgramAdminSnippet(this, pool), // only for admin/service users
                ).reduce

              case GuestRole | Ngo(_, _) | Pi(_) | Staff(_) =>
                NonEmptyList.of(
                  BaseSnippet(this),
                  Snippet(enums, Nil),
                  FilterTypeSnippet(this),
                  PartnerSnippet(this),
                  UserSnippet(this),
                  ProgramSnippet(this, pool, user),
                ).reduce

            }

          val schema = snippet.schema
          val typeMappings = snippet.typeMappings
          override val selectElaborator = snippet.selectElaborator

        }
        m.validator.validateMapping().map(_.toErrorMessage).foreach(println)
        m
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