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
import lucuma.core.model.StandardRole
import lucuma.core.model.StandardRole.Admin
import lucuma.core.model.StandardRole.Ngo
import lucuma.core.model.StandardRole.Pi
import lucuma.core.model.StandardRole.Staff

object OdbMapping {

  case class Channels[F[_]]()

  object Channels {
    def apply[F[_]](pool: Resource[F, Session[F]]): Resource[F, Channels[F]] =
      pool.map { _ =>
        Channels()
      }
  }

  def apply[F[_]: Sync: Trace](
    channels: Channels[F],
    pool:     Resource[F, Session[F]],
    monitor:  SkunkMonitor[F],
    user:     User,
  ):  F[Mapping[F]] =
    Trace[F].span(s"Creating mapping for ${user.displayName} (${user.id}, ${user.role})") {
      val m: Mapping[F] = new SkunkMapping[F](pool, monitor) with SnippetMapping[F] {

        val snippet: Snippet =
          user.role match {
            // case Admin(id) =>
            // case GuestRole =>
            // case ServiceRole(serviceName) =>
            // case Ngo(id, partner) =>
            // case Pi(id) =>
            // case Staff(id) =>
            case _ =>
              NonEmptyList.of(
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
      m.pure[F]

    }

}