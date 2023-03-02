// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.effect.IO
import cats.effect.SyncIO
import cats.effect.Resource
import cats.effect.unsafe.implicits.global
import natchez.Trace.Implicits.noop
import skunk.Session
import skunk.codec.all.*
import skunk.implicits.*
import org.flywaydb.core.api.migration.BaseJavaMigration
import org.flywaydb.core.api.migration.Context

abstract class SkunkMigration extends BaseJavaMigration {

  override final def migrate(ctx: Context): Unit = {

    // Fish the session construction parameters out of the `Context`.  These
    // were in turn provided by the Ciris config but there seems to be no clear
    // way to get at that directly from a migration.

    val conf = ctx.getConfiguration
    val uri  = conf.getUrl match {
      case s"jdbc:$uriString" => new java.net.URI(uriString)
      case s                  => throw new RuntimeException(s"Unexpected database URI: $s")
    }

    val session: Resource[IO, Session[IO]] =
      Session.single[IO](
        host     = uri.getHost,
        port     = uri.getPort,
        user     = conf.getUser,
        database = uri.getPath.drop(1),     // drop leading slash
        password = Option(conf.getPassword)
      )

    session.use(migrateInSession).unsafeRunSync()
  }

  def migrateInSession(s: Session[IO]): IO[Unit]

}
