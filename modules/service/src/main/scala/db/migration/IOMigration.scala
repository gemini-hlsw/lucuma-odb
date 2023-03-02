// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.effect.Clock
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.flywaydb.core.api.migration.Context
import org.flywaydb.core.api.migration.JavaMigration
import org.postgresql.core.BaseConnection


trait IOMigration extends JavaMigration {

  override val canExecuteInTransaction: Boolean =
    true

  override def migrate(ctx: Context): Unit =
    (ctx.getConnection match {
      case bc: BaseConnection =>
        for {
          _ <- IO.println(s"Starting migration: $getDescription")
          t <- Clock[IO].timed(ioMigrate(ctx, bc)).map(_._1)
          _ <- IO.println(s"Completed migration $getDescription in $t")
        } yield ()
      case _                  =>
        IO.raiseError(new RuntimeException("Sorry, expected a postgres BaseConnection"))
    }).unsafeRunSync()

  def ioMigrate(
    ctx:            Context,
    baseConnection: BaseConnection
  ): IO[Unit]

}
