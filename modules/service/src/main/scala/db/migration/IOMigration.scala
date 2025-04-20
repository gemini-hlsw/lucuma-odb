// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.effect.Clock
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.flywaydb.core.api.migration.Context
import org.flywaydb.core.api.migration.JavaMigration
import org.postgresql.core.BaseConnection


/**
 * Executes a programmatic migration in IO.  Implementors provide an `ioMigrate`
 * implementation.
 */
trait IOMigration extends JavaMigration {

  override val canExecuteInTransaction: Boolean =
    true

  override def migrate(ctx: Context): Unit =
    (ctx.getConnection match {
      case bc: BaseConnection =>
        for {
          _ <- IO.println(s"Starting migration.: $getDescription")
          t <- Clock[IO].timed(ioMigrate(ctx, bc)).map(_._1)
          _ <- IO.println(s"Completed migration: $getDescription in ${t.toMillis} ms")
        } yield ()
      case _                  =>
        IO.raiseError(new RuntimeException("Sorry, expected a postgres BaseConnection"))
    }).unsafeRunSync()

  /**
   * Provides the migration program in IO.
   */
  def ioMigrate(
    ctx:            Context,
    baseConnection: BaseConnection
  ): IO[Unit]

}
