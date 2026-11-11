// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.effect.IO
import cats.effect.Resource
import org.postgresql.copy.CopyManager
import org.postgresql.core.BaseConnection

import java.io.InputStream
import java.sql.ResultSet

extension (bc: BaseConnection) {

  def ioCopyIn(sql: String, r: Resource[IO, InputStream]): IO[Unit] =
    for {
      m <- IO(CopyManager(bc))
      _ <- r.use { is => IO.blocking(m.copyIn(sql, is)) }.void
    } yield ()

  def ioUpdate(sql: String): IO[Unit] =
    IO.blocking(bc.execSQLUpdate(sql))

  def ioQueryOption[A](sql: String)(f: ResultSet => A): IO[Option[A]] =
    IO.blocking:
      val rs = bc.execSQLQuery(sql)
      try Option.when(rs.next())(f(rs))
      finally rs.close()

}
