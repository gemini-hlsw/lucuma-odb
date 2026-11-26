// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import cats.syntax.all.*
import lucuma.common.config.DatabaseSsl
import lucuma.odb.FMain
import munit.Location
import skunk.Session
import skunk.codec.all.*
import skunk.implicits.*

class databaseTls extends OdbSuite:

  def validUsers = Nil

  // The tests open their own connections, so no ODB fixtures are needed
  override def munitFixtures = Nil

  private def requireTls = databaseConfig.copy(connection = databaseConfig.connection.copy(ssl = DatabaseSsl.Require))

  private def messages(t: Throwable): List[String] =
    Iterator.iterate(t)(_.getCause).takeWhile(_ != null).flatMap(e => Option(e.getMessage)).toList

  private def assertRefused(fa: IO[Any], expected: String)(using Location): IO[Unit] =
    fa.attempt.map:
      case Left(e)  =>
        val ms = messages(e)
        assert(ms.exists(_.contains(expected)), ms.mkString(" / "))
      case Right(_) => fail("connected without TLS")

  private def selectOne(s: Session[IO]): IO[Int] =
    s.unique(sql"SELECT 1".query(int4))

  test("the test database itself is reachable without TLS"):
    FMain.singleSession[IO](databaseConfig).use(selectOne).assertEquals(1)

  test("a single session that requires TLS refuses a server without TLS"):
    assertRefused(FMain.singleSession[IO](requireTls).use(selectOne), "SSL not available")

  test("a pooled session that requires TLS refuses a server without TLS"):
    assertRefused(FMain.databasePoolResource[IO](requireTls).flatten.use(selectOne), "SSL not available")

  test("Flyway that requires TLS refuses a server without TLS"):
    assertRefused(FMain.migrateDatabase[IO](requireTls), "does not support SSL")
