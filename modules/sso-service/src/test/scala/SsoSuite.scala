// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.Eq
import cats.effect.*
import cats.syntax.all.*
import lucuma.sso.service.database.Database
import munit.CatsEffectSuite
import munit.Location
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import simulator.SsoSimulator

trait SsoSuite extends CatsEffectSuite with SsoSimulator {

  implicit val logger: Logger[IO] =
    Slf4jLogger.getLogger

  def assertEq[A: Eq](obtained: A, expected: A, clue: => Any = ())(using Location): Unit =
    assert(obtained === expected, clue)

  def assertEqIO[A: Eq](obtained: A, expected: A)(using Location): IO[Unit] =
    IO(obtained === expected).assert

  def db: Resource[IO, Database[IO]] =
    SsoSimulator[IO].flatMap(_._1)

}