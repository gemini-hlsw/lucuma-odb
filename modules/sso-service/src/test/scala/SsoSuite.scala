// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.Eq
import cats.effect.*
import cats.syntax.all.*
import munit.CatsEffectSuite
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait SsoSuite extends CatsEffectSuite {

  implicit val logger: Logger[IO] =
    Slf4jLogger.getLogger

  def assertEq[A: Eq](obtained: A, expected: A, clue: => Any = ()): Unit =
    assert(obtained === expected, clue)

  def assertEqIO[A: Eq](obtained: A, expected: A): IO[Unit] =
    IO(obtained === expected).assert

}