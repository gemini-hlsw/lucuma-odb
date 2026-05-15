// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource

import cats.effect.*
import munit.CatsEffectSuite
import munit.Location
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait ResourceBaseSuite:
  this: CatsEffectSuite =>

  given Logger[IO] = Slf4jLogger.getLogger

  extension [A](opt: Option[A])
    def value(using Location): A = opt.getOrElse(fail(s"Expected Some, but got $opt"))

  extension [A](either: Either[?, A])
    def value(using Location): A =
      either.getOrElse(fail(s"Expected Right, but got $either"))

  extension [A](either: Either[A, ?])
    def leftValue(using Location): A =
      either.left.getOrElse(fail(s"Expected Left, but got $either"))
