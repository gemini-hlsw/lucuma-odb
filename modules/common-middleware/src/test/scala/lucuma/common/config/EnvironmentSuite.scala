// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.config

import cats.effect.IO
import ciris.*
import munit.CatsEffectSuite

class EnvironmentSuite extends CatsEffectSuite {

  val Key = "TEST_ENVIRONMENT"

  // A lookup over a fixed map, so the tests do not depend on (or change) the process environment
  // or the JVM-global system properties.
  def lookup(values: (String, String)*): String => ConfigValue[Effect, String] = {
    val m = values.toMap
    key => m.get(key).fold(ConfigValue.missing(ConfigKey.env(key)))(ConfigValue.loaded(ConfigKey.env(key), _))
  }

  def load(values: (String, String)*): ConfigValue[Effect, Environment] =
    Environment.load(Key, lookup(values*))

  def assertFails(cv: ConfigValue[Effect, Environment], expected: String): IO[Unit] =
    cv.attempt[IO].map {
      case Left(err) =>
        assert(
          err.messages.exists(_.contains(expected)),
          s"expected an error about $expected, got: ${err.messages.toList.mkString("; ")}"
        )
      case Right(e)  =>
        fail(s"expected the config to fail, got environment $e")
    }

  test("environment does not load when the variable is not set") {
    assertFails(load(), Key)
  }

  test("environment does not load when the variable is not set, also on Heroku") {
    assertFails(load("DYNO" -> "web.1"), Key)
  }

  test("environment does not load an unknown value") {
    assertFails(load(Key -> "review"), Key)
  }

  test("environment loads Local when DYNO is not set") {
    assertIO(load(Key -> "local").load[IO], Environment.Local)
  }

  test("environment refuses Local when DYNO is set (running on Heroku)") {
    assertFails(load(Key -> "local", "DYNO" -> "web.1"), "DYNO")
  }

  List(
    "dev"         -> Environment.Dev,
    "development" -> Environment.Dev,
    "Development" -> Environment.Dev,
    "staging"     -> Environment.Staging,
    "production"  -> Environment.Production
  ).foreach { case (value, expected) =>
    test(s"environment loads $expected from '$value' on Heroku") {
      assertIO(load(Key -> value, "DYNO" -> "web.1").load[IO], expected)
    }
  }

}
