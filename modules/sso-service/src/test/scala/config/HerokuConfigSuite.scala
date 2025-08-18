// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.config

import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.Semaphore
import cats.implicits.*
import weaver.*

object HerokuConfigSuite extends IOSuite {

  // We need to synchronize access to system poperties because it's global.
  override type Res = Semaphore[IO]
  override def sharedResource: Resource[IO, Semaphore[IO]] =
    Resource.eval(Semaphore[IO](1L))

  def putSystemProperty(key: String, value: String): IO[Unit] =
    IO(System.getProperties().put(key, value)).void

  def removeSystemProperty(key: String): IO[Unit] =
    IO(System.getProperties().remove(key)).void

  // Remove system properties whose keys contain the given substring.
  def removeAllSystemProperties(substring: String): IO[Unit] =
     IO(sys.props.keySet.toList.filter(_.contains(substring))).flatMap(_.traverse_(removeSystemProperty))

  // Clear out all app-specific system properties
  val reset =
    removeAllSystemProperties("LUCUMA")  *>
    removeAllSystemProperties("HEROKU")  *>
    removeSystemProperty("PORT")         *>
    removeSystemProperty("DATABASE_URL")

  test("review") { sem =>
    sem.permit.use { _ =>
      for {
        _ <- reset
        _ <- putSystemProperty("HEROKU_APP_NAME", "chickenpants")
        _ <- putSystemProperty("HEROKU_BRANCH", "fix the thing")
        _ <- HerokuConfig.review.load[IO]
      } yield expect(true)
    }
  }

  test("default") { sem =>
    sem.permit.use { _ =>
      for {
        _ <- reset
        _ <- putSystemProperty("HEROKU_APP_ID", "C637518A-1A35-4649-AB89-2CBDAC214F2D")
        _ <- putSystemProperty("HEROKU_APP_NAME", "...")
        _ <- putSystemProperty("HEROKU_DYNO_ID", "C637518A-1A35-4649-AB89-2CBDAC214F2D")
        _ <- putSystemProperty("HEROKU_RELEASE_CREATED_AT", "2020-08-28T21:05:15Z")
        _ <- putSystemProperty("HEROKU_RELEASE_VERSION", "...")
        _ <- putSystemProperty("HEROKU_SLUG_COMMIT", "...")
        _ <- putSystemProperty("HEROKU_SLUG_DESCRIPTION", "...")
        _ <- HerokuConfig.default.load[IO]
      } yield expect(true)
    }
  }

  test("config (review)") { sem =>
    sem.permit.use { _ =>
      for {
        _ <- reset
        _ <- putSystemProperty("HEROKU_APP_NAME", "chickenpants")
        _ <- putSystemProperty("HEROKU_BRANCH", "fix the thing")
        _ <- HerokuConfig.config.load[IO]
      } yield expect(true)
    }
  }

  test("config (staging/production)") { sem =>
    sem.permit.use { _ =>
      for {
        _ <- reset
        _ <- putSystemProperty("HEROKU_APP_ID", "C637518A-1A35-4649-AB89-2CBDAC214F2D")
        _ <- putSystemProperty("HEROKU_APP_NAME", "...")
        _ <- putSystemProperty("HEROKU_DYNO_ID", "C637518A-1A35-4649-AB89-2CBDAC214F2D")
        _ <- putSystemProperty("HEROKU_RELEASE_CREATED_AT", "2020-08-28T21:05:15Z")
        _ <- putSystemProperty("HEROKU_RELEASE_VERSION", "...")
        _ <- putSystemProperty("HEROKU_SLUG_COMMIT", "...")
        _ <- putSystemProperty("HEROKU_SLUG_DESCRIPTION", "...")
        _ <- HerokuConfig.config.load[IO]
      } yield expect(true)
    }
  }

}
