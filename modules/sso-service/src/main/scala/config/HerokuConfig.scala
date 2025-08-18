// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.config

import cats.implicits.*
import ciris.*
import lucuma.sso.service.config.HerokuConfig.Default
import lucuma.sso.service.config.HerokuConfig.Review

import java.time.LocalDateTime
import java.util.UUID

sealed trait HerokuConfig {

  def appName: String

  def versionText: String =
    this match {
      case Review(_, branch, prNumber) =>
        s"Pull Request ${prNumber.foldMap(n => s"#$n ")}from $branch"
      case Default(_, _, _, releaseCreatedAt, releaseVersion, slugCommit, _) =>
        s"$releaseVersion/$slugCommit created at $releaseCreatedAt"
    }

}

object HerokuConfig {

  /** Configuration provided for review apps.
    * @param appName  The name of the review app.
    * @param branch   The name of the remote branch the review app is tracking.
    * @param prNumber The GitHub Pull Request number if the review app is created automatically.
    * @see https://devcenter.heroku.com/articles/github-integration-review-apps#injected-environment-variables
    */
  case class Review(
    appName:  String,
    branch:   String,
    prNumber: Option[Int],
  ) extends HerokuConfig

  /** Configuration provided for staging and production.
    * @param appId	          The unique identifier for the application.
    * @param appName	        The application name.
    * @param dynoId	          The dyno identifier.
    * @param releaseCreatedAt	The time and date the release was created.
    * @param releaseVersion	  The identifier for the current release.
    * @param slugCommit	      The commit hash for the current release.
    * @param slugDescription	The description of the current release.
    * @see https://devcenter.heroku.com/articles/dyno-metadata
    */
  case class Default(
    appId:            UUID,
    appName:          String,
    dynoId:           UUID,
    releaseCreatedAt: LocalDateTime,
    releaseVersion:   String,
    slugCommit:       String,
    slugDescription:  String,
  ) extends HerokuConfig

  val review: ConfigValue[Effect, Review] = (
    envOrProp("HEROKU_APP_NAME"),
    envOrProp("HEROKU_BRANCH"),
    envOrProp("HEROKU_PR_NUMBER").as[Int].option,
  ).mapN(Review.apply)

  val default: ConfigValue[Effect, Default] = (
    envOrProp("HEROKU_APP_ID").as[UUID],
    envOrProp("HEROKU_APP_NAME"),
    envOrProp("HEROKU_DYNO_ID").as[UUID],
    envOrProp("HEROKU_RELEASE_CREATED_AT").as(using isoLocalDateTime),
    envOrProp("HEROKU_RELEASE_VERSION"),
    envOrProp("HEROKU_SLUG_COMMIT"),
    envOrProp("HEROKU_SLUG_DESCRIPTION"),
  ).mapN(Default.apply)

  val config: ConfigValue[Effect, HerokuConfig] =
    default.widen or review.widen

}


