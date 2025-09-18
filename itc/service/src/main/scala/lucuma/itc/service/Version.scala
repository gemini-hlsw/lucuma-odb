// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import buildinfo.BuildInfo
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.itc.service.config.*

import java.time.Instant
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

trait Version:
  val gitHash = BuildInfo.gitHeadCommit

  val versionDateFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyyMMdd").withZone(ZoneId.from(ZoneOffset.UTC))

  val versionDateTimeFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss").withZone(ZoneId.from(ZoneOffset.UTC))

  def version(environment: ExecutionEnvironment): NonEmptyString =
    val instant = Instant.ofEpochMilli(BuildInfo.buildDateTime)
    NonEmptyString.unsafeFrom(
      environment match
        case ExecutionEnvironment.Local => versionDateTimeFormatter.format(instant)
        case _                          =>
          versionDateFormatter.format(instant) +
            "-" + gitHash.map(_.take(7)).getOrElse("NONE")
    )
