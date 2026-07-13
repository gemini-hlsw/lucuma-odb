// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import buildinfo.BuildInfo
import eu.timepit.refined.types.string.NonEmptyString

trait Version:
  val gitHash = BuildInfo.gitHeadCommit

  // The reported server version is a content hash of the ITC service and model
  // Scala sources (see the `itcSourceHash` task in build.sbt). This way it only
  // changes when the ITC code actually changes, avoiding needless invalidation of
  // the ODB's cached ITC results (which are truncated whenever this version changes).
  def version: NonEmptyString =
    NonEmptyString.unsafeFrom(BuildInfo.itcSourceHash)
