// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.http4s

import cats.effect.Sync

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

/**
 * Calculates the base dir of the application.
 */
def baseDir[F[_]: Sync]: F[Path] = Sync[F].delay:
  // https://stackoverflow.com/questions/320542/how-to-get-the-path-of-a-running-jar-file
  val appPath =
    Paths.get(this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI).getParent.getParent
  if (Files.exists(appPath)) appPath else Paths.get(System.getProperty("user.home"))
