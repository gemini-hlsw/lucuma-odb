// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.data.OptionT
import cats.effect.IO

import java.io.InputStream

val ByteChunkSize: Int =
  8192

/**
 * Gets a list of named GCal configuration files from the classpath.  Each
 * instrument has two corresponding configuration files, one for flats and one
 * for arcs.
 */
def gcalFilesFromClasspath(first: String, rest: String*): NonEmptyList[(String, IO[InputStream])] =
  NonEmptyList(first, rest.toList).map { n =>
    val csv = s"smartgcal/$n.csv"
    csv ->
     OptionT(IO.blocking(Option(this.getClass.getClassLoader.getResourceAsStream(csv))))
       .getOrRaise(new RuntimeException(s"Could not find smartgcal input file: $csv"))

  }

