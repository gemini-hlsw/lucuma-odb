// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.data.OptionT
import cats.effect.IO
import java.io.InputStream

val ByteChunkSize: Int =
  8192

def gcalFilesFromClasspath(first: String, rest: String*): NonEmptyList[(String, IO[InputStream])] =
  NonEmptyList(first, rest.toList).map { n =>
    val csv = s"smartgcal/$n.csv"
    csv ->
     OptionT(IO(Option(this.getClass.getClassLoader.getResourceAsStream(csv))))
      .getOrRaise(new RuntimeException(s"Could not find smartgcal input file: $csv"))

  }

