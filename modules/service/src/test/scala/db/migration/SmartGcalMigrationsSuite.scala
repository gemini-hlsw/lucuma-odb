// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters.*

// Ensure all smart gcal files in the c+ass path are tested
final class SmartGcalMigrationsSuite extends munit.FunSuite:

  private val migrations: List[SmartGcalMigration] =
    List(
      new R__SmartFlamingos2,
      new R__SmartGhost,
      new R__SmartGmosNorth,
      new R__SmartGmosSouth,
      new R__SmartGnirs,
      new R__SmartIgrins2
    )

  private def onClasspath: List[String] =
    val url = Option(getClass.getClassLoader.getResource("smartgcal")).getOrElse:
      fail("smartgcal resource directory not found on the classpath")
    assertEquals(url.getProtocol, "file", s"smartgcal resources must be a directory, found $url")
    Files.list(Paths.get(url.toURI)).iterator.asScala
      .map(_.getFileName.toString)
      .filter(_.endsWith(".csv"))
      .map(n => s"smartgcal/$n")
      .toList
      .sorted

  test("every smartgcal file is loaded by exactly one migration"):
    val listed = migrations.flatMap(_.definitionFiles.toList.map(_._1))
    val files  = onClasspath
    assert(files.nonEmpty)
    assertEquals(listed.diff(listed.distinct), Nil, "files listed by more than one migration")
    assertEquals(files.diff(listed), Nil, "files no migration loads; add them to an R__Smart* migration and to this suite")
    assertEquals(listed.diff(files), Nil, "files a migration lists but the classpath lacks")
