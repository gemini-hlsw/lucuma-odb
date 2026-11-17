// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal

import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters.*

/** The smartgcal CSV files on the classpath, by name without the extension. */
object SmartGcalFiles:

  lazy val names: List[String] =
    val url = Option(getClass.getClassLoader.getResource("smartgcal")).getOrElse:
      sys.error("smartgcal resource directory not found on the classpath")
    require(url.getProtocol == "file", s"smartgcal resources must be a directory, found $url")
    Files.list(Paths.get(url.toURI)).iterator.asScala
      .map(_.getFileName.toString)
      .collect { case n if n.endsWith(".csv") => n.stripSuffix(".csv") }
      .toList
      .sorted
