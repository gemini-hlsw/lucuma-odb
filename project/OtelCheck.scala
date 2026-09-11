import sbt.*

import scala.xml.XML

/**
 * otel4s-oteljava is compiled against one specific opentelemetry-java release.
 * Our explicit `io.opentelemetry` dependencies have to stay on that same
 * version, or a Scala Steward bump silently puts a newer SDK under otel4s.
 * This reads the version otel4s actually declares out of the pom.
 */
object OtelCheck {

  def declaredOtelVersion(
    cache:          File,
    otel4sVersion:  String,
    scalaBinary:    String
  ): Option[String] = {
    val artifact = s"otel4s-oteljava_$scalaBinary"
    val pom      =
      cache / "https" / "repo1.maven.org" / "maven2" / "org" / "typelevel" /
        artifact / otel4sVersion / s"$artifact-$otel4sVersion.pom"

    if (!pom.exists) None
    else
      (XML.loadFile(pom) \ "dependencies" \ "dependency")
        .find { d =>
          (d \ "groupId").text == "io.opentelemetry" &&
          (d \ "artifactId").text == "opentelemetry-sdk"
        }
        .map(d => (d \ "version").text)
  }
}
