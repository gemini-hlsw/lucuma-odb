// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import cats.syntax.all._
import edu.gemini.grackle.ObjectType

@munit.IgnoreSuite // comment this out if you want to run this. there's probably a better way to do this
class Checklist extends OdbSuite {
  val validUsers = Nil

  // These types are "mapped" by virtue of only appearing in Json blobs that
  // we manufacture. This includes ITC and SourceProfile stuff.
  val JsonMappedTypes: Set[String] =
    Set(
      "BandBrightnessIntegrated",
      "BandBrightnessSurface",
      "BandNormalizedIntegrated",
      "BandNormalizedSurface",
      "EmissionLineIntegrated",
      "EmissionLineSurface",
      "EmissionLinesIntegrated",
      "EmissionLinesSurface",
      "FluxDensityContinuumIntegrated",
      "FluxDensityContinuumSurface",
      "FluxDensityEntry",
      "GaussianSource",
      "ItcMissingParams",
      "ItcResultSet",
      "ItcServiceError",
      "ItcSuccess",
      "LineFluxIntegrated",
      "LineFluxSurface",
      "PlannedTime",
      "SourceProfile",
      "SpectralDefinitionIntegrated",
      "SpectralDefinitionSurface",
    )

  def printObjectType(m: BaseMapping[IO], t: ObjectType): IO[Unit] =
    if JsonMappedTypes.contains(t.name) then
      IO.println(s"- [x] ${t.name} (via Json blob)")
    else
      m.typeMapping(t) match
        case None    => IO.println(s"- [ ] ${t.name}")
        case Some(om: m.ObjectMapping) =>
          IO.println(s"- [x] ${t.name}") >>
          t.fields.traverse_ { f =>
            om.fieldMapping(f.name) match
              case None => IO.println(s"  - [ ] ${f.name}")
              case Some(_) => IO.println(s"  - [x] ${f.name}")
          }
        case Some(sm: m.SwitchMapping) =>
          sm.lookup.traverse_ { (p, om) =>
            IO.println(s"- [x] ${t.name} (at ${p.path.mkString(s"${p.rootTpe}/", "/", "")})") >>
            t.fields.traverse_ { f =>
              om.fieldMapping(f.name) match
                case None => IO.println(s"  - [ ] ${f.name}")
                case Some(_) => IO.println(s"  - [x] ${f.name}")
            }
          }
        case m => fail(s"Can't handle object mapping $m")

  def printChecklist(m: BaseMapping[IO]): IO[Unit] = {
    IO.println("\n\n# Schema Coverage") >>
    IO.println("") >>
    m.schema.types.collect { case o: ObjectType => o } .sortBy(_.name).traverse_(printObjectType(m, _)) >>
    IO.println("\n\n")
  }

  test("schema coverage checklist") {
    mapping.use {
      case m: BaseMapping[IO] => printChecklist(m)
      case _ => fail("Not a base mapping??!??")
    }
  }

}
