// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// package lucuma.odb.graphql

// import cats.effect.IO
// import cats.syntax.all.*
// import grackle.Field
// import grackle.ObjectType

// @munit.IgnoreSuite // comment this out if you want to run this. there's probably a better way to do this
// class Checklist extends OdbSuite {
//   val validUsers = Nil

//   // These types are "mapped" by virtue of only appearing in Json blobs that
//   // we manufacture. This includes ITC and SourceProfile stuff.
//   val JsonMappedTypes: Set[String] =
//     Set(
//       "AllConfigChangeEstimates",
//       "AllDetectorEstimates",
//       "BandBrightnessIntegrated",
//       "BandBrightnessSurface",
//       "BandNormalizedIntegrated",
//       "BandNormalizedSurface",
//       "CategorizedTime",
//       "CategorizedTimeRange",
//       "ConfigChangeEstimate",
//       "DatasetEstimate",
//       "DetectorEstimate",
//       "EmissionLineIntegrated",
//       "EmissionLineSurface",
//       "EmissionLinesIntegrated",
//       "EmissionLinesSurface",
//       "ExecutionDigest",
//       "FluxDensityContinuumIntegrated",
//       "FluxDensityContinuumSurface",
//       "FluxDensityEntry",
//       "GaussianSource",
//       "GmosNorthAtom",
//       "GmosNorthExecutionConfig",
//       "GmosNorthExecutionSequence",
//       "GmosNorthStep",
//       "GmosSouthAtom",
//       "GmosSouthExecutionConfig",
//       "GmosSouthExecutionSequence",
//       "GmosSouthStep",
//       "GuideAvailabilityPeriod",
//       "GuideEnvironment",
//       "GuideTarget",
//       "Itc",
//       "ItcMissingParams",
//       "ItcResult",
//       "ItcResultSet",
//       "ItcServiceError",
//       "ItcSuccess",
//       "LineFluxIntegrated",
//       "LineFluxSurface",
//       "SequenceDigest",
//       "SetupTime",
//       "SourceProfile",
//       "SpectralDefinitionIntegrated",
//       "SpectralDefinitionSurface",
//       "StepEstimate",
//       "WavelengthDither"
//     )

//   def printObjectType(m: BaseMapping[IO], t: ObjectType): IO[Unit] =
//     if JsonMappedTypes.contains(t.name) then
//       IO.println(s"- [x] ${t.name} (via Json blob)")
//     else {

//       def definedInInterface(f: Field): Option[String] =
//         t.interfaces.find { nt =>
//           m.typeMapping(nt).exists {
//             case om: m.ObjectMapping =>
//                om.fieldMapping(f.name).isDefined
//             case sm: m.SwitchMapping =>
//                sm.lookup.exists { case (p, om) =>
//                  om.fieldMapping(f.name).isDefined
//                }
//             case _                   =>
//                false
//           }
//         }.map(_.name)

//       def printField(om: m.ObjectMapping, f: Field): IO[Unit] =
//         om.fieldMapping(f.name) match {
//           case None    => IO.println(s"  - ${definedInInterface(f).fold(s"[ ] ${f.name}")(n => s"[x] ${f.name} (via $n)")}")
//           case Some(_) => IO.println(s"  - [x] ${f.name}")
//         }

//       m.typeMapping(t) match
//         case None                      =>
//           IO.println(s"- [ ] ${t.name}")
//         case Some(om: m.ObjectMapping) =>
//           IO.println(s"- [x] ${t.name}") >> t.fields.traverse_ { f => printField(om, f) }
//         case Some(sm: m.SwitchMapping) =>
//           sm.lookup.traverse_ { (p, om) =>
//             IO.println(s"- [x] ${t.name} (at ${p.path.mkString(s"${p.rootTpe}/", "/", "")})") >>
//             t.fields.traverse_ { f => printField(om, f) }
//           }
//         case m => fail(s"Can't handle object mapping $m")
//   }

//   def printChecklist(m: BaseMapping[IO]): IO[Unit] = {
//     IO.println("\n\n# Schema Coverage") >>
//     IO.println("") >>
//     m.schema.types.collect { case o: ObjectType => o } .sortBy(_.name).traverse_(printObjectType(m, _)) >>
//     IO.println("\n\n")
//   }

//   test("schema coverage checklist") {
//     mapping.use {
//       case m: BaseMapping[IO] => printChecklist(m)
//       case _ => fail("Not a base mapping??!??")
//     }
//   }

// }
