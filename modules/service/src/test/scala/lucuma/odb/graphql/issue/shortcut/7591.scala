// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosDtax
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.StepGuideState.*
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.json.all.transport.given


class ShortCut_7591 extends ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      6.secondTimeSpan,
      PosInt.unsafeFrom(6)
    )

  val user: User = serviceUser

  def createTargetWithProfile(pid: Program.Id): IO[Target.Id] =
    query(
      user  = user,
      query =
      s"""
         mutation {
           createTarget(input: {
             programId: ${pid.asJson},
             SET: {
               name: "V1647 Orionis"
               sidereal: {
                 ra: { hms: "05:46:13.137" },
                 dec: { dms: "-00:06:04.89" },
                 epoch: "J2000.0",
                 properMotion: {
                   ra: {
                     milliarcsecondsPerYear: 0.918
                   },
                   dec: {
                     milliarcsecondsPerYear: -1.057
                   },
                 },
                 radialVelocity: {
                   kilometersPerSecond: 27.58
                 },
                 parallax: {
                   milliarcseconds: 2.422
                 }
               },
               sourceProfile: {
                 point: {
                   bandNormalized: {
                     sed: {
                       stellarLibrary: O5_V
                     },
                     brightnesses: [
                       {
                         band: J,
                         value: 14.74,
                         units: VEGA_MAGNITUDE
                       },
                       {
                         band: V,
                         value: 18.1,
                         units: VEGA_MAGNITUDE
                       }
                     ]
                   }
                 }
               }
             }
           }) {
             target {
               id
             }
           }
         }
      """
    ).map(_.hcursor.downFields("createTarget", "target", "id").require[Target.Id])

  // R150 grating with a 0.75" slit and IQ=1.0"

  def createGmosNorthLongSlitObservation(
    pid: Program.Id,
    tid: Target.Id,
  ): IO[Observation.Id] =
    query(
      user  = user,
      query =
      s"""
         mutation {
           createObservation(input: {
             programId: ${pid.asJson},
             SET: {
               constraintSet: {
                 cloudExtinction: POINT_ONE,
                 imageQuality: ONE_POINT_ZERO,
                 skyBackground: DARKEST
               },
               targetEnvironment: {
                 asterism: ${List(tid).asJson}
               },
               ${ObservingModeSetupOperations.SpectroscopyScienceRequirements},
               observingMode: {
                 gmosNorthLongSlit: {
                   grating: R150_G5308
                   fpu: LONG_SLIT_0_75
                   centralWavelength: {
                     nanometers: 540
                   }
                   explicitSpatialOffsets: [
                     { arcseconds: 1.0 },
                     { arcseconds: 2.0 },
                     { arcseconds: 3.0 }
                   ]
                   explicitWavelengthDithers: [
                     { nanometers:  1.0 },
                     { nanometers:  2.0 }
                   ]
                 }
               }
             }
           }) {
             observation {
               id
             }
           }
         }
      """
    ).map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])

  def instrumentConfig(seconds: Int, wavelength: Int): Json =
    json"""
      {
        "exposure" : {
          "seconds" : ${Json.fromBigDecimal(BigDecimal(s"$seconds.000000"))}
        },
        "readout" : {
          "xBin" : "TWO",
          "yBin" : "TWO"
        },
        "roi" : "FULL_FRAME",
        "gratingConfig" : {
          "grating" : "R150_G5308",
          "wavelength" : {
            "nanometers" : ${Json.fromBigDecimal(BigDecimal(s"$wavelength.000"))}
          }
        },
        "filter" : null,
        "fpu" : {
          "builtin" : "LONG_SLIT_0_75"
        },
        "centralWavelength" : {
          "nanometers" : ${Json.fromBigDecimal(BigDecimal(s"$wavelength.000"))}
        }
      }
    """

  def telescopeConfig(q: Int, g: StepGuideState): Json =
    json"""
      {
        "offset" : {
          "p" : {
            "arcseconds" : 0.000000
          },
          "q" : {
            "arcseconds" : ${Json.fromBigDecimal(BigDecimal(s"$q.000000"))}
          }
        },
        "guiding" : ${g.tag.toUpperCase.asJson}
      }
    """

  def arc(seconds: Int, wavelength: Int, q: Int): Json =
    json"""
      {
        "instrumentConfig" : ${instrumentConfig(seconds, wavelength)},
        "stepConfig" : {
          "stepType" : "GCAL",
          "continuum" : null,
          "arcs" : [
            "CU_AR_ARC"
          ]
        },
        "telescopeConfig" : ${telescopeConfig(q, Disabled)},
        "observeClass" : "NIGHT_CAL",
        "breakpoint" : "DISABLED"
      }
    """

  def flat(seconds: Int, wavelength: Int, q: Int): Json =
    json"""
      {
        "instrumentConfig" : ${instrumentConfig(seconds, wavelength)},
        "stepConfig" : {
          "stepType" : "GCAL",
          "continuum" : "QUARTZ_HALOGEN5",
          "arcs" : [
          ]
        },
        "telescopeConfig" : ${telescopeConfig(q, Disabled)},
        "observeClass" : "NIGHT_CAL",
        "breakpoint" : "DISABLED"
      }
    """

  def science(seconds: Int, wavelength: Int, q: Int): Json =
    json"""
      {
        "instrumentConfig" : ${instrumentConfig(seconds, wavelength)},
        "stepConfig" : {
          "stepType" : "SCIENCE"
        },
        "telescopeConfig" : ${telescopeConfig(q, Enabled)},
        "observeClass" : "SCIENCE",
        "breakpoint" : "DISABLED"
      }
    """

  def atom(wavelength: Int, steps: Json*): Json =
    val obsClass = if steps.nonEmpty && steps.forall(j => j.hcursor.downField("observeClass").as[String].contains("NIGHT_CAL")) then "NIGHT_CAL" else "SCIENCE"
    json"""
      {
        "description" : ${s"${wavelength - 540}.000 nm".asJson},
        "observeClass" : ${obsClass.asJson},
        "steps": ${steps.toList.asJson}
      }
    """

  def executionConfig(atoms: Json*): Json =
    json"""
      {
        "executionConfig" : {
          "gmosNorth" : {
            "science" : {
              "nextAtom" : ${atoms.headOption.asJson},
              "possibleFuture": ${atoms.toList.tail.asJson},
              "hasMore": false
            }
          }
        }
      }
    """

  test("initial sequence"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfile(p)
        o <- createGmosNorthLongSlitObservation(p, t)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid),
        expected = executionConfig(
          atom(541,
            arc(seconds = 1, wavelength = 541, q = 1),
            flat(seconds = 1, wavelength = 541, q = 1),
            science(seconds = 6, wavelength = 541, q = 1),
            science(seconds = 6, wavelength = 541, q = 2),
            science(seconds = 6, wavelength = 541, q = 3)
          ),
          atom(542,
            arc(seconds = 1, wavelength = 542, q = 1),
            flat(seconds = 1, wavelength = 542, q = 1),
            science(seconds = 6, wavelength = 542, q = 1),
            science(seconds = 6, wavelength = 542, q = 2),
            science(seconds = 6, wavelength = 542, q = 3)
          )
        ).asRight
      )

  override val ObsWavelength: Wavelength =
    Wavelength.decimalNanometers.unsafeGet(BigDecimal("540.0"))

  override def gmosNorthScience(ditherNm: Int): GmosNorth =
    GmosNorth(
      fakeItcSpectroscopyResult.exposureTime,
      GmosCcdMode(GmosXBinning.Two, GmosYBinning.Two, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Slow),
      GmosDtax.Zero,
      GmosRoi.FullFrame,
      GmosGratingConfig.North(GmosNorthGrating.R150_G5308, GmosGratingOrder.One, obsWavelengthAt(ditherNm)).some,
      none,
      GmosFpuMask.Builtin(GmosNorthFpu.LongSlit_0_75).some
    )

  override def gmosNorthArc(ditherNm: Int): GmosNorth =
    gmosNorthScience(ditherNm).copy(exposure = gn_arc.instrumentConfig.exposureTime)

  override def gmosNorthFlat(ditherNm: Int): GmosNorth =
    gmosNorthScience(ditherNm).copy(exposure = gn_flat.instrumentConfig.exposureTime)

  test("sequence after stopping"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfile(p)
        o <- createGmosNorthLongSlitObservation(p, t)

        v0 <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(1), ArcStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(1), FlatStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(1), StepConfig.Science, sciTelescopeConfig(1), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        s3 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(1), StepConfig.Science, sciTelescopeConfig(2), ObserveClass.Science)
        _  <- addEndStepEvent(s3)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid),
        expected = executionConfig(
          atom(541,
            science(seconds = 6, wavelength = 541, q = 3)
          ),
          atom(542,
            arc(seconds = 1, wavelength = 542, q = 1),
            flat(seconds = 1, wavelength = 542, q = 1),
            science(seconds = 6, wavelength = 542, q = 1),
            science(seconds = 6, wavelength = 542, q = 2),
            science(seconds = 6, wavelength = 542, q = 3)
          )
        ).asRight
      )

  test("sequence on next night"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfile(p)
        o <- createGmosNorthLongSlitObservation(p, t)

        // First night
        v0 <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(1), ArcStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(1), FlatStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(1), StepConfig.Science, sciTelescopeConfig(1), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        s3 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(1), StepConfig.Science, sciTelescopeConfig(2), ObserveClass.Science)
        _  <- addEndStepEvent(s3)

        // Second night -- just do any acquisition step to switch the context.
        // We don't care about the acquisition details.
        v1 <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v1, SequenceType.Acquisition)
        s4 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthAcq(0), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)

        // Now start on the science again.
        a2 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v1, SequenceType.Science)
        s5 <- recordStepAs(serviceUser, a2, Instrument.GmosNorth, gmosNorthScience(1), StepConfig.Science, sciTelescopeConfig(3), ObserveClass.Science)
        _  <- addEndStepEvent(s5)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid),
        expected = executionConfig(
          atom(541,
            arc(seconds = 1, wavelength = 541, q = 3),
            flat(seconds = 1, wavelength = 541, q = 3)
          ),
          atom(542,
            arc(seconds = 1, wavelength = 542, q = 1),
            flat(seconds = 1, wavelength = 542, q = 1),
            science(seconds = 6, wavelength = 542, q = 1),
            science(seconds = 6, wavelength = 542, q = 2),
            science(seconds = 6, wavelength = 542, q = 3)
          )
        ).asRight
      )

  test("sequence after all 541 nm datasets and calibrations"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfile(p)
        o <- createGmosNorthLongSlitObservation(p, t)

        // First night
        v0 <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(1), ArcStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(1), FlatStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(1), StepConfig.Science, sciTelescopeConfig(1), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        s3 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(1), StepConfig.Science, sciTelescopeConfig(2), ObserveClass.Science)
        _  <- addEndStepEvent(s3)

        // Second night -- just do any acquisition step to switch the context.
        // We don't care about the acquisition details.
        v1 <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v1, SequenceType.Acquisition)
        s4 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthAcq(0), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)

        // Now start on the science again and do the calibrations
        a2 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v1, SequenceType.Science)
        s5 <- recordStepAs(serviceUser, a2, Instrument.GmosNorth, gmosNorthScience(1), StepConfig.Science, sciTelescopeConfig(3), ObserveClass.Science)
        _  <- addEndStepEvent(s5)
        s6 <- recordStepAs(serviceUser, a2, Instrument.GmosNorth, gmosNorthArc(1), ArcStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        _  <- addEndStepEvent(s6)
        s7 <- recordStepAs(serviceUser, a2, Instrument.GmosNorth, gmosNorthFlat(1), FlatStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        _  <- addEndStepEvent(s7)

      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid),
        expected = executionConfig(
          atom(542,
            arc(seconds = 1, wavelength = 542, q = 1),
            flat(seconds = 1, wavelength = 542, q = 1),
            science(seconds = 6, wavelength = 542, q = 1),
            science(seconds = 6, wavelength = 542, q = 2),
            science(seconds = 6, wavelength = 542, q = 3)
          )
        ).asRight
      )

  test("sequence after all 541 nm datasets and calibrations + 542 nm calibrations"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfile(p)
        o <- createGmosNorthLongSlitObservation(p, t)

        // First night
        v0 <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(1), ArcStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(1), FlatStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(1), StepConfig.Science, sciTelescopeConfig(1), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        s3 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(1), StepConfig.Science, sciTelescopeConfig(2), ObserveClass.Science)
        _  <- addEndStepEvent(s3)

        // Second night -- just do any acquisition step to switch the context.
        // We don't care about the acquisition details.
        v1 <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v1, SequenceType.Acquisition)
        s4 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthAcq(0), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)

        // Now start on the science again and do the calibrations
        a2 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v1, SequenceType.Science)
        s5 <- recordStepAs(serviceUser, a2, Instrument.GmosNorth, gmosNorthScience(1), StepConfig.Science, sciTelescopeConfig(3), ObserveClass.Science)
        _  <- addEndStepEvent(s5)
        s6 <- recordStepAs(serviceUser, a2, Instrument.GmosNorth, gmosNorthArc(1), ArcStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        _  <- addEndStepEvent(s6)
        s7 <- recordStepAs(serviceUser, a2, Instrument.GmosNorth, gmosNorthFlat(1), FlatStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        _  <- addEndStepEvent(s7)

        a3 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v1, SequenceType.Science)
        s8 <- recordStepAs(serviceUser, a3, Instrument.GmosNorth, gmosNorthArc(2), ArcStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        _  <- addEndStepEvent(s8)
        s9 <- recordStepAs(serviceUser, a3, Instrument.GmosNorth, gmosNorthFlat(2), FlatStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        _  <- addEndStepEvent(s9)

      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid),
        expected = executionConfig(
          atom(542,
            science(seconds = 6, wavelength = 542, q = 1),
            science(seconds = 6, wavelength = 542, q = 2),
            science(seconds = 6, wavelength = 542, q = 3)
          )
        ).asRight
      )

  def nextAtomId(o: Observation.Id): IO[Atom.Id] =
    scienceAtomIds(serviceUser, o).map(_.head)

  test("atom ids"):
    val setup: IO[List[Atom.Id]] =
      for
        p <- createProgram
        t <- createTargetWithProfile(p)
        o <- createGmosNorthLongSlitObservation(p, t)

        // First night
        v0 <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(1), ArcStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(1), FlatStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(1), StepConfig.Science, sciTelescopeConfig(1), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        s3 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(1), StepConfig.Science, sciTelescopeConfig(2), ObserveClass.Science)
        _  <- addEndStepEvent(s3)

        // Second night -- just do any acquisition step to switch the context.
        // We don't care about the acquisition details.
        v1 <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v1, SequenceType.Acquisition)
        s4 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthAcq(0), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)

        // Now start on the science again and do the calibrations
        x0 <- nextAtomId(o)
        a2 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v1, SequenceType.Science)
        x1 <- nextAtomId(o)
        s5 <- recordStepAs(serviceUser, a2, Instrument.GmosNorth, gmosNorthScience(1), StepConfig.Science, sciTelescopeConfig(3), ObserveClass.Science)
        x2 <- nextAtomId(o)
        _  <- addEndStepEvent(s5)
        x3 <- nextAtomId(o)
        s6 <- recordStepAs(serviceUser, a2, Instrument.GmosNorth, gmosNorthArc(1), ArcStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        x4 <- nextAtomId(o)
        _  <- addEndStepEvent(s6)
        x5 <- nextAtomId(o)
        s7 <- recordStepAs(serviceUser, a2, Instrument.GmosNorth, gmosNorthFlat(1), FlatStep, gcalTelescopeConfig(1), ObserveClass.NightCal)
        x6 <- nextAtomId(o)
        _  <- addEndStepEvent(s7)
        x7 <- nextAtomId(o)  // gets a new atom id because we finished the last atom
      yield List(x0, x1, x2, x3, x4, x5, x6, x7)

    assertIOBoolean:
      setup.map: lst =>
        lst.init.distinct.sizeIs == 1 && lst.last != lst.head