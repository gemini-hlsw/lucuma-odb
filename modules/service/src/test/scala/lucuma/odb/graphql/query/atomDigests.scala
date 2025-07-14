// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.math.Offset
import lucuma.core.math.WavelengthDither
import lucuma.core.model.Observation
import lucuma.core.model.sequence.StepConfig
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.json.all.transport.given
import lucuma.odb.sequence.gmos.longslit.Science

class atomDigests extends ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      NonNegInt.unsafeFrom(10)
    )

  def adjustment(Δλnm: Int, qArcsec: Int): Science.Adjustment =
    Science.Adjustment.apply(
      WavelengthDither.intPicometers.get(Δλnm * 1000),
      Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal(qArcsec))
    )

  def atomDigestQuery(oid: Observation.Id): String =
  s"""
    query {
      observation(observationId: "$oid") {
        execution {
          atomDigests {
            state
            value {
              observeClass
              timeEstimate {
                program { seconds }
                nonCharged { seconds }
              }
              stepTypes
              lampTypes
            }
          }
        }
      }
    }
  """

  val fullAtom: Json =
    json"""
      {
        "observeClass": "SCIENCE",
        "timeEstimate": {
          "program": { "seconds": 3892.500000 },
          "nonCharged": { "seconds": 0.000000 }
        },
        "stepTypes": [ "GCAL", "SCIENCE" ],
        "lampTypes": [ "ARC", "FLAT" ]
      }
    """

  val lastAtom: Json =
    json"""
      {
        "observeClass": "SCIENCE",
        "timeEstimate": {
          "program": { "seconds": 1390.300000 },
          "nonCharged": { "seconds": 0.000000 }
        },
        "stepTypes": [ "GCAL", "SCIENCE" ],
        "lampTypes": [ "ARC", "FLAT" ]
      }
    """

  test("pre-execution"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- runObscalcUpdate(p, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = atomDigestQuery(oid),
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "atomDigests": {
                    "state": "READY",
                    "value": [
                      $fullAtom,
                      $fullAtom,
                      $fullAtom,
                      $lastAtom
                    ]
                  }
                }
              }
            }
          """.asRight
      )

  test("mid-execution"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        _  <- runObscalcUpdate(p, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = atomDigestQuery(oid),
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "atomDigests": {
                    "state": "READY",
                    "value": [
                      {
                         "observeClass": "SCIENCE",
                         "timeEstimate": {
                           "program": { "seconds": 2502.200000 },
                           "nonCharged": { "seconds": 0.000000 }
                         },
                         "stepTypes": [ "SCIENCE" ],
                         "lampTypes": []
                      },
                      $fullAtom,
                      $fullAtom,
                      $lastAtom
                    ]
                  }
                }
              }
            }
          """.asRight
      )

  test("pre-calculation"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = atomDigestQuery(oid),
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "atomDigests": {
                    "state": "PENDING",
                    "value": [ ]
                  }
                }
              }
            }
          """.asRight
      )