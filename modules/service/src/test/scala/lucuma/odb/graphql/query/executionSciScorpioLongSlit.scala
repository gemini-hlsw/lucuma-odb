// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program

class executionSciScorpioLongSlit extends ExecutionTestSupport:

  private def sec(s: String): BigDecimal =
    BigDecimal(s).setScale(6)

  private def timeAndCount(seconds: Int, count: Int, nm: Int): String =
    s"{ timeAndCount: { time: { seconds: $seconds }, count: $count, at: { nanometers: $nm } } }"

  // The default telescope configs: q = 0, -15, +15, 0 arcsec.
  private val ScorpioInput: String =
    s"""
      scorpioLongSlit: {
        fpu: LONG_SLIT_0_54
        g: ${timeAndCount(300, 2, 469)}
        r: ${timeAndCount(300, 2, 622)}
        i: ${timeAndCount(120, 4, 755)}
        z: ${timeAndCount(120, 4, 889)}
        repeats: 2
      }
    """

  private val ChannelQuery: String =
    "{ exposureTime { seconds } exposureCount readoutMode }"

  private val DynamicQuery: String =
    s"""
      g $ChannelQuery
      r $ChannelQuery
      i $ChannelQuery
      z $ChannelQuery
      fpu
      disperser
    """

  private def channel(seconds: Int, count: Int): Json =
    json"""
      {
        "exposureTime": { "seconds": ${sec(seconds.toString)} },
        "exposureCount": $count,
        "readoutMode": "LOW_NOISE"
      }
    """

  private val ExpectedDynamic: Json =
    json"""
      {
        "g": ${channel(300, 2)},
        "r": ${channel(300, 2)},
        "i": ${channel(120, 4)},
        "z": ${channel(120, 4)},
        "fpu": "LONG_SLIT_0_54",
        "disperser": "VPH"
      }
    """

  private def expectedStep(q: Int): Json =
    json"""
      {
        "instrumentConfig": $ExpectedDynamic,
        "stepConfig": { "stepType": "SCIENCE" },
        "telescopeConfig": {
          "offset": {
            "p": { "arcseconds": 0.000000 },
            "q": { "arcseconds": ${sec(q.toString)} }
          },
          "guiding": "ENABLED"
        },
        "observeClass": "SCIENCE"
      }
    """

  private val ExpectedAtom: Json =
    json"""
      {
        "description": "Nod Cycle",
        "observeClass": "SCIENCE",
        "steps": ${List(0, -15, 15, 0).map(expectedStep)}
      }
    """

  private val AtomQuery: String =
    s"""
      description
      observeClass
      steps {
        instrumentConfig { $DynamicQuery }
        stepConfig { stepType }
        telescopeConfig {
          offset {
            p { arcseconds }
            q { arcseconds }
          }
          guiding
        }
        observeClass
      }
    """

  private def scienceQuery(oid: Observation.Id): String =
    s"""
      query {
        executionConfig(observationId: "$oid") {
          instrument
          scorpio {
            static { channelSelection }
            science {
              nextAtom { $AtomQuery }
              possibleFuture { $AtomQuery }
              hasMore
            }
          }
        }
      }
    """

  private val ExpectedSequence: Json =
    json"""
      {
        "executionConfig": {
          "instrument": "SCORPIO",
          "scorpio": {
            "static": { "channelSelection": "VIS_ONLY" },
            "science": {
              "nextAtom": $ExpectedAtom,
              "possibleFuture": [ $ExpectedAtom ],
              "hasMore": false
            }
          }
        }
      }
    """

  private val createScorpio: IO[(Program.Id, Observation.Id)] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), ScorpioInput)
    yield (p, o)

  test("generation - pre visit"):
    createScorpio.flatMap: (_, o) =>
      expect(pi, scienceQuery(o), ExpectedSequence.asRight)

  test("generation - post visit"):
    for
      (_, o) <- createScorpio
      _      <- recordVisitAs(serviceUser, o)
      _      <- expect(pi, scienceQuery(o), ExpectedSequence.asRight)
    yield ()

  test("generation - no target"):
    for
      p <- createProgram
      o <- createObservationWithModeAs(pi, p, Nil, ScorpioInput)
      _ <- expect(pi, scienceQuery(o), List(s"Could not generate a sequence for $o: observation is missing target").asLeft)
    yield ()

  test("execution - the recorded step and static config are read back"):
    for
      (_, o) <- createScorpio
      v      <- recordVisitAs(serviceUser, o)
      s      <- firstScienceStepId(serviceUser, o)
      _      <- addEndStepEvent(s, v)
      _      <- expect(
                  pi,
                  s"""
                    query {
                      observation(observationId: "$o") {
                        execution {
                          visits {
                            matches {
                              scorpio { channelSelection }
                              atomRecords {
                                matches {
                                  steps {
                                    matches {
                                      index
                                      scorpio { $DynamicQuery }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  """,
                  json"""
                    {
                      "observation": {
                        "execution": {
                          "visits": {
                            "matches": [
                              {
                                "scorpio": { "channelSelection": "VIS_ONLY" },
                                "atomRecords": {
                                  "matches": [
                                    {
                                      "steps": {
                                        "matches": [
                                          {
                                            "index": 1,
                                            "scorpio": $ExpectedDynamic
                                          }
                                        ]
                                      }
                                    }
                                  ]
                                }
                              }
                            ]
                          }
                        }
                      }
                    }
                  """.asRight
                )
    yield ()

  test("a clone of a materialized observation has the same sequence"):
    for
      (_, o) <- createScorpio
      _      <- recordVisitAs(serviceUser, o)
      c      <- cloneObservationAs(pi, o)
      _      <- expect(pi, scienceQuery(c), ExpectedSequence.asRight)
    yield ()

  // Per step the g and r channels dominate: 2 x (300 + 20.6 readout + 10 write)
  // = 661.2 s, against 4 x (120 + 20.6 + 10) = 602.4 s for i and z.  A nod cycle
  // adds three offsets of 15, 30 and 15 arcsec at 7 s + 0.00625 s/arcsec.
  test("time estimate"):
    val cycle = sec("661.2") * 4 + sec("7.09375") * 2 + sec("7.1875")

    for
      (p, o) <- createScorpio
      _      <- runObscalcUpdate(p, o)
      _      <- expect(
                  pi,
                  s"""
                    query {
                      observation(observationId: "$o") {
                        execution {
                          digest {
                            value {
                              setup {
                                full { seconds }
                                reacquisition { seconds }
                              }
                              science {
                                observeClass
                                timeEstimate { total { seconds } }
                                atomCount
                              }
                            }
                          }
                        }
                      }
                    }
                  """,
                  json"""
                    {
                      "observation": {
                        "execution": {
                          "digest": {
                            "value": {
                              "setup": {
                                "full": { "seconds": 960.000000 },
                                "reacquisition": { "seconds": 300.000000 }
                              },
                              "science": {
                                "observeClass": "SCIENCE",
                                "timeEstimate": { "total": { "seconds": ${(cycle * 2).asJson} } },
                                "atomCount": 2
                              }
                            }
                          }
                        }
                      }
                    }
                  """.asRight
                )
    yield ()

  test("workflow - no ITC result is required"):
    for
      (p, o) <- createScorpio
      _      <- runObscalcUpdate(p, o)
      _      <- expect(
                  pi,
                  s"""
                    query {
                      observation(observationId: "$o") {
                        workflow {
                          value {
                            state
                            validationErrors { messages }
                          }
                        }
                      }
                    }
                  """,
                  json"""
                    {
                      "observation": {
                        "workflow": {
                          "value": {
                            "state": "DEFINED",
                            "validationErrors": []
                          }
                        }
                      }
                    }
                  """.asRight
                )
    yield ()
