// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.sequence.Atom
import lucuma.odb.json.gmos.given
import lucuma.odb.json.time.transport.given
import lucuma.odb.json.wavelength.transport.given

class executionNighttimeArcs extends ExecutionTestSupport {

  val GmosNorthLongSlit: String =
    """
      gmosNorthLongSlit: {
        grating: R831_G5302,
        filter:  R_PRIME,
        fpu:     LONG_SLIT_0_50,
        centralWavelength: { nanometers: 500 },
        explicitYBin: TWO
      }
    """

  def query(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          execution {
            config {
              gmosNorth {
                science {
                  nextAtom {
                    description
                    steps {
                      stepConfig {
                        stepType
                        ... on Gcal {
                          continuum
                          arcs
                        }
                      }
                    }
                  }
                  possibleFuture {
                    description
                    steps {
                      stepConfig {
                        stepType
                        ... on Gcal {
                          continuum
                          arcs
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    """

  val arc0: Json =
    json"""
      {
        "description": "Arc: q 0.0″, λ 500.0 nm",
        "steps": [
          {
            "stepConfig": {
              "stepType": "GCAL",
              "continuum": null,
              "arcs": [ "CU_AR_ARC" ]
            }
          }
        ]
      }
    """

  val arc1: Json =
    json"""
      {
        "description": "Arc: q 15.0″, λ 505.0 nm",
        "steps": [
          {
            "stepConfig": {
              "stepType": "GCAL",
              "continuum": null,
              "arcs": [ "CU_AR_ARC" ]
            }
          }
        ]
      }
    """

  val fullSequence: List[Json] =
    json"""
      [
        {
          "description": "q 0.0″, λ 500.0 nm",
          "steps": [
            {
              "stepConfig": {
                "stepType": "SCIENCE"
              }
            },
            {
              "stepConfig": {
                "stepType": "GCAL",
                "continuum": "QUARTZ_HALOGEN5",
                "arcs": []
              }
            }
          ]
        },
        {
          "description": "q 15.0″, λ 505.0 nm",
          "steps": [
            {
              "stepConfig": {
                "stepType": "GCAL",
                "continuum": "QUARTZ_HALOGEN5",
                "arcs": []
              }
            },
            {
              "stepConfig": {
                "stepType": "SCIENCE"
              }
            }
          ]
        },
        {
          "description": "q 15.0″, λ 505.0 nm",
          "steps": [
            {
              "stepConfig": {
                "stepType": "SCIENCE"
              }
            },
            {
              "stepConfig": {
                "stepType": "GCAL",
                "continuum": "QUARTZ_HALOGEN5",
                "arcs": []
              }
            }
          ]
        },
        {
          "description": "q 0.0″, λ 500.0 nm",
          "steps": [
            {
              "stepConfig": {
                "stepType": "GCAL",
                "continuum": "QUARTZ_HALOGEN5",
                "arcs": []
              }
            },
            {
              "stepConfig": {
                "stepType": "SCIENCE"
              }
            }
          ]
        },
        {
          "description": "q 0.0″, λ 500.0 nm",
          "steps": [
            {
              "stepConfig": {
                "stepType": "SCIENCE"
              }
            },
            {
              "stepConfig": {
                "stepType": "GCAL",
                "continuum": "QUARTZ_HALOGEN5",
                "arcs": []
              }
            }
          ]
        },
        {
          "description": "q 15.0″, λ 505.0 nm",
          "steps": [
            {
              "stepConfig": {
                "stepType": "GCAL",
                "continuum": "QUARTZ_HALOGEN5",
                "arcs": []
              }
            },
            {
              "stepConfig": {
                "stepType": "SCIENCE"
              }
            }
          ]
        }
      ]
  """.asArray.get.toList

  test("nothing yet exected") {

    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), GmosNorthLongSlit)
      } yield o

    setup.flatMap { oid =>
      expect(
        user     = pi,
        query    = query(oid),
        expected = json"""
          {
            "observation": {
              "execution": {
                "config": {
                  "gmosNorth": {
                    "science": {
                      "nextAtom": ${fullSequence.head},
                      "possibleFuture": ${arc0 :: arc1 :: fullSequence.tail}
                    }
                  }
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("execute first atom (but not its arc)") {

    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), GmosNorthLongSlit)

        // Execute the first atom in visit 0
        v0 <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, stepCount = 2)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthScience0, ScienceP00Q00)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthFlat0, Flat)
        _  <- addEndStepEvent(s1)
      } yield o

    setup.flatMap { oid =>
      expect(
        user     = pi,
        query    = query(oid),
        expected = json"""
          {
            "observation": {
              "execution": {
                "config": {
                  "gmosNorth": {
                    "science": {
                      "nextAtom": $arc0,
                      "possibleFuture": ${arc1 :: fullSequence.tail}
                    }
                  }
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("execute first atom (and its arc)") {

    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), GmosNorthLongSlit)

        // Execute the first atom in visit 0
        v0 <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, stepCount = 2)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthScience0, ScienceP00Q00)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthFlat0, Flat)
        _  <- addEndStepEvent(s1)

        // Execute the arc
        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0)
        s2 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, GmosNorthFlat0, Arc)
        _  <- addEndStepEvent(s2)
      } yield o

    setup.flatMap { oid =>
      expect(
        user     = pi,
        query    = query(oid),
        expected = json"""
          {
            "observation": {
              "execution": {
                "config": {
                  "gmosNorth": {
                    "science": {
                      "nextAtom": $arc1,
                      "possibleFuture": ${fullSequence.tail}
                    }
                  }
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("execute first atom (and its arc) in previous visit") {

    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), GmosNorthLongSlit)

        // Execute the first atom in visit 0
        v0 <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, stepCount = 2)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthScience0, ScienceP00Q00)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthFlat0, Flat)
        _  <- addEndStepEvent(s1)

        // Execute the arc
        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0)
        s2 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, GmosNorthFlat0, Arc)
        _  <- addEndStepEvent(s2)

        // Start a new visit (which will necesitate another arc)
        _ <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
      } yield o

    val sci505_FS :: sci505_SF :: rest = fullSequence.tail : @unchecked

    setup.flatMap { oid =>
      expect(
        user     = pi,
        query    = query(oid),
        expected = json"""
          {
            "observation": {
              "execution": {
                "config": {
                  "gmosNorth": {
                    "science": {
                      "nextAtom": $arc1,
                      "possibleFuture": ${sci505_FS :: sci505_SF :: arc0 :: rest}
                    }
                  }
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("execute first atom (but not its arc) in previous visit") {

    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), GmosNorthLongSlit)

        // Execute the first atom in visit 0
        v0 <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, stepCount = 2)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthScience0, ScienceP00Q00)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthFlat0, Flat)
        _  <- addEndStepEvent(s1)

        // Start a new visit (which will necesitate redoing the first step)
        _ <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
      } yield o

    setup.flatMap { oid =>
      expect(
        user     = pi,
        query    = query(oid),
        expected = json"""
          {
            "observation": {
              "execution": {
                "config": {
                  "gmosNorth": {
                    "science": {
                      "nextAtom": ${fullSequence.head},
                      "possibleFuture": ${arc0 :: arc1 :: fullSequence.tail}
                    }
                  }
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  test("execute just the λ 500 atoms in previous visit") {

    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), GmosNorthLongSlit)

        // Execute the first atom (λ 500 Science/Flat) in visit 0
        v0 <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, stepCount = 2)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthScience0, ScienceP00Q00)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthFlat0, Flat)
        _  <- addEndStepEvent(s1)

        // Execute the λ 500 arc in visit 0
        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0)
        s2 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, GmosNorthFlat0, Arc)
        _  <- addEndStepEvent(s2)

        // Execute (λ 500 Flat/Science) in visit 0
        a2 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, stepCount = 2)
        s3 <- recordStepAs(serviceUser, a2, Instrument.GmosNorth, GmosNorthFlat0, Flat)
        _  <- addEndStepEvent(s3)
        s4 <- recordStepAs(serviceUser, a2, Instrument.GmosNorth, GmosNorthScience0, ScienceP00Q00)
        _  <- addEndStepEvent(s4)

        // Execute the remaining (λ 500 Science/Flat) in visit 0
        a3 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, stepCount = 2)
        s5 <- recordStepAs(serviceUser, a3, Instrument.GmosNorth, GmosNorthScience0, ScienceP00Q00)
        _  <- addEndStepEvent(s5)
        s6 <- recordStepAs(serviceUser, a3, Instrument.GmosNorth, GmosNorthFlat0, Flat)
        _  <- addEndStepEvent(s6)

        // Start a new visit (which will contain only λ 505 and its arc)
        _ <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

      } yield o

    val remaining = fullSequence.filter(_.spaces2.indexOf("λ 505") > 0)

    setup.flatMap { oid =>
      expect(
        user     = pi,
        query    = query(oid),
        expected = json"""
          {
            "observation": {
              "execution": {
                "config": {
                  "gmosNorth": {
                    "science": {
                      "nextAtom": $arc1,
                      "possibleFuture": $remaining
                    }
                  }
                }
              }
            }
          }
        """.asRight
      )
    }
  }

  def atomIds(oid: Observation.Id): IO[List[(Atom.Id, String)]] =
    query(
      user  = pi,
      query = s"""
        query {
          observation(observationId: "$oid") {
            execution {
              config {
                gmosNorth {
                  science {
                    nextAtom {
                      id
                      description
                    }
                    possibleFuture {
                      id
                      description
                    }
                  }
                }
              }
            }
          }
        }
      """
    ).map { json =>
      val science  = json.hcursor.downFields("observation", "execution", "config", "gmosNorth", "science")
      def pair(c: io.circe.ACursor): (Atom.Id, String) =
        (c.downField("id").require[Atom.Id], c.downField("description").require[String])
      val head     = pair(science.downField("nextAtom"))
      val tail     = science.downFields("possibleFuture").values.get.toList.map(j => pair(j.hcursor))
      head :: tail
    }

  test("id don't change") {
    for {
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), GmosNorthLongSlit)

      v0   <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
      ids0 <- atomIds(o).map(_.filter(_._2.indexOf("λ 505") > 0))

      // Execute the first atom (λ 500 Science/Flat) in visit 0
      a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, stepCount = 2)
      s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthScience0, ScienceP00Q00)
      _  <- addEndStepEvent(s0)
      s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthFlat0, Flat)
      _  <- addEndStepEvent(s1)

      // Execute the λ 500 arc in visit 0
      a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0)
      s2 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, GmosNorthFlat0, Arc)
      _  <- addEndStepEvent(s2)

      // Execute (λ 500 Flat/Science) in visit 0
      a2 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, stepCount = 2)
      s3 <- recordStepAs(serviceUser, a2, Instrument.GmosNorth, GmosNorthFlat0, Flat)
      _  <- addEndStepEvent(s3)
      s4 <- recordStepAs(serviceUser, a2, Instrument.GmosNorth, GmosNorthScience0, ScienceP00Q00)
      _  <- addEndStepEvent(s4)

      // Execute the remaining (λ 500 Science/Flat) in visit 0
      a3 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, stepCount = 2)
      s5 <- recordStepAs(serviceUser, a3, Instrument.GmosNorth, GmosNorthScience0, ScienceP00Q00)
      _  <- addEndStepEvent(s5)
      s6 <- recordStepAs(serviceUser, a3, Instrument.GmosNorth, GmosNorthFlat0, Flat)
      _  <- addEndStepEvent(s6)

      ids1 <- atomIds(o)
    } yield assertEquals(ids1.toSet, ids0.toSet)
  }
}