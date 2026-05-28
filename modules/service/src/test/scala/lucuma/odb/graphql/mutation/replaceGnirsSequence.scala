// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import io.circe.literal.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation

class replaceGnirsSequence extends query.ExecutionTestSupportForGnirs with ReplaceSequenceOps:
  def stepInput(exposureSeconds: BigDecimal): String =
    s"""
          {
            instrumentConfig: {
              exposure: { seconds: $exposureSeconds }
              coadds: 2
              centralWavelength: { nanometers: 2200 }
              filter: H2
              decker: LONG_CAM_LONG_SLIT
              fpuSlit: LONG_SLIT_0_10
              camera: SHORT_BLUE
              readMode: VERY_BRIGHT
            }
            stepConfig: {
              science: true
            }
            observeClass: SCIENCE
          }
    """

  test("Simple one atom, one step"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsLongSlitObservationAs(pi, p, t)
      yield o

    setup.flatMap: oid =>
      val inputString = input(oid, SequenceType.Science, atomInput("Foo", stepInput(20)))
      expect(
        user     = pi,
        query    = s"""
          mutation {
            replaceGnirsSequence(input: $inputString) {
              sequence {
                description
                steps {
                  instrumentConfig {
                    exposure { seconds }
                    coadds
                    filter
                    fpuSlit
                    camera
                    readMode
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "replaceGnirsSequence": {
              "sequence": [
                {
                  "description": "Foo",
                  "steps": [
                    {
                      "instrumentConfig": {
                        "exposure": { "seconds": 20.000000 },
                        "coadds": 2,
                        "filter": "H2",
                        "fpuSlit": "LONG_SLIT_0_10",
                        "camera": "SHORT_BLUE",
                        "readMode": "VERY_BRIGHT"
                      }
                    }
                  ]
                }
              ]
            }
          }
        """.asRight
      )

  test("Empty"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsLongSlitObservationAs(pi, p, t)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = s"""
          mutation {
            replaceGnirsSequence(input: {
              observationId: "$oid"
              sequenceType: SCIENCE
              sequence: []
            }) {
              sequence {
                description
              }
            }
          }
        """,
        expected = json"""
          {
            "replaceGnirsSequence": {
              "sequence": []
            }
          }
        """.asRight
      )

  test("Matches execution config (before first visit)"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsLongSlitObservationAs(pi, p, t)
      yield o

    assertIOBoolean:
      for
        o  <- setup
        in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(20)))
        i0 <- query(pi, mutation(Instrument.Gnirs, in)).map(mutationOutput(Instrument.Gnirs, _))
        i1 <- scienceSequenceIds(pi, o).map(_.toList)
      yield i0 === i1

  test("Matches execution config (after first visit)"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsLongSlitObservationAs(pi, p, t)
        _ <- recordVisitAs(serviceUser, o)
      yield o

    assertIOBoolean:
      for
        o  <- setup
        in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(20)))
        i0 <- query(pi, mutation(Instrument.Gnirs, in)).map(mutationOutput(Instrument.Gnirs, _))
        i1 <- scienceSequenceIds(pi, o).map(_.toList)
      yield i0 === i1

  test("PI cannot edit the sequence after execution starts"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsLongSlitObservationAs(pi, p, t)
        v <- recordVisitAs(serviceUser, o)
        s <- firstScienceStepId(serviceUser, o)
        _ <- addEndStepEvent(s, v)
      yield o

    for
      o  <- setup
      in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(20)))
      _  <- expect(
        pi,
        mutation(Instrument.Gnirs, in),
        List(
          s"Observation $o is ineligible for this operation due to its workflow state (Ongoing with allowed transition to Inactive/Completed).",
          "User cannot replace the sequence in the current observation workflow state."
        ).asLeft
      )
    yield ()

  test("Staff can edit the sequence after execution starts"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsLongSlitObservationAs(pi, p, t)
        v <- recordVisitAs(serviceUser, o)
        s <- firstScienceStepId(serviceUser, o)
        _ <- addEndStepEvent(s, v)
      yield o

    assertIOBoolean:
      for
        o  <- setup
        in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(20)))
        r  <- query(staff, mutation(Instrument.Gnirs, in))
      yield mutationOutput(Instrument.Gnirs, r).nonEmpty

  test("Can't add too many atoms"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsLongSlitObservationAs(pi, p, t)
      yield o

    for
      o  <- setup
      in  = input(o, SequenceType.Science, List.fill(1001)(atomInput("Foo", stepInput(20)))*)
      _  <- expect(pi, mutation(Instrument.Gnirs, in), List(
        "Execution sequences containing over 1000 atoms are not supported."
      ).asLeft)
    yield ()
