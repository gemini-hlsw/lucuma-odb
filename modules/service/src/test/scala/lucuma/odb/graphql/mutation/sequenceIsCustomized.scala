// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import munit.Location

// Customization tracking is not instrument specific, so GmosNorth stands in for
// all of the replace*Sequence mutations.
class sequenceIsCustomized extends query.ExecutionTestSupportForGmos with ReplaceGmosNorthSequenceOps:

  def expectSequenceFlags(
    o:            Observation.Id,
    acqMat:       Boolean,
    sciMat:       Boolean,
    acqCustom:    Boolean,
    sciCustom:    Boolean
  )(using Location): IO[Unit] =
    expect(
      pi,
      s"""
        query {
          observation(observationId: ${o.asJson}) {
            execution {
              acquisitionSequenceIsMaterialized
              scienceSequenceIsMaterialized
              acquisitionSequenceIsCustomized
              scienceSequenceIsCustomized
            }
          }
        }
      """,
      json"""
        {
          "observation": {
            "execution": {
              "acquisitionSequenceIsMaterialized": ${acqMat.asJson},
              "scienceSequenceIsMaterialized": ${sciMat.asJson},
              "acquisitionSequenceIsCustomized": ${acqCustom.asJson},
              "scienceSequenceIsCustomized": ${sciCustom.asJson}
            }
          }
        }
      """.asRight
    )

  def replaceSequence(oid: Observation.Id, sequenceType: SequenceType): IO[Unit] =
    val inputString = input(oid, sequenceType, atomInput("Foo", stepInput(GmosNorthFilter.GPrime)))
    query(
      pi,
      s"""
        mutation {
          replaceGmosNorthSequence(input: $inputString) {
            sequence { description }
          }
        }
      """
    ).void

  def deleteSequence(oid: Observation.Id): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          deleteSequence(input: { observationId: ${oid.asJson} }) {
            observation { id }
          }
        }
      """
    ).void

  val setup: IO[Observation.Id] =
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
    yield o

  test("new observation is neither materialized nor customized"):
    setup.flatMap: oid =>
      expectSequenceFlags(oid, acqMat = false, sciMat = false, acqCustom = false, sciCustom = false)

  test("materialization by execution does not customize"):
    for
      oid <- setup
      _   <- recordVisitAs(serviceUser, oid)
      _   <- expectSequenceFlags(oid, acqMat = true, sciMat = true, acqCustom = false, sciCustom = false)
    yield ()

  test("replacing the science sequence customizes it"):
    for
      oid <- setup
      _   <- replaceSequence(oid, SequenceType.Science)
      _   <- expectSequenceFlags(oid, acqMat = false, sciMat = true, acqCustom = false, sciCustom = true)
    yield ()

  test("replacing the acquisition sequence customizes it"):
    for
      oid <- setup
      _   <- replaceSequence(oid, SequenceType.Acquisition)
      _   <- expectSequenceFlags(oid, acqMat = true, sciMat = false, acqCustom = true, sciCustom = false)
    yield ()

  test("replacing after materialization by execution customizes"):
    for
      oid <- setup
      _   <- recordVisitAs(serviceUser, oid)
      _   <- replaceSequence(oid, SequenceType.Science)
      _   <- expectSequenceFlags(oid, acqMat = true, sciMat = true, acqCustom = false, sciCustom = true)
    yield ()

  test("deleting the sequence clears customization"):
    for
      oid <- setup
      _   <- replaceSequence(oid, SequenceType.Acquisition)
      _   <- replaceSequence(oid, SequenceType.Science)
      _   <- expectSequenceFlags(oid, acqMat = true, sciMat = true, acqCustom = true, sciCustom = true)
      _   <- deleteSequence(oid)
      _   <- expectSequenceFlags(oid, acqMat = false, sciMat = false, acqCustom = false, sciCustom = false)
    yield ()

  test("resetting the acquisition clears acquisition customization only"):
    for
      oid <- setup
      _   <- replaceSequence(oid, SequenceType.Acquisition)
      _   <- replaceSequence(oid, SequenceType.Science)
      _   <- resetAcquisitionAs(serviceUser, oid)
      _   <- expectSequenceFlags(oid, acqMat = true, sciMat = true, acqCustom = false, sciCustom = true)
    yield ()
