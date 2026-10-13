// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Step
import munit.Location

// Resetting a customized acquisition is not instrument specific, so GMOS North
// stands in for all of the modes with an acquisition.
class resetAcquisitionCustomized
  extends ExecutionTestSupportForGmos
     with mutation.ReplaceGmosNorthSequenceOps
     with mutation.UpdateObservationsOps:

  /** An acquisition atom as (description, (step id, exposure seconds)*). */
  type AcquisitionAtom = (Option[String], List[(Step.Id, BigDecimal)])

  val EditedExposures: List[Int] = List(30, 40)

  def acquisitionStepInput(seconds: Int): String =
    s"""
      {
        instrumentConfig: {
          exposure: { seconds: $seconds }
          readout: {
            xBin: TWO
            yBin: TWO
            ampCount: TWELVE
            ampGain: LOW
            ampReadMode: FAST
          }
          dtax: ZERO
          roi: CCD2
          filter: G_PRIME
          fpu: { builtin: LONG_SLIT_0_50 }
        }
        stepConfig: { science: true }
        observeClass: ACQUISITION
      }
    """

  def replaceAcquisition(user: User, oid: Observation.Id, seconds: Int*): IO[Unit] =
    val inputString = input(oid, SequenceType.Acquisition, atomInput("Edited", seconds.map(acquisitionStepInput)*))
    query(
      user,
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

  def regenerateAcquisitionAs(user: User, oid: Observation.Id): IO[Unit] =
    query(
      user,
      s"""
        mutation {
          resetAcquisition(input: {
            observationId: ${oid.asJson}
            regenerate: true
          }) {
            observation { id }
          }
        }
      """
    ).void

  def acquisition(oid: Observation.Id): IO[List[AcquisitionAtom]] =
    generateGmosNorthOrFail(pi, oid).map: gn =>
      gn.executionConfig.acquisition.toList.flatMap: s =>
        (s.nextAtom :: s.possibleFuture).map: a =>
          (a.description.map(_.value), a.steps.toList.map(s => (s.id, s.instrumentConfig.exposure.toSeconds)))

  def exposures(atoms: List[AcquisitionAtom]): List[(Option[String], List[BigDecimal])] =
    atoms.map((d, steps) => (d, steps.map(_._2)))

  def stepIds(atoms: List[AcquisitionAtom]): List[Step.Id] =
    atoms.flatMap(_._2.map(_._1))

  val EditedAcquisition: List[(Option[String], List[BigDecimal])] =
    List(("Edited".some, EditedExposures.map(BigDecimal(_))))

  def expectEdited(oid: Observation.Id)(using Location): IO[List[AcquisitionAtom]] =
    acquisition(oid).flatTap(atoms => IO(assertEquals(exposures(atoms), EditedAcquisition)))

  def expectGenerated(oid: Observation.Id)(using Location): IO[Unit] =
    acquisition(oid).map: atoms =>
      assertEquals(atoms.headOption.flatMap(_._1), "Initial Acquisition".some)
      assertEquals(atoms.headOption.map(_._2.size), 3.some)

  def expectCustomized(oid: Observation.Id, customized: Boolean)(using Location): IO[Unit] =
    expect(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            execution { acquisitionSequenceIsCustomized }
          }
        }
      """,
      json"""
        {
          "observation": {
            "execution": { "acquisitionSequenceIsCustomized": ${customized.asJson} }
          }
        }
      """.asRight
    )

  val setup: IO[Observation.Id] =
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _ <- replaceAcquisition(pi, o, EditedExposures*)
    yield o

  test("reset keeps an edited acquisition, with fresh step ids"):
    for
      oid    <- setup
      before <- expectEdited(oid)
      _      <- resetAcquisitionAs(serviceUser, oid)
      after  <- expectEdited(oid)
      _       = assert(stepIds(before).intersect(stepIds(after)).isEmpty)
      _      <- expectCustomized(oid, customized = true)
    yield ()

  test("reset restarts a fully executed edited acquisition from a copy"):
    for
      oid    <- setup
      v      <- recordVisitAs(serviceUser, oid)
      before <- expectEdited(oid)
      _      <- stepIds(before).traverse_(addEndStepEvent(_, v))
      _      <- acquisition(oid).map(atoms => assertEquals(atoms, Nil))
      _      <- resetAcquisitionAs(serviceUser, oid)
      after  <- expectEdited(oid)
      _       = assert(stepIds(before).intersect(stepIds(after)).isEmpty)
    yield ()

  test("repeated resets copy only the latest batch"):
    for
      oid   <- setup
      v     <- recordVisitAs(serviceUser, oid)
      first <- expectEdited(oid)
      _     <- addEndStepEvent(stepIds(first).head, v)
      _     <- resetAcquisitionAs(serviceUser, oid)
      copy  <- expectEdited(oid)
      _     <- stepIds(copy).traverse_(addEndStepEvent(_, v))
      _     <- resetAcquisitionAs(serviceUser, oid)
      _     <- expectEdited(oid)
    yield ()

  test("an edit after partial execution keeps the executed steps in the run"):
    for
      p         <- createProgramAs(pi)
      t         <- createTargetWithProfileAs(pi, p)
      oid       <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v         <- recordVisitAs(serviceUser, oid)
      generated <- acquisition(oid)
      first      = generated.head._2.head
      _         <- addEndStepEvent(first._1, v)
      _         <- replaceAcquisition(serviceUser, oid, EditedExposures*)
      run        = (generated.head._1, List(first._2)) :: EditedAcquisition
      _         <- resetAcquisitionAs(serviceUser, oid)
      copy      <- acquisition(oid)
      _          = assertEquals(exposures(copy), run)
      _         <- stepIds(copy).traverse_(addEndStepEvent(_, v))
      _         <- resetAcquisitionAs(serviceUser, oid)
      _         <- acquisition(oid).map(atoms => assertEquals(exposures(atoms), run))
    yield ()

  test("an edit while a step is ongoing supersedes that step"):
    for
      p         <- createProgramAs(pi)
      t         <- createTargetWithProfileAs(pi, p)
      oid       <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v         <- recordVisitAs(serviceUser, oid)
      generated <- acquisition(oid)
      _         <- addStepEventAs(serviceUser, generated.head._2.head._1, v, StepStage.StartStep)
      _         <- replaceAcquisition(serviceUser, oid, EditedExposures*)
      _         <- resetAcquisitionAs(serviceUser, oid)
      _         <- expectEdited(oid)
    yield ()

  test("deleting the sequence restores the generated acquisition"):
    for
      oid <- setup
      _   <- deleteSequence(oid)
      _   <- resetAcquisitionAs(serviceUser, oid)
      _   <- expectGenerated(oid)
      _   <- expectCustomized(oid, customized = false)
    yield ()

  test("editing the acquisition parameters keeps an edited acquisition"):
    val update = """
      observingMode: {
        gmosNorthLongSlit: {
          acquisition: {
            explicitFilter: I_PRIME
          }
        }
      }
    """
    for
      oid <- setup
      _   <- query(pi, updateObservationsMutation(oid, update, "observations { id }"))
      _   <- expectEdited(oid)
      _   <- expectCustomized(oid, customized = true)
    yield ()

  test("regenerate discards an edited acquisition, even after execution"):
    for
      oid   <- setup
      v     <- recordVisitAs(serviceUser, oid)
      first <- expectEdited(oid)
      _     <- addEndStepEvent(stepIds(first).head, v)
      _     <- regenerateAcquisitionAs(serviceUser, oid)
      _     <- expectGenerated(oid)
      _     <- expectCustomized(oid, customized = false)
    yield ()
