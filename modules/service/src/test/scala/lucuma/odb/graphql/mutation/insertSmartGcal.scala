// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SmartGcalType
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.util.TimeSpan
import lucuma.odb.data.OdbError
import munit.Location

class insertSmartGcal extends query.ExecutionTestSupportForGmos with ReplaceGmosNorthSequenceOps:

  val setup: IO[(Program.Id, Observation.Id)] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
    yield (p, o)

  def insertQuery(
    oid:          Observation.Id,
    types:        List[SmartGcalType],
    after:        Option[Step.Id]      = None,
    sequenceType: Option[SequenceType] = None
  ): String =
    val afterField  = after.foldMap(sid => s"afterStepId: ${sid.asJson}")
    val seqField    = sequenceType.foldMap(st => s"sequenceType: ${st.tag.toUpperCase}")
    s"""
      mutation {
        insertSmartGcal(input: {
          observationId: ${oid.asJson}
          smartGcalTypes: ${types.map(_.tag.toUpperCase).mkString("[", ", ", "]")}
          $afterField
          $seqField
        }) {
          observation { id }
        }
      }
    """

  def insertAs(
    user:         User,
    oid:          Observation.Id,
    types:        List[SmartGcalType],
    after:        Option[Step.Id]      = None,
    sequenceType: Option[SequenceType] = None
  )(using Location): IO[Unit] =
    expect(
      user,
      insertQuery(oid, types, after, sequenceType),
      json"""
        {
          "insertSmartGcal": {
            "observation": { "id": $oid }
          }
        }
      """.asRight
    )

  def insertFails(
    user:   User,
    oid:    Observation.Id,
    types:  List[SmartGcalType],
    after:  Option[Step.Id],
    errors: List[String]
  )(using Location): IO[Unit] =
    expect(user, insertQuery(oid, types, after), errors.asLeft)

  // The remaining (unexecuted or executing) science sequence.
  def science(oid: Observation.Id): IO[List[(Atom.Id, List[Step[GmosNorth]])]] =
    generateGmosNorthOrFail(serviceUser, oid).map: gn =>
      gn.executionConfig.science.toList.flatMap: s =>
        (s.nextAtom :: s.possibleFuture).map(a => a.id -> a.steps.toList)

  def ids(seq: List[(Atom.Id, List[Step[GmosNorth]])]): List[(Atom.Id, List[Step.Id])] =
    seq.map((aid, steps) => aid -> steps.map(_.id))

  // The new steps are the ones whose ids were not there before.
  def inserted(
    before: List[(Atom.Id, List[Step[GmosNorth]])],
    after:  List[(Atom.Id, List[Step[GmosNorth]])]
  ): List[Step[GmosNorth]] =
    val old = before.flatMap(_._2.map(_.id)).toSet
    after.flatMap(_._2).filterNot(s => old.contains(s.id))

  // Expected configuration of a GCAL step based on `ref`.
  def assertGcalStep(step: Step[GmosNorth], ref: Step[GmosNorth], arc: Boolean)(using Location): Unit =
    val (gcal, exp) =
      if arc then (ArcStep,  gmos_arc.instrumentConfig.exposureTime)
      else        (FlatStep, gmos_flat.instrumentConfig.exposureTime)
    assertEquals(step.stepConfig, gcal)
    assertEquals(step.instrumentConfig, ref.instrumentConfig.copy(exposure = exp))
    assertEquals(step.telescopeConfig.offset, ref.telescopeConfig.offset)
    assertEquals(step.telescopeConfig.guiding, StepGuideState.Disabled)
    assertEquals(step.observeClass, ObserveClass.NightCal)
    assert(step.estimate.total > TimeSpan.Zero)

  def expectSequenceFlags(
    o:         Observation.Id,
    acqMat:    Boolean,
    sciMat:    Boolean,
    sciCustom: Boolean
  )(using Location): IO[Unit] =
    expect(
      pi,
      s"""
        query {
          observation(observationId: ${o.asJson}) {
            execution {
              acquisitionSequenceIsMaterialized
              scienceSequenceIsMaterialized
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
              "scienceSequenceIsCustomized": ${sciCustom.asJson}
            }
          }
        }
      """.asRight
    )

  def workflowState(p: Program.Id, o: Observation.Id): IO[ObservationWorkflowState] =
    runObscalcUpdate(p, o) *> queryObservationWorkflowState(pi, o)

  def executeAll(steps: List[Step[GmosNorth]], v: Visit.Id): IO[Unit] =
    steps.traverse_(s => addEndStepEvent(s.id, v))

  test("before execution: materializes only the science sequence, inserting before the first step"):
    for
      (_, o) <- setup
      seq0   <- science(o)
      _      <- expectSequenceFlags(o, acqMat = false, sciMat = false, sciCustom = false)
      _      <- insertAs(pi, o, List(SmartGcalType.Arc))
      _      <- expectSequenceFlags(o, acqMat = false, sciMat = true, sciCustom = true)
      seq1   <- science(o)
    yield
      val ref = seq0.head._2.head
      val arc = seq1.head._2.head
      // based on the first step, and the generated ids are kept
      assertGcalStep(arc, ref, arc = true)
      assertEquals(ids(seq1), (seq0.head._1, arc.id :: seq0.head._2.map(_.id)) :: ids(seq0.tail))

  test("flat and arc are inserted in the order given"):
    for
      (_, o) <- setup
      seq0   <- science(o)
      _      <- insertAs(pi, o, List(SmartGcalType.Flat, SmartGcalType.Arc))
      seq1   <- science(o)
    yield
      val ref = seq0.head._2.head
      inserted(seq0, seq1) match
        case List(f, a) =>
          assertGcalStep(f, ref, arc = false)
          assertGcalStep(a, ref, arc = true)
          assertEquals(seq1.head._2.take(2).map(_.id), List(f.id, a.id))
        case other      =>
          fail(s"Expected a flat and an arc, got $other")

  test("after an unexecuted step in the middle of an atom"):
    for
      (_, o) <- setup
      seq0   <- science(o)
      (aid, steps) = seq0.head
      _      <- insertAs(pi, o, List(SmartGcalType.Arc), steps(0).id.some)
      seq1   <- science(o)
    yield
      val arc = inserted(seq0, seq1).head
      assertGcalStep(arc, steps(0), arc = true)
      assertEquals(ids(seq1).head, aid -> (steps(0).id :: arc.id :: steps.tail.map(_.id)))

  test("after the last step of an unexecuted atom: extends that atom"):
    for
      (_, o) <- setup
      seq0   <- science(o)
      _       = assert(seq0.size > 1, "expecting multiple atoms")
      (a0, s0) = seq0(0)
      (a1, s1) = seq0(1)
      _      <- insertAs(pi, o, List(SmartGcalType.Arc), s0.last.id.some)
      seq1   <- science(o)
    yield
      val arc = inserted(seq0, seq1).head
      assertGcalStep(arc, s0.last, arc = true)
      assertEquals(ids(seq1).take(2), List(a0 -> (s0.map(_.id) :+ arc.id), a1 -> s1.map(_.id)))

  test("mid-atom, same visit: after the last executed step, in its atom"):
    for
      (_, o) <- setup
      v      <- recordVisitAs(serviceUser, o)
      seq0   <- science(o)
      (aid, steps) = seq0.head
      _      <- addEndStepEvent(steps(0).id, v)
      _      <- insertAs(staff, o, List(SmartGcalType.Arc))
      seq1   <- science(o)
    yield
      val arc = inserted(seq0, seq1).head
      assertGcalStep(arc, steps(0), arc = true)
      assertEquals(ids(seq1).head, aid -> (arc.id :: steps.tail.map(_.id)))

  test("completed atom, same visit: extends the completed atom"):
    for
      (_, o) <- setup
      v      <- recordVisitAs(serviceUser, o)
      seq0   <- science(o)
      (a0, s0) = seq0.head
      _      <- executeAll(s0, v)
      _      <- insertAs(staff, o, List(SmartGcalType.Arc), s0.last.id.some)
      seq1   <- science(o)
    yield
      val arc = inserted(seq0, seq1).head
      assertGcalStep(arc, s0.last, arc = true)
      assertEquals(ids(seq1), (a0 -> List(arc.id)) :: ids(seq0.tail))

  test("completed atom, earlier visit: a new atom of its own"):
    for
      (_, o) <- setup
      v1     <- recordVisitAs(serviceUser, o)
      seq0   <- science(o)
      (a0, s0) = seq0.head
      _      <- executeAll(s0, v1)
      _      <- recordVisitAs(serviceUser, o)
      _      <- insertAs(staff, o, List(SmartGcalType.Arc))
      seq1   <- science(o)
    yield
      val arc = inserted(seq0, seq1).head
      assertGcalStep(arc, s0.last, arc = true)
      assert(!seq0.map(_._1).contains(seq1.head._1), "expected a new atom")
      assertEquals(ids(seq1).tail, ids(seq0.tail))
      assertEquals(seq1.head._2.map(_.id), List(arc.id))

  test("an executed atom is moved ahead of atoms renumbered by a replace"):
    for
      (_, o) <- setup
      v      <- recordVisitAs(serviceUser, o)
      seq0   <- science(o)
      (a0, s0) = seq0.head
      _      <- executeAll(s0, v)
      in      = input(o, SequenceType.Science, atomInput("A", stepInput(GmosNorthFilter.GPrime)), atomInput("B", stepInput(GmosNorthFilter.GPrime)))
      rep    <- query(staff, mutation(Instrument.GmosNorth, in)).map(mutationOutput(Instrument.GmosNorth, _))
      _      <- insertAs(staff, o, List(SmartGcalType.Arc))
      seq1   <- science(o)
    yield
      val arc = inserted(seq0, seq1).filterNot(s => rep.flatMap(_._2).contains(s.id)).head
      assertGcalStep(arc, s0.last, arc = true)
      assertEquals(ids(seq1), (a0 -> List(arc.id)) :: rep)

  test("while a step is executing: inserted after it, leaving it executing"):
    for
      (_, o) <- setup
      v      <- recordVisitAs(serviceUser, o)
      seq0   <- science(o)
      (aid, steps) = seq0.head
      _      <- addStepEventAs(serviceUser, steps(0).id, v, StepStage.StartStep)
      _      <- insertAs(serviceUser, o, List(SmartGcalType.Arc))
      seq1   <- science(o)
      _      <- addStepEventAs(serviceUser, steps(0).id, v, StepStage.EndStep)
      seq2   <- science(o)
    yield
      val arc = inserted(seq0, seq1).head
      assertGcalStep(arc, steps(0), arc = true)
      assertEquals(ids(seq1).head, aid -> (steps(0).id :: arc.id :: steps.tail.map(_.id)))
      assertEquals(ids(seq2).head, aid -> (arc.id :: steps.tail.map(_.id)))

  test("afterStepId may not refer to a step executed before the most recent one"):
    for
      (_, o) <- setup
      v      <- recordVisitAs(serviceUser, o)
      seq0   <- science(o)
      steps   = seq0.head._2
      _      <- addEndStepEvent(steps(0).id, v)
      _      <- addEndStepEvent(steps(1).id, v)
      _      <- insertFails(
                  staff,
                  o,
                  List(SmartGcalType.Arc),
                  steps(0).id.some,
                  List(s"Step ${steps(0).id} is not the most recently executed step, so nothing may be inserted after it.")
                )
    yield ()

  test("afterStepId may refer to the executing step"):
    for
      (_, o) <- setup
      v      <- recordVisitAs(serviceUser, o)
      s      <- firstScienceStepId(serviceUser, o)
      _      <- addEndStepEvent(s, v)
      seq0   <- science(o)
      t       = seq0.head._2.head
      _      <- addStepEventAs(serviceUser, t.id, v, StepStage.StartStep)
      _      <- insertAs(staff, o, List(SmartGcalType.Arc), t.id.some)
      seq1   <- science(o)
    yield
      val arc = inserted(seq0, seq1).head
      assertGcalStep(arc, t, arc = true)
      assertEquals(seq1.head._2.take(2).map(_.id), List(t.id, arc.id))

  test("afterStepId must be part of the sequence, and a failure leaves the sequence unmaterialized"):
    for
      (_, o) <- setup
      s      <- firstAcquisitionStepId(serviceUser, o)
      _      <- insertFails(pi, o, List(SmartGcalType.Arc), s.some, List(s"Step $s is not part of the sequence."))
      _      <- expectSequenceFlags(o, acqMat = false, sciMat = false, sciCustom = false)
    yield ()

  test("acquisition sequence: a missing smart GCAL mapping is an error, leaving the sequence unmaterialized"):
    // The test data has GMOS smart GCAL mappings for spectroscopy only, which
    // the GMOS imaging acquisition configuration doesn't match.
    for
      (_, o) <- setup
      _      <- expectOdbError(
                  pi,
                  insertQuery(o, List(SmartGcalType.Flat), sequenceType = SequenceType.Acquisition.some),
                  {
                    case OdbError.InvalidArgument(Some(m)) if m.startsWith("Cannot insert a smart GCAL flat step: missing Smart GCAL mapping: GmosNorth { grating: None") => ()
                  }
                )
      _      <- expectSequenceFlags(o, acqMat = false, sciMat = false, sciCustom = false)
    yield ()

  test("completed observation: staff may insert, which reopens it; a PI may not"):
    for
      (p, o) <- setup
      v      <- recordVisitAs(serviceUser, o)
      seq0   <- science(o)
      _      <- executeAll(seq0.flatMap(_._2), v)
      w0     <- workflowState(p, o)
      _      <- insertFails(
                  pi,
                  o,
                  List(SmartGcalType.Arc),
                  none,
                  List(
                    s"Observation $o is ineligible for this operation due to its workflow state (Completed).",
                    "User cannot insert smart GCAL steps in the current observation workflow state."
                  )
                )
      _      <- insertAs(staff, o, List(SmartGcalType.Arc))
      w1     <- workflowState(p, o)
      seq1   <- science(o)
    yield
      assertEquals(w0, ObservationWorkflowState.Completed)
      assertEquals(w1, ObservationWorkflowState.Ongoing)
      // same visit, so the last atom is extended
      seq1 match
        case List((aid, List(arc))) =>
          assertEquals(aid, seq0.last._1)
          assertGcalStep(arc, seq0.last._2.last, arc = true)
        case other                  =>
          fail(s"Expected the last atom extended by the arc, got $other")
