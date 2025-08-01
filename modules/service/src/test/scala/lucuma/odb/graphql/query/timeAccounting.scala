// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Encoder
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.AtomStage
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.Site
import lucuma.core.enums.StepStage
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.enums.TwilightType.Nautical
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.ExecutionEvent.*
import lucuma.core.model.Observation
import lucuma.core.model.ObservingNight
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.BandedTime
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.TimeCharge
import lucuma.odb.graphql.input.AllocationInput
import lucuma.odb.graphql.input.TimeChargeCorrectionInput
import lucuma.odb.service.TimeAccounting
import lucuma.odb.util.Codecs.*
import skunk.syntax.all.*

import java.time.LocalDateTime
import java.time.Month

class timeAccounting extends OdbSuite with DatabaseOperations { this: OdbSuite =>

  val pi         = TestUsers.Standard.pi(1, 30)
  val service    = TestUsers.service(2)
  val staff      = TestUsers.Standard.staff(3, 33)
  val mode       = ObservingModeType.GmosSouthLongSlit

  val EventId    = ExecutionEvent.Id.fromLong(1).get  // we'll just share one
  val visitLdt   = LocalDateTime.of(2023, Month.DECEMBER, 19, 23, 0)
  val visitTime  = Timestamp.unsafeFromLocalDateTime(visitLdt)
  val night      = ObservingNight.fromSiteAndLocalDateTime(Site.GS, visitLdt)
  val tbn        = night.twilightBoundedUnsafe(Nautical)
  val tbnStart   = Timestamp.unsafeFromInstantTruncated(tbn.start)

  extension (i: Int) {
    def sec: TimeSpan =
      TimeSpan.FromMicroseconds.getOption(i * 1_000_000L).get

    def fromNightStart: Timestamp =
      Timestamp.unsafeFromInstantTruncated(tbn.start).plusMillisOption(i * 1000L).get

    def fromNightEnd: Timestamp =
      Timestamp.unsafeFromInstantTruncated(tbn.end).plusMillisOption(i * 1000L).get
  }

  extension (s: String) {
    def comment: Option[NonEmptyString] =
      NonEmptyString.from(s).toOption
  }

  val tMinus10 = -10.fromNightStart
  val t00 =  0.fromNightStart
  val t10 = 10.fromNightStart
  val t20 = 20.fromNightStart
  val t30 = 30.fromNightStart
  val t40 = 40.fromNightStart
  val t50 = 50.fromNightStart

  val validUsers = List(pi, service, staff)

  def invoiceQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          execution {
            visits {
              matches {
                timeChargeInvoice {
                  executionTime {
                    program { seconds }
                    nonCharged { seconds }
                    total { seconds }
                  }
                  discounts {
                    interval {
                      start
                      end
                    }
                    amount { seconds }
                    comment
                    ... on TimeChargeDaylightDiscount {
                      site
                    }
                    ... on TimeChargeOverlapDiscount {
                      observation {
                        id
                      }
                    }
                    ... on TimeChargeQaDiscount {
                      datasets {
                        id
                      }
                    }
                  }
                  corrections {
                    chargeClass
                    op
                    amount { seconds }
                    user { id }
                    comment
                  }
                  finalCharge {
                    program { seconds }
                    nonCharged { seconds }
                    total { seconds }
                  }
                }
              }
            }
          }
        }
      }
    """

  def expectedBandedTime(
    bt: BandedTime
  ): Json =
    json"""
      {
        "band": ${bt.band.map(_.tag.toScreamingSnakeCase).asJson},
        "time": ${expectedCategorizedTime(bt.time)}
      }
    """

  def expectedCategorizedTime(
    ct: CategorizedTime
  ): Json =
    json"""
      {
        "program": {
          "seconds": ${ct.programTime.toSeconds}
        },
        "nonCharged": {
          "seconds": ${ct.nonCharged.toSeconds}
        },
        "total": {
          "seconds": ${ct.sum.toSeconds}
        }
      }
    """

  def expectedCorrection(
    c: TimeChargeCorrectionInput
  ): Json =
    json"""
      {
        "chargeClass": ${c.chargeClass.tag.toScreamingSnakeCase},
        "op": ${c.op.tag.toScreamingSnakeCase},
        "amount": {
          "seconds": ${c.amount.toSeconds}
        },
        "user": {
          "id": ${staff.id.toString}
        },
        "comment": ${c.comment.map(_.value).asJson}
      }
    """

  def expectedDiscount(
    d: TimeCharge.DiscountEntry
  ): Json =
    json"""
      {
        "interval": {
          "start": ${d.discount.interval.start.asJson},
          "end": ${d.discount.interval.end.asJson}
        },
        "amount": {
          "seconds": ${d.discount.program.toSeconds}
        },
        "comment": ${d.discount.comment}
      }
    """.withObject: obj =>
      d match
        case TimeCharge.DiscountEntry.Daylight(_, s) => obj.add("site",     s.asJson).toJson
        case TimeCharge.DiscountEntry.NoData(_)      => obj.toJson
        case TimeCharge.DiscountEntry.Overlap(_, o)  => obj.add("observation", Json.obj("id" -> o.asJson)).toJson
        case TimeCharge.DiscountEntry.Qa(_, ds)      => obj.add("datasets", ds.toList.map(id => Json.obj("id" -> id.asJson)).asJson).toJson

  def invoiceExected(
    invoice:     TimeCharge.Invoice,
    corrections: List[TimeChargeCorrectionInput]
  ): Either[List[String], Json] = {

    val matches = List(
      json"""
      {
        "timeChargeInvoice": {
          "executionTime": ${expectedCategorizedTime(invoice.executionTime)},
          "discounts": ${invoice.discounts.map(expectedDiscount).asJson},
          "corrections": ${corrections.map(expectedCorrection).asJson},
          "finalCharge": ${expectedCategorizedTime(invoice.finalCharge)}
        }
      }
      """
    ).asJson

    json"""
      {
        "observation": {
          "execution": {
            "visits": {
              "matches": $matches
            }
          }
        }
      }
    """.asRight
  }

  case class StepNode(
    sid:  Step.Id,
    dids: List[Dataset.Id]
  )

  case class AtomNode(
    aid:   Atom.Id,
    steps: List[StepNode]
  )

  case class VisitNode(
    pid:   Program.Id,
    oid:   Observation.Id,
    vid:   Visit.Id,
    atoms: List[AtomNode]
  )

  def recordStep(
    user:         User,
    mode:         ObservingModeType,
    aid:          Atom.Id,
    datasetCount: Int,
    idx:          Int
  ): IO[StepNode] =
    for {
      sid  <- recordStepAs(user, mode.instrument, aid)
      dids <- (0 until datasetCount).toList.traverse { d => recordDatasetAs(user, sid, f"N18630101S${idx+d+1}%04d.fits") }
    } yield StepNode(sid, dids)

  def recordAtom(
    user:         User,
    mode:         ObservingModeType,
    vid:          Visit.Id,
    stepCount:    Int,
    datasetCount: Int,
    idx:          Int
  ): IO[AtomNode] =

    for {
      aid   <- recordAtomAs(user, mode.instrument, vid)
      steps <- (0 until stepCount).toList.traverse { s => recordStep(user, mode, aid, datasetCount, idx + s * datasetCount) }
    } yield AtomNode(aid, steps)

  def setVisitTime(
    vid:  Visit.Id,
    when: Timestamp
  ): IO[Unit] =
    withSession { s =>
      val cmd = sql"""
        UPDATE t_visit
           SET c_created  = $core_timestamp
         WHERE c_visit_id = $visit_id
      """.command

      s.execute(cmd)((when, vid)).void
    }

  def recordVisit(
    user:         User,
    serviceUser:  User,
    mode:         ObservingModeType,
    when:         Timestamp,
    atomCount:    Int,
    stepCount:    Int,
    datasetCount: Int,
    idx:          Int
  ): IO[VisitNode] =

    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      v   <- recordVisitForObs(pid, oid, serviceUser, mode, when, atomCount, stepCount, datasetCount, idx)
    } yield v

  def recordVisitForObs(
    pid:          Program.Id,
    oid:          Observation.Id,
    user:         User,
    mode:         ObservingModeType,
    when:         Timestamp,
    atomCount:    Int,
    stepCount:    Int,
    datasetCount: Int,
    idx:          Int
  ): IO[VisitNode] =

    for {
      vid   <- recordVisitAs(user, mode.instrument, oid)
      _     <- setVisitTime(vid, when)
      atoms <- (0 until atomCount).toList.traverse { a => recordAtom(user, mode, vid, stepCount, datasetCount, idx + a * stepCount * datasetCount) }
    } yield VisitNode(pid, oid, vid, atoms)

  def insertSequenceEvent(p: Program.Id)(e: SequenceEvent): IO[Unit] =
    withSession { s =>
      val cmd = sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_received,
          c_program_id,
          c_observation_id,
          c_visit_id,
          c_sequence_command
        )
        SELECT
          'sequence' :: e_execution_event_type,
          $core_timestamp,
          $program_id,
          $observation_id,
          $visit_id,
          $sequence_command
      """.command

      s.execute(cmd)((e.received, p, e.observationId, e.visitId, e.command))
    }.void

  def insertSlewEvent(p: Program.Id)(e: SlewEvent): IO[Unit] =
    withSession { s =>
      val cmd = sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_received,
          c_program_id,
          c_observation_id,
          c_visit_id,
          c_slew_stage
        )
        SELECT
          'slew' :: e_execution_event_type,
          $core_timestamp,
          $program_id,
          $observation_id,
          $visit_id,
          $slew_stage
      """.command

      s.execute(cmd)((e.received, p, e.observationId, e.visitId, e.stage))
    }.void

  def insertAtomEvent(p: Program.Id)(e: AtomEvent): IO[Unit] =
    withSession { s =>
      val cmd = sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_received,
          c_program_id,
          c_observation_id,
          c_visit_id,
          c_atom_id,
          c_atom_stage
        )
        SELECT
          'atom' :: e_execution_event_type,
          $core_timestamp,
          $program_id,
          $observation_id,
          $visit_id,
          $atom_id,
          $atom_stage
      """.command

      s.execute(cmd)((e.received, p, e.observationId, e.visitId, e.atomId, e.stage))
    }.void

  def insertStepEvent(p: Program.Id)(e: StepEvent): IO[Unit] =
    withSession { s =>
      val cmd = sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_received,
          c_program_id,
          c_observation_id,
          c_visit_id,
          c_atom_id,
          c_step_id,
          c_step_stage
        )
        SELECT
          'step' :: e_execution_event_type,
          $core_timestamp,
          $program_id,
          $observation_id,
          $visit_id,
          $atom_id,
          $step_id,
          $step_stage
      """.command

      s.execute(cmd)((e.received, p, e.observationId, e.visitId, e.atomId, e.stepId, e.stage))
    }.void

  def insertDatasetEvent(p: Program.Id)(e: DatasetEvent): IO[Unit] =
    withSession { s =>
      val cmd = sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_received,
          c_program_id,
          c_observation_id,
          c_visit_id,
          c_atom_id,
          c_step_id,
          c_dataset_id,
          c_dataset_stage
        )
        SELECT
          'dataset' :: e_execution_event_type,
          $core_timestamp,
          $program_id,
          $observation_id,
          $visit_id,
          $atom_id,
          $step_id,
          $dataset_id,
          $dataset_stage
      """.command

      s.execute(cmd)((e.received, p, e.observationId, e.visitId, e.atomId, e.stepId, e.datasetId, e.stage))
    }.void

  // For sanity sake, each test will start clean so that visits
  // are not overlapping.
  def cleanup: IO[Unit] =
    withSession: s =>
      s.execute(sql"DELETE FROM t_execution_event".command).void

  def insertEvents(
    pid:    Program.Id,
    events: List[ExecutionEvent]
  ): IO[Unit] =
    events.traverse_ { e =>
      e.fold(insertSlewEvent(pid), insertSequenceEvent(pid), insertAtomEvent(pid), insertStepEvent(pid), insertDatasetEvent(pid))
    }

  test("timeChargeInvoice (no events)") {
    recordVisit(pi, service, mode, visitTime, 1, 1, 1, 0).flatMap { v =>
      expect(pi, invoiceQuery(v.oid), invoiceExected(TimeCharge.Invoice.Empty, Nil))
    }
  }

  test("timeChargeInvoice (no discounts)") {
    val events = List(
      (SequenceCommand.Start, t00),
      (SequenceCommand.Stop,  t10)
    )

    val expExecution   = CategorizedTime(ChargeClass.Program -> 10.sec)
    val expFinalCharge = CategorizedTime(ChargeClass.Program -> 10.sec)
    val invoice        = TimeCharge.Invoice(expExecution, Nil, expFinalCharge)

    for {
      v <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 100)
      es = events.map { (c, t) => SequenceEvent(EventId, t, v.oid, v.vid, none, c) }
      _ <- insertEvents(v.pid, es)
      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v.vid)(using xa) } }
      _ <- expect(pi, invoiceQuery(v.oid), invoiceExected(invoice, Nil))
    } yield ()

  }

  test("timeChargeInvoice (daylight discount)") {

    val t0 = -1.fromNightStart
    val t1 =  1.fromNightStart

    val events = List(
      (SequenceCommand.Start, t0),
      (SequenceCommand.Stop,  t1)
    )

    val expExecution   = CategorizedTime(ChargeClass.Program -> 2.sec)
    val discount       = TimeCharge.Discount(
      TimestampInterval.between(t0, tbnStart),
      1.sec,
      TimeAccounting.comment.PreDusk
    )
    val daylightEntry  = TimeCharge.DiscountEntry.Daylight(discount, Site.GS)
    val expFinalCharge = CategorizedTime(ChargeClass.Program -> 1.sec)
    val invoice        = TimeCharge.Invoice(expExecution, List(daylightEntry), expFinalCharge)

    for {
      _ <- cleanup
      v <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 200)
      es = events.map { (c, t) => SequenceEvent(EventId, t, v.oid, v.vid, none, c) }
      _ <- insertEvents(v.pid, es)
      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v.vid)(using xa) } }
      _ <- expect(pi, invoiceQuery(v.oid), invoiceExected(invoice, Nil))
    } yield ()

  }

  test("timeChargeInvoice (no data discount)") {
    val events = List(
      (SequenceCommand.Start, t00),
      (SequenceCommand.Stop,  t10)
    )

    val expExecution = CategorizedTime(ChargeClass.Program -> 10.sec)
    val discount     = TimeCharge.Discount(
      TimestampInterval.between(t00, t10),
      10.sec,
      TimeAccounting.comment.NoData
    )
    val noDataEntry  = TimeCharge.DiscountEntry.NoData(discount)
    val invoice      = TimeCharge.Invoice(expExecution, List(noDataEntry), CategorizedTime.Zero)

    for {
      _ <- cleanup
      v <- recordVisit(pi, service, mode, visitTime, 1, 0, 0, 250)
      es = events.map { (c, t) => SequenceEvent(EventId, t, v.oid, v.vid, none, c) }
      _ <- insertEvents(v.pid, es)
      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v.vid)(using xa) } }
      _ <- expect(pi, invoiceQuery(v.oid), invoiceExected(invoice, Nil))
    } yield ()

  }

  test("timeChargeInvoice (qa discount)") {

    val ts = (0 until 12).map(_.fromNightStart)

    def events(v: VisitNode): List[ExecutionEvent] = {

      val aid0 = v.atoms.head.aid
      val sid0 = v.atoms.head.steps.head.sid
      val did0 = v.atoms.head.steps.head.dids.head

      val aid1 = v.atoms.last.aid
      val sid1 = v.atoms.last.steps.head.sid
      val did1 = v.atoms.last.steps.head.dids.head

      List(
        AtomEvent(   EventId, ts( 0), v.oid, v.vid, none, aid0,             AtomStage.StartAtom),
        StepEvent(   EventId, ts( 1), v.oid, v.vid, none, aid0, sid0,       StepStage.StartStep),
        DatasetEvent(EventId, ts( 2), v.oid, v.vid, none, aid0, sid0, did0, DatasetStage.StartExpose),
        DatasetEvent(EventId, ts( 3), v.oid, v.vid, none, aid0, sid0, did0, DatasetStage.EndWrite),
        StepEvent(   EventId, ts( 4), v.oid, v.vid, none, aid0, sid0,       StepStage.EndStep),
        AtomEvent(   EventId, ts( 5), v.oid, v.vid, none, aid0,             AtomStage.EndAtom),

        AtomEvent(   EventId, ts( 6), v.oid, v.vid, none, aid1,             AtomStage.StartAtom),
        StepEvent(   EventId, ts( 7), v.oid, v.vid, none, aid1, sid1,       StepStage.StartStep),
        DatasetEvent(EventId, ts( 8), v.oid, v.vid, none, aid1, sid1, did1, DatasetStage.StartExpose),
        DatasetEvent(EventId, ts( 9), v.oid, v.vid, none, aid1, sid1, did1, DatasetStage.EndWrite),
        StepEvent(   EventId, ts(10), v.oid, v.vid, none, aid1, sid1,       StepStage.EndStep),
        AtomEvent(   EventId, ts(11), v.oid, v.vid, none, aid1,             AtomStage.EndAtom)
      )
    }

    val expExecution   = CategorizedTime(ChargeClass.Program -> 11.sec)
    val discount       = TimeCharge.Discount(
      TimestampInterval.between(ts(6), ts(11)),
      5.sec,
      TimeAccounting.comment.Qa
    )
    def qaEntry(v: VisitNode) = TimeCharge.DiscountEntry.Qa(discount, v.atoms.last.steps.head.dids.toSet)
    val expFinalCharge        = CategorizedTime(ChargeClass.Program -> 6.sec)
    def invoice(v: VisitNode) = TimeCharge.Invoice(expExecution, List(qaEntry(v)), expFinalCharge)

    for {
      _ <- cleanup
      v <- recordVisit(pi, service, mode, visitTime, 2, 1, 1, 300)
      es = events(v)
      _ <- insertEvents(v.pid, es)
      _ <- updateDatasets(staff, DatasetQaState.Fail, v.atoms.last.steps.head.dids)
      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v.vid)(using xa) } }
      _ <- expect(pi, invoiceQuery(v.oid), invoiceExected(invoice(v), Nil))
    } yield ()

  }

  private def correctionTest(
    corrections:  List[TimeChargeCorrectionInput],
    finalCharge: TimeSpan,
    index:       Int
  ): IO[Unit] = {
    val events = List(
      (SequenceCommand.Start, t00),
      (SequenceCommand.Stop,  t10)
    )

    val expExecution   = CategorizedTime(ChargeClass.Program -> 10.sec)
    val expFinalCharge = CategorizedTime(ChargeClass.Program -> finalCharge)
    val invoice        = TimeCharge.Invoice(expExecution, Nil, expFinalCharge)

    for {
      _ <- cleanup
      v <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, index)
      es = events.map { (c, t) => SequenceEvent(EventId, t, v.oid, v.vid, none, c) }
      _ <- insertEvents(v.pid, es)
      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v.vid)(using xa) } }
      _ <- corrections.traverse_ { c => addTimeChargeCorrection(staff, v.vid, c) }
      _ <- expect(pi, invoiceQuery(v.oid), invoiceExected(invoice, corrections))
    } yield ()
  }

  test("timeChargeInvoice (simple add correction)") {
    correctionTest(
      List(TimeChargeCorrectionInput(ChargeClass.Program, TimeChargeCorrection.Op.Add, 5.sec, "add".comment)),
      15.sec,
      400
    )
  }

  test("timeChargeInvoice (simple subtract correction)") {
    correctionTest(
      List(TimeChargeCorrectionInput(ChargeClass.Program, TimeChargeCorrection.Op.Subtract, 5.sec, "subtract".comment)),
      5.sec,
      500
    )
  }

  test("timeChargeInvoice (over subtract)") {
    correctionTest(
      List(TimeChargeCorrectionInput(ChargeClass.Program, TimeChargeCorrection.Op.Subtract, 11.sec, "over subtract".comment)),
      0.sec,
      600
    )
  }

  test("timeChargeInvoice (over add)") {
    recordVisit(pi, service, mode, visitTime, 1, 1, 1, 700).flatMap: v =>
      expect(
        user  = pi,
        query =
          s"""
            mutation {
              addTimeChargeCorrection(input: {
                visitId: "${v.vid}",
                correction: {
                  chargeClass: PROGRAM,
                  op: ADD,
                  amount: {
                    hours: "${366 * 24}"
                  }
                }
              }) {
                timeChargeInvoice {
                  corrections {
                    created
                  }
                }
              }
            }
          """,
        expected = List(
          "Argument 'input.correction' is invalid: Time charge correction values over 365 days are not permitted."
        ).asLeft
      )
  }

  test("timeChargeInvoice (multi-correction)") {
    correctionTest(
      List(
        TimeChargeCorrectionInput(ChargeClass.Program, TimeChargeCorrection.Op.Add, 1.sec, "add 1".comment),
        TimeChargeCorrectionInput(ChargeClass.Program, TimeChargeCorrection.Op.Subtract, 2.sec, "subtract 2".comment)
      ),
      9.sec,
      800
    )
  }

  test("timeChargeInvoice (pre correction add)") {
    val expExecution   = CategorizedTime(ChargeClass.Program -> 0.sec)
    val expFinalCharge = CategorizedTime(ChargeClass.Program -> 5.sec)
    val invoice        = TimeCharge.Invoice(expExecution, Nil, expFinalCharge)
    val correction     = TimeChargeCorrectionInput(ChargeClass.Program, TimeChargeCorrection.Op.Add, 5.sec, "add".comment)

    for {
      _ <- cleanup
      v <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 900)
      _ <- addTimeChargeCorrection(staff, v.vid, correction)
      _ <- expect(pi, invoiceQuery(v.oid), invoiceExected(invoice, List(correction)))
    } yield ()
  }

  test("timeChargeInvoice (pre correction subtract)") {
    val expExecution   = CategorizedTime(ChargeClass.Program -> 0.sec)
    val expFinalCharge = CategorizedTime(ChargeClass.Program -> 0.sec)
    val invoice        = TimeCharge.Invoice(expExecution, Nil, expFinalCharge)
    val correction     = TimeChargeCorrectionInput(ChargeClass.Program, TimeChargeCorrection.Op.Subtract, 5.sec, "add".comment)

    for {
      _ <- cleanup
      v <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 1000)
      _ <- addTimeChargeCorrection(staff, v.vid, correction)
      _ <- expect(pi, invoiceQuery(v.oid), invoiceExected(invoice, List(correction)))
    } yield ()
  }

  def observationQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          execution {
            timeCharge {
              program { seconds }
              nonCharged { seconds }
              total { seconds }
            }
          }
        }
      }
    """

  def observationExpectedCharge(ct: CategorizedTime): Either[List[String], Json] =
    json"""
      {
        "observation": {
          "execution": {
            "timeCharge": ${expectedCategorizedTime(ct)}
          }
        }
      }
    """.asRight

  test("observation timeCharge, empty observation") {
    for {
      v <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 1100)
      _ <- expect(pi, observationQuery(v.oid), observationExpectedCharge(CategorizedTime.Zero))
    } yield ()
  }

  test("observation timeCharge, one visit") {
    val events = List(
      (SequenceCommand.Start, t00),
      (SequenceCommand.Stop,  t10)
    )

    val expected = CategorizedTime(ChargeClass.Program -> 10.sec)

    for {
      _ <- cleanup
      v <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 1200)
      es = events.map { (c, t) => SequenceEvent(EventId, t, v.oid, v.vid, none, c) }
      _ <- insertEvents(v.pid, es)
      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v.vid)(using xa) } }
      _ <- expect(pi, observationQuery(v.oid), observationExpectedCharge(expected))
    } yield ()

  }

  test("observation timeCharge, two visits") {
    val events0 = List(
      (SequenceCommand.Start, t00),
      (SequenceCommand.Stop,  t10)
    )
    val events1 = List(
      (SequenceCommand.Start, t30),
      (SequenceCommand.Stop,  t40)
    )

    val expected = CategorizedTime(ChargeClass.Program -> 20.sec)

    for {
      _  <- cleanup
      v0 <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 1300)
      pid = v0.pid
      oid = v0.oid
      es0 = events0.map { (c, t) => SequenceEvent(EventId, t, oid, v0.vid, none, c) }
      _ <- insertEvents(pid, es0)
      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v0.vid)(using xa) } }

      v1 <- recordVisitForObs(pid, oid, service, mode, t20, 1, 1, 1, 1301)
      es1 = events1.map { (c, t) => SequenceEvent(EventId, t, oid, v1.vid, none, c) }
      _ <- insertEvents(pid, es1)
      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v1.vid)(using xa) } }

      _ <- expect(pi, observationQuery(oid), observationExpectedCharge(expected))
    } yield ()

  }


/*
  test("observation timeCharge, overflow") {
    val t0 =  0.fromNightStart
    val t1 = 10.fromNightStart
    val t2 = 20.fromNightStart
    val t3 = 30.fromNightStart
    val t4 = 40.fromNightStart

    val events0 = List(
      (SequenceCommand.Start, t0),
      (SequenceCommand.Stop,  t1)
    )
    val events1 = List(
      (SequenceCommand.Start, t3),
      (SequenceCommand.Stop,  t4)
    )

    val expected   = TimeAccounting.CategorizedTimeMax
    val correction = TimeChargeCorrectionInput(ChargeClass.Program, TimeChargeCorrection.Op.Add, TimeSpan.Max, "add max".comment)

    for {
      v0 <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 1400)
      pid = v0.pid
      oid = v0.oid
      es0 = events0.map { (c, t) => SequenceEvent(EventId, t, oid, v0.vid, c) }
      _ <- insertEvents(es0)
      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v0.vid)(using xa) } }

      v1 <- recordVisitForObs(pid, oid, service, mode, t2, 1, 1, 1, 1401)
      es1 = events1.map { (c, t) => SequenceEvent(EventId, t, oid, v1.vid, c) }
      _ <- insertEvents(es1)
      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v1.vid)(using xa) } }
      _ <- addTimeChargeCorrection(staff, v1.vid, correction)

      _ <- expect(pi, observationQuery(oid), observationExpectedCharge(expected))
    } yield ()

  }
 */

    def programQuery(pid: Program.Id): String =
    s"""
      query {
        program(programId: "$pid") {
          timeCharge {
            band
            time {
              program { seconds }
              nonCharged { seconds }
              total { seconds }
            }
          }
        }
      }
    """

  def programExpectedCharge(times: List[BandedTime]): Either[List[String], Json] =
    json"""
      {
        "program": {
          "timeCharge": ${times.map(expectedBandedTime).asJson}
        }
      }
    """.asRight

  test("program timeCharge, no observations") {
    for {
      _ <- cleanup
      p <- createProgramAs(pi)
      _ <- expect(pi, programQuery(p), programExpectedCharge(Nil))
    } yield ()
  }

  test("program timeCharge, one observation") {
    val events = List(
      (SequenceCommand.Start, t00),
      (SequenceCommand.Stop,  t10)
    )

    val expected = List(BandedTime(None, CategorizedTime(ChargeClass.Program -> 10.sec)))

    for {
      _ <- cleanup
      v <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 1500)
      es = events.map { (c, t) => SequenceEvent(EventId, t, v.oid, v.vid, none, c) }
      _ <- insertEvents(v.pid, es)
      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v.vid)(using xa) } }
      _ <- expect(pi, programQuery(v.pid), programExpectedCharge(expected))
    } yield ()
  }

  test("program timeCharge, two observations, no band") {
    val events0 = List(
      (SequenceCommand.Start, t00),
      (SequenceCommand.Stop,  t10)
    )
    val events1 = List(
      (SequenceCommand.Start, t30),
      (SequenceCommand.Stop,  t40)
    )

    val expected = List(BandedTime(None, CategorizedTime(ChargeClass.Program -> 20.sec)))

    for {
      _   <- cleanup
      // Obs0
      v0  <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 1600)
      pid  = v0.pid
      oid0 = v0.oid
      es0  = events0.map { (c, t) => SequenceEvent(EventId, t, oid0, v0.vid, none, c) }
      _   <- insertEvents(pid, es0)
      _   <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v0.vid)(using xa) } }

      // Obs1
      oid1 <- createObservationAs(pi, pid, mode.some)
      v1   <- recordVisitForObs(pid, oid1, service, mode, t20, 1, 1, 1, 1601)
      es1   = events1.map { (c, t) => SequenceEvent(EventId, t, oid1, v1.vid, none, c) }
      _    <- insertEvents(pid, es1)
      _    <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v1.vid)(using xa) } }

      _ <- expect(pi, programQuery(pid), programExpectedCharge(expected))
    } yield ()
  }

  test("program timeCharge, two observations, same band") {

    val events0 = List(
      (SequenceCommand.Start, t00),
      (SequenceCommand.Stop,  t10)
    )
    val events1 = List(
      (SequenceCommand.Start, t30),
      (SequenceCommand.Stop,  t40)
    )

    val expected = List(BandedTime(ScienceBand.Band2.some, CategorizedTime(ChargeClass.Program -> 20.sec)))

    for {
      _    <- cleanup
      pid  <- createProgramAs(pi)
      _    <- setAllocationsAs(service, pid, List(AllocationInput(TimeAccountingCategory.BR, ScienceBand.Band2, 10.hourTimeSpan)))

      // Obs0
      oid0 <- createObservationAs(pi, pid, mode.some)
      _    <- setScienceBandAs(pi, oid0, ScienceBand.Band2.some)
      v0   <- recordVisitForObs(pid, oid0, service, mode, visitTime, 1, 1, 1, 1700)

      es0  = events0.map { (c, t) => SequenceEvent(EventId, t, oid0, v0.vid, none, c) }
      _   <- insertEvents(pid, es0)
      _   <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v0.vid)(using xa) } }

      // Obs1
      oid1 <- createObservationAs(pi, pid, mode.some)
      _    <- setScienceBandAs(pi, oid0, ScienceBand.Band2.some)
      v1   <- recordVisitForObs(pid, oid1, service, mode, t20, 1, 1, 1, 1701)
      es1   = events1.map { (c, t) => SequenceEvent(EventId, t, oid1, v1.vid, none, c) }
      _    <- insertEvents(pid, es1)
      _    <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v1.vid)(using xa) } }

      _ <- expect(pi, programQuery(pid), programExpectedCharge(expected))
    } yield ()
  }

  test("program timeCharge, two observations, distinct bands") {

    val events0 = List(
      (SequenceCommand.Start, t00),
      (SequenceCommand.Stop,  t10)
    )
    val events1 = List(
      (SequenceCommand.Start, t30),
      (SequenceCommand.Stop,  t40)
    )

    val allocations = List(
      AllocationInput(TimeAccountingCategory.AR, ScienceBand.Band1, 10.hourTimeSpan),
      AllocationInput(TimeAccountingCategory.BR, ScienceBand.Band2, 10.hourTimeSpan)
    )

    val expected = List(
      BandedTime(ScienceBand.Band1.some, CategorizedTime(ChargeClass.Program -> 10.sec)),
      BandedTime(ScienceBand.Band2.some, CategorizedTime(ChargeClass.Program -> 10.sec))
    )

    for {
      _    <- cleanup
      pid  <- createProgramAs(pi)
      _    <- setAllocationsAs(service, pid, allocations)

      // Obs0
      oid0 <- createObservationAs(pi, pid, mode.some)
      _    <- setScienceBandAs(pi, oid0, ScienceBand.Band1.some)
      v0   <- recordVisitForObs(pid, oid0, service, mode, visitTime, 1, 1, 1, 1800)

      es0  = events0.map { (c, t) => SequenceEvent(EventId, t, oid0, v0.vid, none, c) }
      _   <- insertEvents(pid, es0)
      _   <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v0.vid)(using xa) } }

      // Obs1
      oid1 <- createObservationAs(pi, pid, mode.some)
      _    <- setScienceBandAs(pi, oid1, ScienceBand.Band2.some)
      v1   <- recordVisitForObs(pid, oid1, service, mode, t20, 1, 1, 1, 1801)
      es1   = events1.map { (c, t) => SequenceEvent(EventId, t, oid1, v1.vid, none, c) }
      _    <- insertEvents(pid, es1)
      _    <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v1.vid)(using xa) } }

      _ <- expect(pi, programQuery(pid), programExpectedCharge(expected))
    } yield ()
  }

  test("timeChargeInvoice (overlap)"):

    val events0 = List(
      (SequenceCommand.Start, tMinus10),
      (SequenceCommand.Stop,  t20)
    )

    val events1 = List(
      (SequenceCommand.Start, t10),
      (SequenceCommand.Stop,  t30)
    )

    val expExecution0   = CategorizedTime(ChargeClass.Program -> 30.sec) // -10 -> 20
    val expExecution1   = CategorizedTime(ChargeClass.Program -> 20.sec) //  10 -> 30
    val discountDay     = TimeCharge.Discount(
      TimestampInterval.between(tMinus10, t00),
      10.sec,
      TimeAccounting.comment.PreDusk
    )
    val discountOverlap = TimeCharge.Discount(
      TimestampInterval.between(t10, t20),
      10.sec,
      TimeAccounting.comment.Overlap
    )
    val daylightEntry   = TimeCharge.DiscountEntry.Daylight(discountDay, Site.GS)
    def overlapEntry(oid: Observation.Id) =
      TimeCharge.DiscountEntry.Overlap(discountOverlap, oid)

    val expFinalCharge0 = CategorizedTime(ChargeClass.Program -> 10.sec) // 30 - 10 (daylight) - 10 (overlap)
    val expFinalCharge1 = CategorizedTime(ChargeClass.Program -> 20.sec)

    def invoice0(oid: Observation.Id) =
      TimeCharge.Invoice(expExecution0, List(daylightEntry, overlapEntry(oid)), expFinalCharge0)
    val invoice1 = TimeCharge.Invoice(expExecution1, Nil, expFinalCharge1)

    for
      _  <- cleanup
      v0 <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 1900)
      e0  = events0.map { (c, t) => SequenceEvent(EventId, t, v0.oid, v0.vid, none, c) }
      _  <- insertEvents(v0.pid, e0)
      v1 <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 1950)
      e1  = events1.map { (c, t) => SequenceEvent(EventId, t, v1.oid, v1.vid, none, c) }
      _  <- insertEvents(v1.pid, e1)

      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v0.vid)(using xa) } }
      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v1.vid)(using xa) } }

      _ <- expect(pi, invoiceQuery(v0.oid), invoiceExected(invoice0(v1.oid), Nil))
      _ <- expect(pi, invoiceQuery(v1.oid), invoiceExected(invoice1, Nil))
    yield ()

  test("timeChargeInvoice (overlap the overlap)"):

    val events0 = List(
      (SequenceCommand.Start, tMinus10),
      (SequenceCommand.Stop,  t20)
    )

    val events1 = List(
      (SequenceCommand.Start, t10),
      (SequenceCommand.Stop,  t40)
    )

    val events2 = List(
      (SequenceCommand.Start, t20),
      (SequenceCommand.Stop,  t50)
    )

    val expExecution0   = CategorizedTime(ChargeClass.Program -> 30.sec) // -10 -> 20
    val expExecution1   = CategorizedTime(ChargeClass.Program -> 30.sec) //  10 -> 40
    val expExecution2   = CategorizedTime(ChargeClass.Program -> 30.sec) //  20 -> 50

    val discountDay      = TimeCharge.Discount(
      TimestampInterval.between(tMinus10, t00),
      10.sec,
      TimeAccounting.comment.PreDusk
    )
    val discountOverlap0 = TimeCharge.Discount(
      TimestampInterval.between(t10, t20),
      10.sec,
      TimeAccounting.comment.Overlap
    )
    val discountOverlap1 = TimeCharge.Discount(
      TimestampInterval.between(t20, t40),
      20.sec,
      TimeAccounting.comment.Overlap
    )

    val daylightEntry   = TimeCharge.DiscountEntry.Daylight(discountDay, Site.GS)
    def overlapEntry0(oid: Observation.Id) =
      TimeCharge.DiscountEntry.Overlap(discountOverlap0, oid)
    def overlapEntry1(oid: Observation.Id) =
      TimeCharge.DiscountEntry.Overlap(discountOverlap1, oid)

    val expFinalCharge0 = CategorizedTime(ChargeClass.Program -> 10.sec) // 30 - 10 (daylight) - 10 (overlap)
    val expFinalCharge1 = CategorizedTime(ChargeClass.Program -> 10.sec) // 30 - 20 (overlap)
    val expFinalCharge2 = CategorizedTime(ChargeClass.Program -> 30.sec) // 30 (no discounts)

    def invoice0(oid: Observation.Id) =
      TimeCharge.Invoice(expExecution0, List(daylightEntry, overlapEntry0(oid)), expFinalCharge0)
    def invoice1(oid: Observation.Id) =
      TimeCharge.Invoice(expExecution1, List(overlapEntry1(oid)), expFinalCharge1)
    val invoice2 = TimeCharge.Invoice(expExecution2, Nil, expFinalCharge2)

    for
      _  <- cleanup
      v0 <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 2000)
      e0  = events0.map { (c, t) => SequenceEvent(EventId, t, v0.oid, v0.vid, none, c) }
      _  <- insertEvents(v0.pid, e0)
      v1 <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 2050)
      e1  = events1.map { (c, t) => SequenceEvent(EventId, t, v1.oid, v1.vid, none, c) }
      _  <- insertEvents(v1.pid, e1)
      v2 <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 2100)
      e2  = events2.map { (c, t) => SequenceEvent(EventId, t, v2.oid, v2.vid, none, c) }
      _  <- insertEvents(v2.pid, e2)

      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v0.vid)(using xa) } }
      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v1.vid)(using xa) } }
      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v2.vid)(using xa) } }

      _ <- expect(pi, invoiceQuery(v0.oid), invoiceExected(invoice0(v1.oid), Nil))
      _ <- expect(pi, invoiceQuery(v1.oid), invoiceExected(invoice1(v2.oid), Nil))
      _ <- expect(pi, invoiceQuery(v2.oid), invoiceExected(invoice2, Nil))
    yield ()

  test("timeChargeInvoice (simultaneous discounts)"):

    val tem20 = -20.fromNightEnd
    val tem10 = -10.fromNightEnd
    val te00 =   0.fromNightEnd
    val te10  =  10.fromNightEnd
    val te20  =  20.fromNightEnd

    val events0 = List(
      (SequenceCommand.Start, tem20),
      (SequenceCommand.Stop,   te10)
    )

    val events1 = List(
      (SequenceCommand.Start, tem10),
      (SequenceCommand.Stop,   te20)
    )

    val expExecution0   = CategorizedTime(ChargeClass.Program -> 30.sec) // -20 -> 10
    val expExecution1   = CategorizedTime(ChargeClass.Program -> 30.sec) // -10 -> 20

    val discountDay0    = TimeCharge.Discount(
      TimestampInterval.between(te00, te10),
      10.sec,
      TimeAccounting.comment.PostDawn
    )
    val discountDay1    = TimeCharge.Discount(
      TimestampInterval.between(te00, te20),
      20.sec,
      TimeAccounting.comment.PostDawn
    )

    val discountOverlap = TimeCharge.Discount(
      TimestampInterval.between(tem10, te00),
      10.sec,
      TimeAccounting.comment.Overlap
    )
    val daylightEntry0  = TimeCharge.DiscountEntry.Daylight(discountDay0, Site.GS)
    val daylightEntry1  = TimeCharge.DiscountEntry.Daylight(discountDay1, Site.GS)
    def overlapEntry(oid: Observation.Id) =
      TimeCharge.DiscountEntry.Overlap(discountOverlap, oid)

    val expFinalCharge0 = CategorizedTime(ChargeClass.Program -> 10.sec) // 30 - 10 (daylight) - 10 (overlap)
    val expFinalCharge1 = CategorizedTime(ChargeClass.Program -> 10.sec) // 30 - 20 (daylight)

    def invoice0(oid: Observation.Id) =
      TimeCharge.Invoice(expExecution0, List(overlapEntry(oid), daylightEntry0), expFinalCharge0)
    val invoice1 = TimeCharge.Invoice(expExecution1, List(daylightEntry1), expFinalCharge1)

    for
      _  <- cleanup
      v0 <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 2150)
      e0  = events0.map { (c, t) => SequenceEvent(EventId, t, v0.oid, v0.vid, none, c) }
      _  <- insertEvents(v0.pid, e0)
      v1 <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 2200)
      e1  = events1.map { (c, t) => SequenceEvent(EventId, t, v1.oid, v1.vid, none, c) }
      _  <- insertEvents(v1.pid, e1)

      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v0.vid)(using xa) } }
      _ <- withServices(service) { s => s.session.transaction use { xa => s.timeAccountingService.update(v1.vid)(using xa) } }

      _ <- expect(pi, invoiceQuery(v0.oid), invoiceExected(invoice0(v1.oid), Nil))
      _ <- expect(pi, invoiceQuery(v1.oid), invoiceExected(invoice1, Nil))
    yield ()


}
