// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.Site
import lucuma.core.enums.StepStage
import lucuma.core.enums.TwilightType.Nautical
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.ExecutionEvent.*
import lucuma.core.model.Observation
import lucuma.core.model.ObservingNight
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.syntax.string.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.ObservingModeType
import lucuma.odb.data.TimeCharge
import lucuma.odb.graphql.input.TimeChargeCorrectionInput
import lucuma.odb.service.TimeAccounting
import lucuma.odb.util.Codecs.*
import skunk.syntax.all.*

import java.time.LocalDateTime
import java.time.Month

class timeAccounting extends OdbSuite with DatabaseOperations { this: OdbSuite =>

  val pi         = TestUsers.Standard.pi(1, 30)
  val service    = TestUsers.service(2)
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
  }

  extension (s: String) {
    def comment: Option[NonEmptyString] =
      NonEmptyString.from(s).toOption
  }

  val validUsers = List(pi, service)

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
                    partner { seconds }
                    nonCharged { seconds }
                    total { seconds }
                  }
                  discounts {
                    interval {
                      start
                      end
                    }
                    program { seconds }
                    partner { seconds }
                    comment
                    ... on TimeChargeDaylightDiscount {
                      site
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
                    partner { seconds }
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

  def expectedCategorizedTime(
    ct: CategorizedTime
  ): Json =
    json"""
      {
        "program": {
          "seconds": ${ct.programTime.toSeconds}
        },
        "partner": {
          "seconds": ${ct.partnerTime.toSeconds}
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
          "id": ${pi.id.toString}
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
        "program": {
          "seconds": ${d.discount.program.toSeconds}
        },
        "partner": {
          "seconds": ${d.discount.partner.toSeconds}
        },
        "comment": ${d.discount.comment}
      }
    """.withObject { obj =>
      d match {
        case TimeCharge.DiscountEntry.Daylight(_, s) => obj.add("site",     s.asJson).toJson
        case TimeCharge.DiscountEntry.NoData(_)      => obj.toJson
        case TimeCharge.DiscountEntry.Qa(_, ds)      => obj.add("datasets", ds.toList.map(id => Json.obj("id" -> id.asJson)).asJson).toJson
      }
    }

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

  def insertSequenceEvent(e: SequenceEvent): IO[Unit] =
    withSession { s =>
      val cmd = sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_received,
          c_observation_id,
          c_visit_id,
          c_sequence_command
        )
        SELECT
          'sequence' :: e_execution_event_type,
          $core_timestamp,
          $observation_id,
          $visit_id,
          $sequence_command
      """.command

      s.execute(cmd)((e.received, e.observationId, e.visitId, e.command))
    }.void

  def insertStepEvent(e: StepEvent): IO[Unit] =
    withSession { s =>
      val cmd = sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_received,
          c_observation_id,
          c_visit_id,
          c_step_id,
          c_step_stage
        )
        SELECT
          'step' :: e_execution_event_type,
          $core_timestamp,
          $observation_id,
          $visit_id,
          $step_id,
          $step_stage
      """.command

      s.execute(cmd)((e.received, e.observationId, e.visitId, e.stepId, e.stage))
    }.void

  def insertDatasetEvent(e: DatasetEvent): IO[Unit] =
    withSession { s =>
      val cmd = sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_received,
          c_observation_id,
          c_visit_id,
          c_step_id,
          c_dataset_id,
          c_dataset_stage
        )
        SELECT
          'dataset' :: e_execution_event_type,
          $core_timestamp,
          $observation_id,
          $visit_id,
          $step_id,
          $dataset_id,
          $dataset_stage
      """.command

      s.execute(cmd)((e.received, e.observationId, e.visitId, e.stepId, e.datasetId, e.stage))
    }.void

  def insertEvents(
    events: List[ExecutionEvent]
  ): IO[Unit] =
    events.traverse(_.fold(insertSequenceEvent, insertStepEvent, insertDatasetEvent)).void

  test("timeChargeInvoice (no events)") {
    recordVisit(pi, service, mode, visitTime, 1, 1, 1, 0).flatMap { v =>
      expect(pi, invoiceQuery(v.oid), invoiceExected(TimeCharge.Invoice.Empty, Nil))
    }
  }

  test("timeChargeInvoice (no discounts)") {

    val t0 =  0.fromNightStart
    val t1 = 10.fromNightStart

    val events = List(
      (SequenceCommand.Start, t0),
      (SequenceCommand.Stop,  t1)
    )

    val expExecution   = CategorizedTime(ChargeClass.Program -> 10.sec)
    val expFinalCharge = CategorizedTime(ChargeClass.Program -> 10.sec)
    val invoice        = TimeCharge.Invoice(expExecution, Nil, expFinalCharge)

    for {
      v <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 100)
      es = events.map { (c, t) => SequenceEvent(EventId, t, v.oid, v.vid, c) }
      _ <- insertEvents(es)
      _ <- withServices(pi) { s => s.session.transaction use { xa => s.timeAccountingService.update(v.vid)(using xa) } }
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
      TimeSpan.Zero,
      1.sec,
      TimeAccounting.comment.PreDusk
    )
    val daylightEntry  = TimeCharge.DiscountEntry.Daylight(discount, Site.GS)
    val expFinalCharge = CategorizedTime(ChargeClass.Program -> 1.sec)
    val invoice        = TimeCharge.Invoice(expExecution, List(daylightEntry), expFinalCharge)

    for {
      v <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 200)
      es = events.map { (c, t) => SequenceEvent(EventId, t, v.oid, v.vid, c) }
      _ <- insertEvents(es)
      _ <- withServices(pi) { s => s.session.transaction use { xa => s.timeAccountingService.update(v.vid)(using xa) } }
      _ <- expect(pi, invoiceQuery(v.oid), invoiceExected(invoice, Nil))
    } yield ()

  }

  test("timeChargeInvoice (no data discount)") {

    val t0 =  0.fromNightStart
    val t1 = 10.fromNightStart

    val events = List(
      (SequenceCommand.Start, t0),
      (SequenceCommand.Stop,  t1)
    )

    val expExecution = CategorizedTime(ChargeClass.Program -> 10.sec)
    val discount     = TimeCharge.Discount(
      TimestampInterval.between(t0, t1),
      TimeSpan.Zero,
      10.sec,
      TimeAccounting.comment.NoData
    )
    val noDataEntry  = TimeCharge.DiscountEntry.NoData(discount)
    val invoice      = TimeCharge.Invoice(expExecution, List(noDataEntry), CategorizedTime.Zero)

    for {
      v <- recordVisit(pi, service, mode, visitTime, 1, 0, 0, 250)
      es = events.map { (c, t) => SequenceEvent(EventId, t, v.oid, v.vid, c) }
      _ <- insertEvents(es)
      _ <- withServices(pi) { s => s.session.transaction use { xa => s.timeAccountingService.update(v.vid)(using xa) } }
      _ <- expect(pi, invoiceQuery(v.oid), invoiceExected(invoice, Nil))
    } yield ()

  }

  test("timeChargeInvoice (qa discount)") {

    val ts = (0 until 8).map(_.fromNightStart)

    def events(v: VisitNode): List[ExecutionEvent] = {
      val sid0 = v.atoms.head.steps.head.sid
      val did0 = v.atoms.head.steps.head.dids.head
      val sid1 = v.atoms.last.steps.head.sid
      val did1 = v.atoms.last.steps.head.dids.head

      List(
        StepEvent(   EventId, ts(0), v.oid, v.vid, sid0,       StepStage.StartStep),
        DatasetEvent(EventId, ts(1), v.oid, v.vid, sid0, did0, DatasetStage.StartExpose),
        DatasetEvent(EventId, ts(2), v.oid, v.vid, sid0, did0, DatasetStage.EndWrite),
        StepEvent(   EventId, ts(3), v.oid, v.vid, sid0,       StepStage.EndStep),

        StepEvent(   EventId, ts(4), v.oid, v.vid, sid1,       StepStage.StartStep),
        DatasetEvent(EventId, ts(5), v.oid, v.vid, sid1, did1, DatasetStage.StartExpose),
        DatasetEvent(EventId, ts(6), v.oid, v.vid, sid1, did1, DatasetStage.EndWrite),
        StepEvent(   EventId, ts(7), v.oid, v.vid, sid1,       StepStage.EndStep)
      )
    }

    val expExecution   = CategorizedTime(ChargeClass.Program -> 7.sec)
    val discount       = TimeCharge.Discount(
      TimestampInterval.between(ts(4), ts(7)),
      TimeSpan.Zero,
      3.sec,
      TimeAccounting.comment.Qa
    )
    def qaEntry(v: VisitNode) = TimeCharge.DiscountEntry.Qa(discount, v.atoms.last.steps.head.dids.toSet)
    val expFinalCharge        = CategorizedTime(ChargeClass.Program -> 4.sec)
    def invoice(v: VisitNode) = TimeCharge.Invoice(expExecution, List(qaEntry(v)), expFinalCharge)

    for {
      v <- recordVisit(pi, service, mode, visitTime, 2, 1, 1, 300)
      es = events(v)
      _ <- insertEvents(es)
      _ <- updateDatasets(pi, DatasetQaState.Fail, v.atoms.last.steps.head.dids)
      _ <- withServices(pi) { s => s.session.transaction use { xa => s.timeAccountingService.update(v.vid)(using xa) } }
      _ <- expect(pi, invoiceQuery(v.oid), invoiceExected(invoice(v), Nil))
    } yield ()

  }

  private def correctionTest(
    corrections:  List[TimeChargeCorrectionInput],
    finalCharge: TimeSpan,
    index:       Int
  ): IO[Unit] = {
    val t0 =  0.fromNightStart
    val t1 = 10.fromNightStart

    val events = List(
      (SequenceCommand.Start, t0),
      (SequenceCommand.Stop,  t1)
    )

    val expExecution   = CategorizedTime(ChargeClass.Program -> 10.sec)
    val expFinalCharge = CategorizedTime(ChargeClass.Program -> finalCharge)
    val invoice        = TimeCharge.Invoice(expExecution, Nil, expFinalCharge)

    for {
      v <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, index)
      es = events.map { (c, t) => SequenceEvent(EventId, t, v.oid, v.vid, c) }
      _ <- insertEvents(es)
      _ <- withServices(pi) { s => s.session.transaction use { xa => s.timeAccountingService.update(v.vid)(using xa) } }
      _ <- corrections.traverse_ { c => addTimeChargeCorrection(pi, v.vid, c) }
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
    correctionTest(
      List(TimeChargeCorrectionInput(ChargeClass.Program, TimeChargeCorrection.Op.Add, TimeSpan.Max, "over add".comment)),
      TimeSpan.Max,
      700
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
      v <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 900)
      _ <- addTimeChargeCorrection(pi, v.vid, correction)
      _ <- expect(pi, invoiceQuery(v.oid), invoiceExected(invoice, List(correction)))
    } yield ()
  }

  test("timeChargeInvoice (pre correction subtract)") {
    val expExecution   = CategorizedTime(ChargeClass.Program -> 0.sec)
    val expFinalCharge = CategorizedTime(ChargeClass.Program -> 0.sec)
    val invoice        = TimeCharge.Invoice(expExecution, Nil, expFinalCharge)
    val correction     = TimeChargeCorrectionInput(ChargeClass.Program, TimeChargeCorrection.Op.Subtract, 5.sec, "add".comment)

    for {
      v <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 1000)
      _ <- addTimeChargeCorrection(pi, v.vid, correction)
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
              partner { seconds }
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
    val t0 =  0.fromNightStart
    val t1 = 10.fromNightStart

    val events = List(
      (SequenceCommand.Start, t0),
      (SequenceCommand.Stop,  t1)
    )

    val expected = CategorizedTime(ChargeClass.Program -> 10.sec)

    for {
      v <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 1200)
      es = events.map { (c, t) => SequenceEvent(EventId, t, v.oid, v.vid, c) }
      _ <- insertEvents(es)
      _ <- withServices(pi) { s => s.session.transaction use { xa => s.timeAccountingService.update(v.vid)(using xa) } }
      _ <- expect(pi, observationQuery(v.oid), observationExpectedCharge(expected))
    } yield ()

  }

  test("observation timeCharge, two visits") {
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

    val expected = CategorizedTime(ChargeClass.Program -> 20.sec)

    for {
      v0 <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 1300)
      pid = v0.pid
      oid = v0.oid
      es0 = events0.map { (c, t) => SequenceEvent(EventId, t, oid, v0.vid, c) }
      _ <- insertEvents(es0)
      _ <- withServices(pi) { s => s.session.transaction use { xa => s.timeAccountingService.update(v0.vid)(using xa) } }

      v1 <- recordVisitForObs(pid, oid, service, mode, t2, 1, 1, 1, 1301)
      es1 = events1.map { (c, t) => SequenceEvent(EventId, t, oid, v1.vid, c) }
      _ <- insertEvents(es1)
      _ <- withServices(pi) { s => s.session.transaction use { xa => s.timeAccountingService.update(v1.vid)(using xa) } }

      _ <- expect(pi, observationQuery(oid), observationExpectedCharge(expected))
    } yield ()

  }

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
      _ <- withServices(pi) { s => s.session.transaction use { xa => s.timeAccountingService.update(v0.vid)(using xa) } }

      v1 <- recordVisitForObs(pid, oid, service, mode, t2, 1, 1, 1, 1401)
      es1 = events1.map { (c, t) => SequenceEvent(EventId, t, oid, v1.vid, c) }
      _ <- insertEvents(es1)
      _ <- withServices(pi) { s => s.session.transaction use { xa => s.timeAccountingService.update(v1.vid)(using xa) } }
      _ <- addTimeChargeCorrection(pi, v1.vid, correction)

      _ <- expect(pi, observationQuery(oid), observationExpectedCharge(expected))
    } yield ()

  }

    def programQuery(pid: Program.Id): String =
    s"""
      query {
        program(programId: "$pid") {
          timeCharge {
            program { seconds }
            partner { seconds }
            nonCharged { seconds }
            total { seconds }
          }
        }
      }
    """

  def programExpectedCharge(ct: CategorizedTime): Either[List[String], Json] =
    json"""
      {
        "program": {
          "timeCharge": ${expectedCategorizedTime(ct)}
        }
      }
    """.asRight

  test("program timeCharge, no observations") {
    for {
      p <- createProgramAs(pi)
      _ <- expect(pi, programQuery(p), programExpectedCharge(CategorizedTime.Zero))
    } yield ()
  }

  test("program timeCharge, one observation") {
    val t0 =  0.fromNightStart
    val t1 = 10.fromNightStart

    val events = List(
      (SequenceCommand.Start, t0),
      (SequenceCommand.Stop,  t1)
    )

    val expected = CategorizedTime(ChargeClass.Program -> 10.sec)

    for {
      v <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 1500)
      es = events.map { (c, t) => SequenceEvent(EventId, t, v.oid, v.vid, c) }
      _ <- insertEvents(es)
      _ <- withServices(pi) { s => s.session.transaction use { xa => s.timeAccountingService.update(v.vid)(using xa) } }
      _ <- expect(pi, programQuery(v.pid), programExpectedCharge(expected))
    } yield ()
  }

  test("program timeCharge, two observations") {
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

    val expected = CategorizedTime(ChargeClass.Program -> 20.sec)

    for {
      // Obs0
      v0  <- recordVisit(pi, service, mode, visitTime, 1, 1, 1, 1600)
      pid  = v0.pid
      oid0 = v0.oid
      es0  = events0.map { (c, t) => SequenceEvent(EventId, t, oid0, v0.vid, c) }
      _   <- insertEvents(es0)
      _   <- withServices(pi) { s => s.session.transaction use { xa => s.timeAccountingService.update(v0.vid)(using xa) } }

      // Obs1
      oid1 <- createObservationAs(pi, pid, mode.some)
      v1   <- recordVisitForObs(pid, oid1, service, mode, t2, 1, 1, 1, 1601)
      es1   = events1.map { (c, t) => SequenceEvent(EventId, t, oid1, v1.vid, c) }
      _    <- insertEvents(es1)
      _    <- withServices(pi) { s => s.session.transaction use { xa => s.timeAccountingService.update(v1.vid)(using xa) } }

      _ <- expect(pi, programQuery(pid), programExpectedCharge(expected))
    } yield ()
  }
}
