// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.Order.catsKernelOrderingForOrder
import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import io.circe.Encoder
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType.Nautical
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.ExecutionEvent.*
import lucuma.core.model.Observation
import lucuma.core.model.ObservingNight
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.ObservingModeType
import lucuma.odb.data.TimeCharge
import lucuma.odb.service.TimeAccounting
import lucuma.odb.util.Codecs.*
import skunk.syntax.all.*

import java.time.LocalDateTime
import java.time.Month
import scala.collection.immutable.SortedSet

class timeAccounting extends OdbSuite with DatabaseOperations { this: OdbSuite =>

  val pi         = TestUsers.Standard.pi(1, 30)
  val mode       = ObservingModeType.GmosSouthLongSlit

  val EventId    = ExecutionEvent.Id.fromLong(1).get  // we'll just share one
  val visitLdt   = LocalDateTime.of(2023, Month.DECEMBER, 19, 23, 0)
  val visitTime  = Timestamp.unsafeFromLocalDateTime(visitLdt)
  val night      = ObservingNight.fromSiteAndLocalDateTime(Site.GS, visitLdt)
  val tbn        = night.twilightBoundedUnsafe(Nautical)
  val tbnStart   = Timestamp.unsafeFromInstantTruncated(tbn.start)

  val validUsers = List(pi).toList

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
        case TimeCharge.DiscountEntry.Daylight(_, s) => obj.add("site", s.asJson).toJson
        case _ => obj.toJson
      }
    }

  def invoiceExected(
    invoices: List[TimeCharge.Invoice]
  ): Either[List[String], Json] = {

    val matches = invoices.map { inv =>
      json"""
      {
        "timeChargeInvoice": {
          "executionTime": ${expectedCategorizedTime(inv.executionTime)},
          "discounts": ${inv.discounts.map(expectedDiscount).asJson},
          "finalCharge": ${expectedCategorizedTime(inv.finalCharge)}
        }
      }
      """
    }.asJson

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
    mode:         ObservingModeType,
    when:         Timestamp,
    atomCount:    Int,
    stepCount:    Int,
    datasetCount: Int,
    idx:          Int
  ): IO[VisitNode] =

    for {
      pid   <- createProgramAs(user)
      oid   <- createObservationAs(user, pid, mode.some)
      vid   <- recordVisitAs(user, mode.instrument, oid)
      _     <- setVisitTime(vid, when)
      atoms <- (0 until atomCount).toList.traverse { a => recordAtom(user, mode, vid, stepCount, datasetCount, idx + a * stepCount * datasetCount) }
    } yield VisitNode(oid, vid, atoms)

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

  def insertEvents(
    events: List[SequenceEvent]
  ): IO[Unit] =
    events.traverse(insertSequenceEvent).void

  test("observation -> execution -> visits -> timeChargeInvoice (no events)") {
    recordVisit(pi, mode, visitTime, 1, 1, 1, 0).flatMap { v =>
      expect(pi, invoiceQuery(v.oid), invoiceExected(List(TimeCharge.Invoice.Empty)))
    }
  }

  val oneSecond: TimeSpan = TimeSpan.FromMicroseconds.getOption(1_000_000L).get
  val twoSecond: TimeSpan = TimeSpan.FromMicroseconds.getOption(2_000_000L).get

  test("observation -> execution -> visits -> timeChargeInvoice (sequence events)") {

    val t0 = Timestamp.unsafeFromInstantTruncated(tbn.start).plusMillisOption(-1000).get
    val t1 = Timestamp.unsafeFromInstantTruncated(tbn.start).plusMillisOption( 1000).get

    val events = List(
      (SequenceCommand.Start, t0),
      (SequenceCommand.Stop,  t1)
    )

    val expExecution   = CategorizedTime(ChargeClass.Program -> twoSecond)
    val discount       = TimeCharge.Discount(
      TimestampInterval.between(t0, tbnStart),
      TimeSpan.Zero,
      oneSecond,
      SortedSet.empty,
      TimeAccounting.comment.PreDusk
    )
    val daylightEntry  = TimeCharge.DiscountEntry.Daylight(discount, Site.GS)
    val expFinalCharge = CategorizedTime(ChargeClass.Program -> oneSecond)
    val invoice        = TimeCharge.Invoice(expExecution, List(daylightEntry), expFinalCharge)

    for {
      v <- recordVisit(pi, mode, visitTime, 1, 1, 1, 10)
      es = events.map { (c, t) => SequenceEvent(EventId, t, v.oid, v.vid, c) }
      _ <- insertEvents(es)
      _ <- withServices(pi) { s => s.session.transaction use { xa => s.timeAccountingService.update(v.vid)(using xa) } }
      _ <- expect(pi, invoiceQuery(v.oid), invoiceExected(List(invoice)))
    } yield ()

  }

}
