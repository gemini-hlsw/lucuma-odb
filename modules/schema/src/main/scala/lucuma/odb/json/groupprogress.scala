// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import io.circe.ACursor
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.GroupConstraintViolation
import lucuma.core.enums.GroupStatus
import lucuma.core.model.group.GroupProgress
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval

trait GroupProgressCodec:

  // A GroupWindow: an end of Timestamp.Max is no end, and so null.
  private def windowJson(w: TimestampInterval): Json =
    Json.obj(
      "start" -> w.start.asJson,
      "end"   -> Option.when(w.end =!= Timestamp.Max)(w.end).asJson
    )

  private def decodeWindow(c: ACursor): Decoder.Result[TimestampInterval] =
    for
      s <- c.downField("start").as[Timestamp]
      e <- c.downField("end").as[Option[Timestamp]]
      w <- Either.cond(
             e.forall(s <= _),
             TimestampInterval.between(s, e.getOrElse(Timestamp.Max)),
             DecodingFailure(s"Group window ends ($e) before it starts ($s)", c.history)
           )
    yield w

  // The GraphQL type is flat: the fields that apply to the status are read
  // and the rest ignored.  Every status but empty requires at least one
  // element.
  given Decoder_GroupProgress: Decoder[GroupProgress] =
    Decoder.instance: c =>
      def required: Decoder.Result[PosInt] =
        c.downField("required").as[PosInt]

      for
        s <- c.downField("status").as[GroupStatus]
        d <- c.downField("completed").as[NonNegInt]
        p <- s match
               case GroupStatus.Empty         =>
                 GroupProgress.Empty.asRight
               case GroupStatus.Pending       =>
                 required.map(GroupProgress.Pending(_))
               case GroupStatus.Ongoing       =>
                 for
                   r <- required
                   w <- c.downField("nextWindow").as[Option[Json]].flatMap(_.traverse(j => decodeWindow(j.hcursor)))
                 yield GroupProgress.Ongoing(d, r, w)
               case GroupStatus.Complete      =>
                 required.map(GroupProgress.Complete(d, _))
               case GroupStatus.Unschedulable =>
                 for
                   r  <- required
                   vs <- c.downField("violations").as[List[GroupConstraintViolation]]
                   ns <- NonEmptyList.fromList(vs).toRight(DecodingFailure("An unschedulable group has no violations", c.history))
                 yield GroupProgress.Unschedulable(d, r, ns.toNes)
      yield p

  // The GraphQL type is flat: fields that don't apply to a status are zero,
  // null or empty.
  given Encoder_GroupProgress: Encoder[GroupProgress] =
    Encoder.instance: p =>
      val (completed, required, nextWindow, violations) =
        p match
          case GroupProgress.Empty                  => (0, 0, None, Nil)
          case GroupProgress.Pending(r)             => (0, r.value, None, Nil)
          case GroupProgress.Ongoing(c, r, w)       => (c.value, r.value, w, Nil)
          case GroupProgress.Complete(c, r)         => (c.value, r.value, None, Nil)
          case GroupProgress.Unschedulable(c, r, v) => (c.value, r.value, None, v.toSortedSet.toList)
      Json.obj(
        "status"     -> p.status.asJson,
        "completed"  -> completed.asJson,
        "required"   -> required.asJson,
        "nextWindow" -> nextWindow.map(windowJson).asJson,
        "violations" -> violations.asJson
      )

object groupprogress extends GroupProgressCodec
