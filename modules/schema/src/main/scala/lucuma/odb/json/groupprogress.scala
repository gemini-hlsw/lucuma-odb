// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.ACursor
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.GroupConstraintViolationCode
import lucuma.core.enums.GroupStatus
import lucuma.core.enums.Site
import lucuma.core.model.Group
import lucuma.core.model.LocalObservingNight
import lucuma.core.model.Observation
import lucuma.core.model.ObservingNight
import lucuma.core.model.User
import lucuma.core.model.group.GroupConstraintViolation
import lucuma.core.model.group.GroupConstraintWaiver
import lucuma.core.model.group.GroupProgress
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval

import java.time.LocalDate

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

  // A GroupElementId: exactly one of the two ids is set.
  private def elementJson(e: Either[Group.Id, Observation.Id]): Json =
    Json.obj(
      "groupId"       -> e.left.toOption.asJson,
      "observationId" -> e.toOption.asJson
    )

  private def decodeElement(c: ACursor): Decoder.Result[Either[Group.Id, Observation.Id]] =
    (c.downField("groupId").as[Option[Group.Id]], c.downField("observationId").as[Option[Observation.Id]]).tupled.flatMap:
      case (Some(g), None) => g.asLeft.asRight
      case (None, Some(o)) => o.asRight.asRight
      case _               => DecodingFailure("A group element needs exactly one of groupId and observationId", c.history).asLeft

  // A GroupNight.
  private def nightJson(n: ObservingNight): Json =
    Json.obj(
      "site" -> n.site.asJson,
      "date" -> n.toLocalObservingNight.toLocalDate.asJson
    )

  private def decodeNight(c: ACursor): Decoder.Result[ObservingNight] =
    for
      s <- c.downField("site").as[Site]
      d <- c.downField("date").as[LocalDate]
    yield ObservingNight(s, LocalObservingNight(d))

  // A GroupConstraintViolation: the fields that don't apply to its code are
  // null.
  given Encoder_GroupConstraintViolation: Encoder[GroupConstraintViolation] =
    Encoder.instance: v =>
      import GroupConstraintViolation.Unwaivable.*
      import GroupConstraintViolation.Waivable.*
      val (element, night, subgroup) =
        v match
          case Order(e)                       => (e.some, none, none)
          case MinimumInterval(e)             => (e.some, none, none)
          case MaximumInterval(e)             => (e.some, none, none)
          case SameNight(n)                   => (none, n.some, none)
          case ObservationsPerNightMinimum(n) => (none, n.some, none)
          case ObservationsPerNightMaximum(n) => (none, n.some, none)
          case SubgroupUnschedulable(g)       => (none, none, g.some)
          case SameNightMultipleSites            |
               ObservationsPerNightMultipleSites |
               InsufficientElements              => (none, none, none)
      Json.obj(
        "code"       -> v.code.asJson,
        "element"    -> element.map(elementJson).asJson,
        "night"      -> night.map(nightJson).asJson,
        "subgroupId" -> subgroup.asJson
      )

  given Decoder_GroupConstraintViolation: Decoder[GroupConstraintViolation] =
    Decoder.instance: c =>
      import GroupConstraintViolationCode as Code
      import GroupConstraintViolation.Unwaivable
      import GroupConstraintViolation.Waivable
      def element = decodeElement(c.downField("element"))
      def night   = decodeNight(c.downField("night"))
      c.downField("code").as[GroupConstraintViolationCode].flatMap:
        case Code.Order                             => element.map(Waivable.Order(_))
        case Code.MinimumInterval                   => element.map(Waivable.MinimumInterval(_))
        case Code.MaximumInterval                   => element.map(Waivable.MaximumInterval(_))
        case Code.SameNight                         => night.map(Waivable.SameNight(_))
        case Code.SameNightMultipleSites            => Unwaivable.SameNightMultipleSites.asRight
        case Code.ObservationsPerNightMinimum       => night.map(Waivable.ObservationsPerNightMinimum(_))
        case Code.ObservationsPerNightMaximum       => night.map(Waivable.ObservationsPerNightMaximum(_))
        case Code.ObservationsPerNightMultipleSites => Unwaivable.ObservationsPerNightMultipleSites.asRight
        case Code.InsufficientElements              => Unwaivable.InsufficientElements.asRight
        case Code.SubgroupUnschedulable             => c.downField("subgroupId").as[Group.Id].map(Unwaivable.SubgroupUnschedulable(_))

  given Encoder_GroupConstraintWaiver: Encoder[GroupConstraintWaiver] =
    Encoder.instance: w =>
      Json.obj(
        "id"        -> w.id.asJson,
        "groupId"   -> w.groupId.asJson,
        "violation" -> (w.violation: GroupConstraintViolation).asJson,
        "reason"    -> w.reason.asJson,
        "grantedBy" -> w.grantedBy.asJson,
        "grantedAt" -> w.grantedAt.asJson,
        "revokedBy" -> w.revoked.map(_.revokedBy).asJson,
        "revokedAt" -> w.revoked.map(_.revokedAt).asJson
      )

  given Decoder_GroupConstraintWaiver: Decoder[GroupConstraintWaiver] =
    Decoder.instance: c =>
      for
        id <- c.downField("id").as[GroupConstraintWaiver.Id]
        g  <- c.downField("groupId").as[Group.Id]
        v  <- c.downField("violation").as[GroupConstraintViolation].flatMap:
                case w: GroupConstraintViolation.Waivable => w.asRight
                case u                                    => DecodingFailure(s"A ${u.code} violation can't be waived", c.history).asLeft
        r  <- c.downField("reason").as[NonEmptyString]
        gb <- c.downField("grantedBy").as[User.Id]
        ga <- c.downField("grantedAt").as[Timestamp]
        rb <- c.downField("revokedBy").as[Option[User.Id]]
        ra <- c.downField("revokedAt").as[Option[Timestamp]]
        rv <- (rb, ra) match
                case (Some(b), Some(a)) => GroupConstraintWaiver.Revocation(b, a).some.asRight
                case (None, None)       => none.asRight
                case _                  => DecodingFailure("A revoked waiver needs both revokedBy and revokedAt", c.history).asLeft
      yield GroupConstraintWaiver(id, g, v, r, gb, ga, rv)

  // The GraphQL type is flat: the fields that apply to the status are read
  // and the rest ignored.  Every status but empty requires at least one
  // element.
  given Decoder_GroupProgress: Decoder[GroupProgress] =
    Decoder.instance: c =>
      def required: Decoder.Result[PosInt] =
        c.downField("required").as[PosInt]

      // Empty when not selected.
      def waived: Decoder.Result[List[GroupConstraintWaiver]] =
        c.downField("waived").as[Option[List[GroupConstraintWaiver]]].map(_.orEmpty)

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
                   r  <- required
                   w  <- c.downField("nextWindow").as[Option[Json]].flatMap(_.traverse(j => decodeWindow(j.hcursor)))
                   ws <- waived
                 yield GroupProgress.Ongoing(d, r, w, ws)
               case GroupStatus.Complete      =>
                 (required, waived).mapN(GroupProgress.Complete(d, _, _))
               case GroupStatus.Unschedulable =>
                 for
                   r  <- required
                   vs <- c.downField("violations").as[List[GroupConstraintViolation]]
                   ns <- NonEmptyList.fromList(vs).toRight(DecodingFailure("An unschedulable group has no violations", c.history))
                   ws <- waived
                 yield GroupProgress.Unschedulable(d, r, ns, ws)
      yield p

  // The GraphQL type is flat: fields that don't apply to a status are zero,
  // null or empty.
  given Encoder_GroupProgress: Encoder[GroupProgress] =
    Encoder.instance: p =>
      val (completed, required, nextWindow, violations) =
        p match
          case GroupProgress.Empty                         => (0, 0, None, Nil)
          case GroupProgress.Pending(r)                    => (0, r.value, None, Nil)
          case GroupProgress.Ongoing(c, r, w, _)           => (c.value, r.value, w, Nil)
          case GroupProgress.Complete(c, r, _)             => (c.value, r.value, None, Nil)
          case GroupProgress.Unschedulable(c, r, vs, _)    => (c.value, r.value, None, vs.toList)
      Json.obj(
        "status"     -> p.status.asJson,
        "completed"  -> completed.asJson,
        "required"   -> required.asJson,
        "nextWindow" -> nextWindow.map(windowJson).asJson,
        "violations" -> violations.asJson,
        "waived"     -> p.waived.asJson
      )

object groupprogress extends GroupProgressCodec
