// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.Eq
import cats.syntax.eq.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.Target

/**
 * When the generator cannot find an observation or target parameter that it
 * needs, a MissingParam is created to record it.
 */
sealed trait MissingParam:
  def name: String
  def format: String

object MissingParam:
  case class MissingObservationParam(name: String) extends MissingParam:
    def format: String = name

  case class MissingTargetParam(targetId: Target.Id, name: String) extends MissingParam:
    def format: String = s"target $targetId: $name"

  given Eq[MissingParam] =
    Eq.instance {
      case (MissingObservationParam(p0), MissingObservationParam(p1)) => p0 === p1
      case (MissingTargetParam(t0, p0), MissingTargetParam(t1, p1))   => (t0 === t1) && (p0 === p1)
      case _                                                          => false
    }

  def forObservation(param: String): MissingParam =
    MissingObservationParam(param)

  def forTarget(targetId: Target.Id, param: String): MissingParam =
    MissingTargetParam(targetId, param)

  given Decoder[MissingParam] =
    Decoder.instance: c =>
      for
        n <- c.downField("name").as[String]
        t <- c.downField("targetId").as[Option[Target.Id]]
      yield t.fold(MissingObservationParam(n))(MissingTargetParam(_, n))

  given Encoder[MissingParam] =
    Encoder.instance:
      case MissingObservationParam(n) => Json.obj("name" -> n.asJson)
      case MissingTargetParam(t, n)   => Json.obj("targetId" -> t.asJson, "name" -> n.asJson)