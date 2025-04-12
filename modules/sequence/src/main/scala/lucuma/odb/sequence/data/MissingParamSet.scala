// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.syntax.*
import lucuma.core.model.Target
import lucuma.odb.sequence.util.HashBytes

import MissingParam.*

/**
 * A collection of MissingParam
 */
opaque type MissingParamSet = NonEmptyList[MissingParam]

object MissingParamSet:
  def fromParams(params: NonEmptyList[MissingParam]): MissingParamSet =
    params

  extension (m: MissingParamSet)
    def params: NonEmptyList[MissingParam] =
      m

    /**
     * Formats a collection of missing parameters, grouping those for the
     * observation as a whole, and those for particular targets and producing
     * a descriptive message.
     */
    def format: String =
      val grouped = m.groupMapReduceWith {
        case MissingObservationParam(_) => none[Target.Id]
        case MissingTargetParam(t, _)   => t.some
      } {
        case MissingObservationParam(p) => p
        case MissingTargetParam(_, p)   => p
      } { (p0, p1) =>
        s"$p0, $p1"
      }
      val o = grouped.get(none).map(ps => s"observation is missing $ps")
      val t = grouped.toList.collect {
        case (Some(t), ps) => s"target $t is missing $ps"
      }
      o.fold(t.intercalate(", ")) { s =>
        t match
          case Nil => s
          case _   => s"$s, ${t.intercalate(", ")}"
      }

  given Eq[MissingParamSet] with
    def eqv(x: MissingParamSet, y: MissingParamSet): Boolean =
      x.params === y.params

  given HashBytes[MissingParamSet] with
    def hashBytes(a: MissingParamSet): Array[Byte] =
      Array.emptyByteArray

  given Decoder[MissingParamSet] =
    Decoder.instance: c =>
      c.values.toList.flatMap(_.toList).traverse(_.as[MissingParam]).flatMap:
        case Nil     => DecodingFailure("Empty MissingParamSet", c.history).asLeft
        case p :: ps => MissingParamSet.fromParams(NonEmptyList(p, ps)).asRight

  given Encoder[MissingParamSet] =
    Encoder.instance: a =>
      a.toList.map(_.asJson).asJson
