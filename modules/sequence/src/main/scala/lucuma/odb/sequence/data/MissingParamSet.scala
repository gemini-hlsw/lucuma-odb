// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.foldable.*
import cats.syntax.option.*
import lucuma.core.model.Target
import lucuma.odb.sequence.util.HashBytes

import MissingParam.*

opaque type MissingParamSet = NonEmptyList[MissingParam]

object MissingParamSet:
  def fromParams(params: NonEmptyList[MissingParam]): MissingParamSet =
    params

  extension (m: MissingParamSet)
    def params: NonEmptyList[MissingParam] =
      m

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
      val o = grouped.get(none).map(ps => s"observation is missing: { $ps }")
      val t = grouped.toList.collect {
        case (Some(t), ps) => s"target $t is missing: { $ps }"
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
