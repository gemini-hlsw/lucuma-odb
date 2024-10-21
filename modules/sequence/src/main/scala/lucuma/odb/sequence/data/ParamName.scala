// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import lucuma.core.model.Target
import lucuma.odb.sequence.util.HashBytes

sealed trait ParamName:
  def name: String
  def format: String

object ParamName:
  case class ObservationParamName(name: String) extends ParamName:
    def format: String = name

  case class TargetParamName(targetId: Target.Id, name: String) extends ParamName:
    def format: String = s"target $targetId: $name"

  given Eq[ParamName] =
    Eq.instance {
      case (ObservationParamName(p0), ObservationParamName(p1)) => p0 === p1
      case (TargetParamName(t0, p0), TargetParamName(t1, p1))   => (t0 === t1) && (p0 === p1)
      case _                                                    => false
    }

  def forObservation(param: String): ParamName =
    ObservationParamName(param)

  def forTarget(targetId: Target.Id, param: String): ParamName =
    TargetParamName(targetId, param)

  opaque type Missing = NonEmptyList[ParamName]

  object Missing:
    def fromParams(params: NonEmptyList[ParamName]): Missing =
      params

    extension (m: Missing)
      def params: NonEmptyList[ParamName] =
        m

      def format: String =
        val grouped = m.groupMapReduceWith {
          case ObservationParamName(_) => none[Target.Id]
          case TargetParamName(t, _)   => t.some
        } {
          case ObservationParamName(p) => p
          case TargetParamName(_, p)   => p
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

    given Eq[Missing] with
      def eqv(x: Missing, y: Missing): Boolean =
        x.params === y.params

    given HashBytes[Missing] with
      def hashBytes(a: Missing): Array[Byte] =
        Array.emptyByteArray