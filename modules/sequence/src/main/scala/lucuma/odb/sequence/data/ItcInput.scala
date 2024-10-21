// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.Eq
import cats.Order.given
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import lucuma.core.model.Target
import lucuma.itc.client.ImagingIntegrationTimeInput
import lucuma.itc.client.ImagingIntegrationTimeParameters
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.itc.client.SpectroscopyIntegrationTimeParameters
import lucuma.itc.client.TargetInput
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.HashBytes

import scala.collection.mutable.ArrayBuilder

object ItcInput:

  sealed trait Param:
    def format: String

  object Param:
    case class ObsParam(param: String) extends Param:
      def format: String =
        param

    case class TargetParam(targetId: Target.Id, param: String) extends Param:
      def format: String =
        s"target $targetId: $param"

    given Eq[Param] =
      Eq.instance {
        case (ObsParam(p0), ObsParam(p1))               => p0 === p1
        case (TargetParam(t0, p0), TargetParam(t1, p1)) => (t0 === t1) && (p0 === p1)
        case _                                          => false
      }

  def missingObsParam(param: String): Param =
    Param.ObsParam(param)

  def missingTargetParam(targetId: Target.Id, param: String): Param =
    Param.TargetParam(targetId, param)

  opaque type Missing = NonEmptyList[Param]

  object Missing:
    def fromParams(params: NonEmptyList[Param]): Missing =
      params

    extension (m: Missing)
      def params: NonEmptyList[Param] =
        m

      def format: String =
        val grouped = m.groupMapReduceWith {
          case Param.ObsParam(_) => none[Target.Id]
          case Param.TargetParam(t, _)   => t.some
        } {
          case Param.ObsParam(p) => p
          case Param.TargetParam(_, p)   => p
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

  end Missing

  case class Defined(
    imaging:      ImagingIntegrationTimeParameters,
    spectroscopy: SpectroscopyIntegrationTimeParameters,
    targets:      NonEmptyList[(Target.Id, TargetInput)]
  ):
    lazy val imagingInput: ImagingIntegrationTimeInput =
      ImagingIntegrationTimeInput(imaging, targets.map(_._2))

    lazy val spectroscopyInput: SpectroscopyIntegrationTimeInput =
      SpectroscopyIntegrationTimeInput(spectroscopy, targets.map(_._2))

    lazy val targetVector: NonEmptyVector[(Target.Id, TargetInput)] =
      targets.toNev

  object Defined:

    given Eq[Defined] =
      Eq.by { a => (a.imaging, a.spectroscopy, a.targets) }

    given HashBytes[Defined] with
      given HashBytes[TargetInput]                           = HashBytes.forJsonEncoder
      given HashBytes[ImagingIntegrationTimeParameters]      = HashBytes.forJsonEncoder
      given HashBytes[SpectroscopyIntegrationTimeParameters] = HashBytes.forJsonEncoder

      def hashBytes(a: Defined): Array[Byte] =
        def targetsBytes(
          targets: NonEmptyList[(Target.Id, TargetInput)]
        ): Array[Byte] =
          val bld = ArrayBuilder.make[Byte]
          targets.toList.sortBy(_._1).foreach: (tid, tinput) =>
            bld.addAll(tid.hashBytes)
            bld.addAll(tinput.hashBytes)
          bld.result()

        Array.concat(
          a.imaging.hashBytes,
          a.spectroscopy.hashBytes,
          targetsBytes(a.targets)
        )
  end Defined