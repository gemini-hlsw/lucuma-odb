// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Order
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.data.Zipper
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.model.Target
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.SignalToNoiseAt
import monocle.Prism
import monocle.macros.GenPrism

sealed trait Itc:

  // Used in the circe decoder.  Itc results are stored in a jsonb column.
  def dataType: Itc.Type

  def scienceExposureCount: PosInt

object Itc:

  enum Type(val tag: String) derives Enumerated:
    case GmosNorthImaging extends Type("gmos_north_imaging")
    case GmosSouthImaging extends Type("gmos_south_imaging")
    case Spectroscopy     extends Type("spectroscopy")

  case class GmosNorthImaging(
    science: NonEmptyMap[GmosNorthFilter, Zipper[Result]]
  ) extends Itc:

    override def dataType: Type =
      Type.GmosNorthImaging

    override def scienceExposureCount: PosInt =
      PosInt.unsafeFrom:
        science.foldLeft(0) { (cnt, z) =>
          cnt + z.focus.value.exposureCount.value
        }

  val gmosNorthImaging: Prism[Itc, GmosNorthImaging] =
    GenPrism[Itc, GmosNorthImaging]

  case class GmosSouthImaging(
    science: NonEmptyMap[GmosSouthFilter, Zipper[Result]]
  ) extends Itc:

    override def dataType: Type =
      Type.GmosSouthImaging

    override def scienceExposureCount: PosInt =
      PosInt.unsafeFrom:
        science.foldLeft(0) { (cnt, z) =>
          cnt + z.focus.value.exposureCount.value
        }

  val gmosSouthImaging: Prism[Itc, GmosSouthImaging] =
    GenPrism[Itc, GmosSouthImaging]

  case class Spectroscopy(
    acquisition: Zipper[Result],
    science:     Zipper[Result]
  ) extends Itc:

    override def dataType: Type =
      Type.Spectroscopy

    override def scienceExposureCount: PosInt =
      science.focus.value.exposureCount

  val spectroscopy: Prism[Itc, Spectroscopy] =
    GenPrism[Itc, Spectroscopy]

  // Corresponds to ItcResult in the OdbSchema.graphql
  case class Result(
    targetId:      Target.Id,
    value:         IntegrationTime,
    signalToNoise: Option[SignalToNoiseAt]
  ):
    def totalTime: Option[TimeSpan] =
      val total = BigInt(value.exposureTime.toMicroseconds) * value.exposureCount.value
      Option.when(total.isValidLong)(TimeSpan.fromMicroseconds(total.longValue)).flatten

  object Result:
    given Order[Result] =
      Order.by(s => (s.totalTime, s.targetId))

//  private def targetIds(z: Zipper[Result]): NonEmptyList[Target.Id] =
//    z.toNel.map(_.targetId).sortBy(_.value)
//
//  def fromResults(
//    modeData:    ModeData,
//    acquisition: Option[Zipper[Result]],
//    science:     NonEmptyList[Zipper[Result]]
//  ): Option[Itc] =
//    Option.when(acquisition.forall(z => targetIds(z) === targetIds(science)))(
//      Itc(acquisition.map(ResultSet(modeData, _)), science.map(ResultSet(modeData, _))
//    )