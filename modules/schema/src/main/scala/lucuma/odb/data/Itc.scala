// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
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

/**
 * ITC result data.  Because the results differ depending upon whether it is
 * imaging or spectroscopy, and which particular instrument is in use, there
 * are specific types for each case.
 */
sealed trait Itc:

  // Used in the circe decoder.  Itc results are stored in a jsonb column.
  def dataType: Itc.Type

  def scienceExposureCount: PosInt

object Itc:

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

  // ITC result type discriminator.
  enum Type(val tag: String) derives Enumerated:
    case GmosNorthImaging extends Type("gmos_north_imaging")
    case GmosSouthImaging extends Type("gmos_south_imaging")
    case Spectroscopy     extends Type("spectroscopy")

  /**
   * GMOS North imaging results.  There are results per-GMOS North filter.
   */
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

  object GmosNorthImaging:
    given Eq[GmosNorthImaging] =
      Eq.by(_.science)

  val gmosNorthImaging: Prism[Itc, GmosNorthImaging] =
    GenPrism[Itc, GmosNorthImaging]

  /**
   * GMOS South imaging results.  There are results per-GMOS South filter.
   */
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

  object GmosSouthImaging:
    given Eq[GmosSouthImaging] =
      Eq.by(_.science)

  val gmosSouthImaging: Prism[Itc, GmosSouthImaging] =
    GenPrism[Itc, GmosSouthImaging]

  /**
   * Spectroscopy results for all instruments. Spectroscopy has separate
   * acquisition and science results.
   */
  case class Spectroscopy(
    acquisition: Zipper[Result],
    science:     Zipper[Result]
  ) extends Itc:

    override def dataType: Type =
      Type.Spectroscopy

    override def scienceExposureCount: PosInt =
      science.focus.value.exposureCount

  object Spectroscopy:
    given Eq[Spectroscopy] =
      Eq.by: a =>
        (
          a.acquisition,
          a.science
        )

  val spectroscopy: Prism[Itc, Spectroscopy] =
    GenPrism[Itc, Spectroscopy]

  given Eq[Itc] =
    Eq.instance {
      case (a: GmosNorthImaging, b: GmosNorthImaging) => a === b
      case (a: GmosSouthImaging, b: GmosSouthImaging) => a === b
      case (a: Spectroscopy,     b: Spectroscopy    ) => a === b
      case _                                          => false
    }