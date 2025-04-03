// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.MonadError
import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Tag
import monocle.Focus
import monocle.Lens
import skunk.*
import skunk.implicits.*

import scala.collection.immutable.SortedMap


opaque type GmosReadoutTime = SortedMap[GmosReadoutTime.Key, TimeSpan]


object GmosReadoutTime {

  case class Key(
    detector: Either[GmosNorthDetector, GmosSouthDetector],
    ccdMode: GmosCcdMode,
    roi:     GmosRoi
  )

  object Key {
    val detector: Lens[Key, Either[GmosNorthDetector, GmosSouthDetector]] =
      Focus[Key](_.detector)

    val ccdMode: Lens[Key, GmosCcdMode] =
      Focus[Key](_.ccdMode)

    val roi: Lens[Key, GmosRoi] =
      Focus[Key](_.roi)

    given Order[Key] =
      Order.by { a => (
        a.detector,
        a.ccdMode,
        a.roi
      )}

  }

  extension (m: GmosReadoutTime) {

    def apply(key: GmosReadoutTime.Key): TimeSpan =
      m.getOrElse(key, TimeSpan.Zero)

    def apply(detector: GmosNorthDetector, gn: DynamicConfig.GmosNorth): TimeSpan =
      apply(GmosReadoutTime.Key(detector.asLeft, gn.readout, gn.roi))

    def apply(detector: GmosSouthDetector, gs: DynamicConfig.GmosSouth): TimeSpan =
      apply(GmosReadoutTime.Key(detector.asRight, gs.readout, gs.roi))

  }

  private object statements {

   import lucuma.odb.util.Codecs.*
   import lucuma.odb.util.GmosCodecs.*

    val select: Query[Void, (Tag, GmosCcdMode, GmosRoi, TimeSpan)] =
      sql"""
        SELECT
          c_detector,
          c_x_bin,
          c_y_bin,
          c_amp_count,
          c_amp_gain,
          c_amp_read_mode,
          c_roi,
          c_time
        FROM
          t_gmos_readout
      """.query(tag *: gmos_ccd_mode *: gmos_roi *: time_span)
  }

  def load[F[_]](s: Session[F])(using MonadError[F, Throwable]): F[GmosReadoutTime] = {

    val e2v: List[Either[GmosNorthDetector, GmosSouthDetector]] =
      List(GmosNorthDetector.E2V.asLeft, GmosSouthDetector.E2V.asRight)

    val hamamatsu: List[Either[GmosNorthDetector, GmosSouthDetector]] =
      List(GmosNorthDetector.Hamamatsu.asLeft, GmosSouthDetector.Hamamatsu.asRight)

    def keys(d: Tag, c: GmosCcdMode, r: GmosRoi, t: TimeSpan): F[List[(Key, TimeSpan)]] =
      d.value match {
        case "E2V"       => e2v.map       { d => (Key(d, c, r), t) }.pure
        case "HAMAMATSU" => hamamatsu.map { d => (Key(d, c, r), t) }.pure
        case _           => MonadError[F, Throwable].raiseError(new RuntimeException(s"Unexpected GMOS detector '${d.value}'"))
      }

    def readoutMap(lst: List[(Key, TimeSpan)]): GmosReadoutTime =
      // TODO: For now treat "Custom" ROI as if it were full frame.  When we have
      // TODO: custom ROI definitions in the sequence we can apply the old `ocs`
      // TODO: logic:
      /*
        final BuiltinROI roiKey = (builtinROI == BuiltinROI.CUSTOM) ? BuiltinROI.FULL_FRAME : builtinROI;
        final GmosReadoutKey key = new GmosReadoutKey(ampCount, ampReadMode, roiKey, xBin.getValue(), yBin.getValue(), ampGain, detMan);
        final Double d  = map.get(key);
        double overhead = (d == null) ? 0 : d;
        if (builtinROI == BuiltinROI.CUSTOM) {
            final int rows = customRoiList.totalUnbinnedRows();
            if (rows > 0) {
                overhead = 1 + overhead * rows / detMan.getYsize();
            }
        }
       */
      // val m = SortedMap.from(lst)
      lst.foldLeft(SortedMap.empty[Key, TimeSpan]) { case (m, (k, t)) =>
        val mʹ = m.updated(k, t)
        k.roi match {
          case GmosRoi.FullFrame => mʹ.updated(Key.roi.replace(GmosRoi.Custom)(k), t)
          case _                 => mʹ
        }
      }

    def validate(m: GmosReadoutTime): F[Unit] =
      (
        for {
          d <- GmosNorthDetector.all.map(_.asLeft[GmosSouthDetector]) ++ GmosSouthDetector.values.map(_.asRight[GmosNorthDetector])
          x <- GmosXBinning.values
          y <- GmosYBinning.values
          c <- d.fold(_.ampCounts.toNonEmptyList.toList, _.ampCounts.toNonEmptyList.toList)
          g <- GmosAmpGain.all
          o <- GmosAmpReadMode.all
          r <- GmosRoi.all
          k  = Key(d, GmosCcdMode(x, y, c, g, o), r)
        } yield m.get(k).liftTo[F](new RuntimeException(s"Missing GMOS readout time definition for $k"))
      ).sequence.void

    s.execute(statements.select)
     .flatMap(_.flatTraverse { case (tag, ccd, roi, time) => keys(tag, ccd, roi, time) })
     .map(readoutMap)
     .flatMap(m => validate(m).as(m))

  }

}
