// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Eq
import cats.MonadError
import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.derived.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import skunk.*
import skunk.implicits.*

import scala.collection.immutable.SortedMap

opaque type GhostReadoutTime = SortedMap[GhostReadoutTime.Key, GhostReadoutTime.Value]

object GhostReadoutTime:

  case class Key(
    readMode: GhostReadMode,
    binning:  GhostBinning
  ) derives Order

  case class Value(
    red:  TimeSpan,
    blue: TimeSpan
  ) derives Eq

  object Value:
    val Zero: Value = Value(TimeSpan.Zero, TimeSpan.Zero)

  extension (m: GhostReadoutTime)
    def apply(key: Key): Value =
      m.getOrElse(key, Value.Zero)

    def apply(readMode: GhostReadMode, binning: GhostBinning): Value =
      apply(Key(readMode, binning))

  private object Statements:

    import lucuma.odb.util.Codecs.*
    import lucuma.odb.util.GhostCodecs.*

    val key: Codec[Key] =
      (ghost_read_mode *: ghost_binning).to[Key]

    val value: Codec[Value] =
      (time_span *: time_span).to[Value]

    val select: Query[Void, (Key, Value)] =
      sql"""
        SELECT
          c_read_mode,
          c_binning,
          c_red_time,
          c_blue_time
        FROM t_ghost_readout
      """.query(key *: value)

  def load[F[_]](s: Session[F])(using MonadError[F, Throwable]): F[GhostReadoutTime] =
    def validate(m: GhostReadoutTime): F[Unit] =
      (
        for
          r <- Enumerated[GhostReadMode].all
          b <- Enumerated[GhostBinning].all
          k  = Key(r, b)
        yield m.get(k).liftTo[F](new RuntimeException(s"Missing GHOST readout time definition for $k"))
      ).sequence.void

    s.execute(Statements.select)
     .map(SortedMap.from)
     .flatTap(validate)