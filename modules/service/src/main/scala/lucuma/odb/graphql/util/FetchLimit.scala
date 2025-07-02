// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.util

import cats.effect.Async
import cats.effect.Resource
import cats.syntax.all.*
import grackle.Result
import grackle.skunk.SkunkMapping
import scala.util.control.NonFatal
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import java.util.UUID
import java.time.Instant
import io.circe.Json
import java.time.LocalDate

trait FetchLimit[F[_]: Async](maxBytes: Long) extends SkunkMapping[F]:

  case class FetchLimitError(maxBytes: Long) extends Exception(s"Fetch limit ($maxBytes bytes) exceeded.")

  /** 
   * Compute an approximate size for the given object. This is only used for values pulled from
   * the database so we only have to consider types for which we have codecs. The list is incomplete
   * but it's not really a big deal.
   */
  private def approximateSize(a: Any): Long =
    a match
      case _: Byte  | _: Boolean | 
           _: Short | _: Char    | 
           _: Int   | _: Float   => 32
      case _: Long  | _: Double  => 64
      case s: String             => s.length * 16
      case x: Array[_]           => x.foldLeft(0L)((s, a) => s + 64 + approximateSize(a))
      case x: Iterable[_]        => x.foldLeft(0L)((s, a) => s + 64 + approximateSize(a))
      case x: UUID               => 64 + 128
      case x: Instant            => 64 + 64
      case x: LocalDate          => 64 + 128
      case x: BigDecimal         => 64 + 128 // meh
      case x: Json               => x.fold(64, _ => 64, _ => 64, approximateSize, approximateSize, _.toIterable.foldLeft(0L)((n, x) => n + approximateSize(x)))
      case None                  => 0
      case Some(a)               => 64 + approximateSize(a)
      case x: Product            => x.productIterator.foldLeft(0L)((s, a) => s + 64 + approximateSize(a))
      case x                     => 
        println(s"Can't compute size of ${x.getClass.getName}, skipping")
        0L

  override def fetch(fragment: Fragment, codecs: List[(Boolean, Codec)]): F[Result[Vector[Array[Any]]]] =
    pool
      .use: s =>
        Resource.eval(s.prepare(fragment.fragment.query(rowDecoder(codecs)))).use: ps =>
          ps.stream(fragment.argument, 1024)
            .evalMapAccumulate(0L): (prev, arr) =>
              val next = prev + approximateSize(arr)
              Async[F].raiseWhen(next > maxBytes)(FetchLimitError(maxBytes)) >>
              (next, arr).pure[F]
            .compile
            .toVector
            .map: vec =>
              Result.success(vec.map(_._2))
      .recover:
        case e: FetchLimitError => OdbError.RemoteServiceCallError(Some(e.getMessage())).asFailure
      .onError:
        case NonFatal(e) => Async[F].delay(e.printStackTrace())

