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
import skunk.Decoder

trait FetchLimit[F[_]: Async](maxBytes: Long) extends SkunkMapping[F]:

  case class FetchLimitError(maxBytes: Long) extends Exception(s"Fetch limit ($maxBytes bytes) exceeded.")

  // Given a decoder, return an equivalent decoder that also yields the total length of the underly row data
  def countingDecoder[A](dec: Decoder[A]): Decoder[(A, Long)] =
    new Decoder[(A, Long)]:
      override def types = dec.types
      override def decode(offset: Int, ss: List[Option[String]]): Either[Decoder.Error, (A, Long)] =
        dec.decode(offset, ss).map((_, ss.foldMap(_.foldMap(_.length.toLong))))

  override def fetch(fragment: Fragment, codecs: List[(Boolean, Codec)]): F[Result[Vector[Array[Any]]]] =
    pool
      .use: s =>
        Resource.eval(s.prepare(fragment.fragment.query(countingDecoder(rowDecoder(codecs))))).use: ps =>
          ps.stream(fragment.argument, 1024)
            .evalMapAccumulate(0L): 
              case (prev, (arr, size)) =>
                val next = prev + size
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

