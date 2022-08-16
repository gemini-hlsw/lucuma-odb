// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.enums

import cats.data.NonEmptyList
import clue.GraphQLOperation
import io.circe.Decoder
import lucuma.core.util.Enumerated

/** A GraphQL operation that fetches a list of `A` and turns it into an `Enumerated[A]`. */
abstract class EnumeratedQuery[A](tag: A => String) extends GraphQLOperation[Nothing] {

  given ElemDecoder: Decoder[A]

  type Variables = Unit
  type Data = Enumerated[A]

  val varEncoder  = implicitly
  val dataDecoder = Decoder[NonEmptyList[A]].map(Enumerated.fromNEL(_).withTag(tag))

}

