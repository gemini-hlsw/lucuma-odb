// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding

object CoordinatesInput {

  type Create = Coordinates
  object Create:
    val Binding: Matcher[Create] =
      Edit.Binding.emap:
        case Edit(Some(ra), Some(dec)) => Right(Coordinates(ra, dec))
        case _ => Left("Both ra and dec are required in order to specify a coordinate.")
        
  final case class Edit(
    ra:  Option[RightAscension],
    dec: Option[Declination]
  )
  object Edit:
    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          RightAscensionInput.Binding.Option("ra", rRa),
          DeclinationInput.Binding.Option("dec", rDec)
        ) => (rRa, rDec).parMapN(Edit(_, _))
      }

}
