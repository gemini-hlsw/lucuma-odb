// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.odb.graphql.binding.*

object GoaPropertiesInput:

  val DefaultProprietaryMonths: NonNegInt = NonNegInt.unsafeFrom(0)
  val DefaultShouldNotify: Boolean        = true
  val DefaultPrivateHeader: Boolean       = false

  case class Create(
    proprietaryMonths: NonNegInt,
    shouldNotify:      Boolean,
    privateHeader:     Boolean
  )

  object Create:
    val Default: Create =
      Create(
        DefaultProprietaryMonths,
        DefaultShouldNotify,
        DefaultPrivateHeader
      )

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          NonNegIntBinding.Option("proprietaryMonths", rProprietary),
          BooleanBinding.Option("shouldNotify", rShouldNotify),
          BooleanBinding.Option("privateHeader", rPrivateHeader)
        ) => (rProprietary, rShouldNotify, rPrivateHeader).parTupled.map:
          (proprietary, shouldNotify, headerPrivate) => Create(
            proprietary.getOrElse(DefaultProprietaryMonths),
            shouldNotify.getOrElse(DefaultShouldNotify),
            headerPrivate.getOrElse(DefaultPrivateHeader)
          )

  case class Edit(
    proprietaryMonths: Option[NonNegInt],
    shouldNotify:      Option[Boolean],
    privateHeader:     Option[Boolean]
  )

  object Edit:
    val Default: Edit =
      Edit(None, None, None)

    val Binding: Matcher[GoaPropertiesInput.Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          NonNegIntBinding.Option("proprietaryMonths", rProprietary),
          BooleanBinding.Option("shouldNotify", rShouldNotify),
          BooleanBinding.Option("privateHeader", rPrivateHeader)
        ) => (rProprietary, rShouldNotify, rPrivateHeader).parTupled.map(Edit.apply)