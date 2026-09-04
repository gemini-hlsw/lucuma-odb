// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.Result
import lucuma.odb.data.TooWindow
import lucuma.odb.graphql.binding.*

object TooWindowInput {

  private val ExactlyOne: Result[Nothing] =
    Matcher.validationFailure("Exactly one of 'duration' and 'forever' must be specified.")

  val Binding: Matcher[TooWindow] =
    ObjectFieldsBinding.rmap {
      case List(
        TimeSpanInput.Binding.Option("duration", rDuration),
        BooleanBinding.Option("forever", rForever)
      ) =>
        (rDuration, rForever).parTupled.flatMap {
          case (Some(d), None)     => Result(TooWindow.For(d))
          case (None, Some(true))  => Result(TooWindow.Forever)
          case (None, Some(false)) =>
            Matcher.validationFailure("A ToO window that is not 'forever' needs a 'duration'.")
          case _                   => ExactlyOne
        }
    }

}
