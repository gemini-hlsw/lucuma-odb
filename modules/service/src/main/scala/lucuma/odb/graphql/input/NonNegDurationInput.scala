// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import edu.gemini.grackle.Result
import lucuma.core.model.NonNegDuration

object NonNegDurationInput {

  val Binding =
    DurationInput.Binding.rmap {
      case NonNegDuration(nnd) => Result(nnd)
      case _ => Result.failure(s"Duration cannot be negative.")
    }

}