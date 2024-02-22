// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.model.sequence.StepConfig.Gcal.Lamp
import lucuma.odb.graphql.binding.*

object StepConfigGcalInput {

  val Binding: Matcher[Gcal] =
    ObjectFieldsBinding.rmap {
      case List(
        GcalArcBinding.List.Option("arcs", rArcs),
        GcalContinuumBinding.Option("continuum", rContinuum),
        GcalDiffuserBinding("diffuser", rDiffuser),
        GcalFilterBinding("filter", rFilter),
        GcalShutterBinding("shutter", rShutter)
      ) => (rArcs, rContinuum, rDiffuser, rFilter, rShutter).parTupled.flatMap {
         (arcs, continuum, diffuser, filter, shutter) =>
           Lamp.fromContinuumOrArcs(continuum, arcs.toList.flatten) match {
             case Left(msg)   => Matcher.validationFailure(msg)
             case Right(lamp) => Result(Gcal(lamp, filter, diffuser, shutter))
           }

      }
    }

}
