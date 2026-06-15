// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import lucuma.core.enums.Site
import lucuma.core.syntax.string.*
import lucuma.core.util.DateInterval
import lucuma.odb.data.KeckInstrument
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

object KeckCallPropertiesInput:

  case class Create(
    instruments:      List[KeckInstrument],
    coordinateLimits: CoordinateLimitsInput.Create
  )

  object Create:

    val Binding: Matcher[DateInterval => Create] =
      ObjectFieldsBinding.rmap:
        case List(
          KeckInstrumentBinding.List.Option("instruments", rInstruments),
          CoordinateLimitsInput.Create.Binding.Option("coordinateLimits", rLimits)
        ) =>
          val rInstrumentsʹ = dedup("instruments", rInstruments)(_.tag.toScreamingSnakeCase).map(_.toList.flatten)
          (
            rInstrumentsʹ,
            rLimits
          ).parMapN: (instruments, limits) =>
            (active: DateInterval) =>
              // GN coordinate limit defaults apply to Keck as well.
              Create(
                instruments,
                limits.fold(CoordinateLimitsInput.Create.default(Site.GN, active))(f => f(Site.GN, active))
              )


  case class Edit(
    instruments:      Nullable[List[KeckInstrument]],
    coordinateLimits: Option[CoordinateLimitsInput.Edit]
  )

  object Edit:

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          KeckInstrumentBinding.List.Nullable("instruments", rInstruments),
          CoordinateLimitsInput.Edit.Binding.Option("coordinateLimits", rLimits)
        ) =>
          val rInstrumentsʹ = dedup("instruments", rInstruments)(_.tag.toScreamingSnakeCase)
          (
            rInstrumentsʹ,
            rLimits
          ).parMapN(Edit.apply)