// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import lucuma.core.enums.Site
import lucuma.core.syntax.string.*
import lucuma.core.util.DateInterval
import lucuma.odb.data.Nullable
import lucuma.core.enums.SubaruInstrument
import lucuma.core.enums.SubaruCallForProposalsType
import lucuma.odb.graphql.binding.*

object SubaruCallPropertiesInput:

  case class Create(
    subaruType:       SubaruCallForProposalsType,
    instruments:      List[SubaruInstrument],
    coordinateLimits: CoordinateLimitsInput.Create
  )

  object Create:

    val Binding: Matcher[DateInterval => Create] =
      ObjectFieldsBinding.rmap:
        case List(
          SubaruCallForProposalsTypeBinding.Option("type", rType),
          SubaruInstrumentBinding.List.Option("instruments", rInstruments),
          CoordinateLimitsInput.Create.Binding.Option("coordinateLimits", rLimits)
        ) =>
          val rInstrumentsʹ = dedup("instruments", rInstruments)(_.tag.toScreamingSnakeCase).map(_.toList.flatten)
          (
            rType.map(_.getOrElse(SubaruCallForProposalsType.Normal)),
            rInstrumentsʹ,
            rLimits
          ).parMapN: (subaruType, instruments, limits) =>
            (active: DateInterval) =>
              // GN coordinate limit defaults apply to Subaru as well.
              Create(
                subaruType,
                instruments,
                limits.fold(CoordinateLimitsInput.Create.default(Site.GN, active))(f => f(Site.GN, active))
              )

  case class Edit(
    subaruType:       Option[SubaruCallForProposalsType],
    instruments:      Nullable[List[SubaruInstrument]],
    coordinateLimits: Option[CoordinateLimitsInput.Edit]
  )

  object Edit:

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          SubaruCallForProposalsTypeBinding.NonNullable("type", rType),
          SubaruInstrumentBinding.List.Nullable("instruments", rInstruments),
          CoordinateLimitsInput.Edit.Binding.Option("coordinateLimits", rLimits)
        ) =>
          val rInstrumentsʹ = dedup("instruments", rInstruments)(_.tag.toScreamingSnakeCase)
          (
            rType,
            rInstrumentsʹ,
            rLimits
          ).parMapN(Edit.apply)