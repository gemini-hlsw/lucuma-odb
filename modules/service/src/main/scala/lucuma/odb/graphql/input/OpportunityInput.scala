// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.core.math.Region
import lucuma.odb.graphql.binding.*

case class OpportunityInput(region: Region)

object OpportunityInput:

  case class Create(region: RegionInput.Create)

  /**
   * The region is optional but *not* nullable. Every opportunity target has one -- omitting it on
   * create approves the whole sky rather than leaving it unset -- so there is no state for null to
   * denote; ceasing to be a Target of Opportunity is a subtype change, made through the top-level
   * sidereal / nonsidereal fields. Omitting it here leaves the approved region untouched.
   *
   * GraphQL cannot express "optional but not nullable" in the type -- an input field is either
   * required or nullable -- so it is enforced here, by `NonNullable`.
   */
  case class Edit(region: Option[RegionInput.Edit])

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap:
      case List(
        RegionInput.CreateBinding.NonNullable("region", rRegion)
      ) =>
        rRegion.map: region =>
          Create(region.getOrElse(RegionInput.Default))

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap:
      case List(
        RegionInput.EditBinding.NonNullable("region", rRegion)
      ) =>
        rRegion.map(Edit.apply)
