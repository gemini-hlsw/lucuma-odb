// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.core.math.Region
import lucuma.odb.graphql.binding.*

case class OpportunityInput(region: Region)

object OpportunityInput:

  case class Create(region: RegionInput.Create)
  case class Edit(region: RegionInput.Edit)

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap:
      case List(
        RegionInput.CreateBinding("region", rRegion)
      ) => 
        rRegion.map(Create.apply)

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap:
      case List(
        RegionInput.EditBinding("region", rRegion)
      ) => 
        rRegion.map(Edit.apply)
 

