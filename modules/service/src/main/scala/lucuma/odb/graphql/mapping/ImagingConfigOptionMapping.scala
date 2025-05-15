// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.ImagingConfigOptionTable

trait ImagingConfigOptionMapping[F[_]] extends ImagingConfigOptionTable[F] {

  lazy val ImagingConfigOptionMapping: ObjectMapping =
    ObjectMapping(ImagingConfigOptionType)(

      SqlField("instrument",     ImagingConfigOptionTable.Instrument, key = true),
      SqlField("index",          ImagingConfigOptionTable.Index, key = true, hidden = true),

      SqlField("filterLabel",    ImagingConfigOptionTable.FilterLabel),

      SqlField("adaptiveOptics", ImagingConfigOptionTable.Ao),
      SqlField("site",           ImagingConfigOptionTable.Site),
      SqlObject("fov"),

      SqlObject("gmosNorth", Join(List(
        ImagingConfigOptionTable.Instrument -> ImagingConfigOptionGmosNorthTable.Instrument,
        ImagingConfigOptionTable.Index      -> ImagingConfigOptionGmosNorthTable.Index
      ))),

      SqlObject("gmosSouth", Join(List(
        ImagingConfigOptionTable.Instrument -> ImagingConfigOptionGmosSouthTable.Instrument,
        ImagingConfigOptionTable.Index      -> ImagingConfigOptionGmosSouthTable.Index
      ))),

    )

  lazy val ImagingConfigOptionGmosNorthMapping: ObjectMapping =
    ObjectMapping(ImagingConfigOptionGmosNorthType)(

      SqlField("instrument", ImagingConfigOptionGmosNorthTable.Instrument, key = true, hidden = true),
      SqlField("index",      ImagingConfigOptionGmosNorthTable.Index, key = true, hidden = true),

      SqlField("filter",     ImagingConfigOptionGmosNorthTable.Filter)

    )

  lazy val ImagingConfigOptionGmosSouthMapping: ObjectMapping =
    ObjectMapping(ImagingConfigOptionGmosSouthType)(

      SqlField("instrument", ImagingConfigOptionGmosSouthTable.Instrument, key = true, hidden = true),
      SqlField("index",      ImagingConfigOptionGmosSouthTable.Index, key = true, hidden = true),

      SqlField("filter",     ImagingConfigOptionGmosSouthTable.Filter)

    )

}
