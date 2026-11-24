// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.ScorpioDynamicView

trait ScorpioDynamicMapping[F[_]] extends ScorpioDynamicView[F]:

  private def channelMapping(channel: ScorpioDynamicView.Channel): ObjectMapping =
    ObjectMapping(StepRecordType / "scorpio" / channel.name)(
      SqlField("id",            channel.SyntheticId, key = true, hidden = true),
      SqlObject("exposureTime"),
      SqlField("exposureCount", channel.ExposureCount),
      SqlField("readoutMode",   channel.ReadoutMode)
    )

  lazy val ScorpioDynamicMappings: List[TypeMapping] =
    ObjectMapping(StepRecordType / "scorpio")(
      SqlField("id",        ScorpioDynamicView.Id, key = true, hidden = true),
      SqlObject("g"),
      SqlObject("r"),
      SqlObject("i"),
      SqlObject("z"),
      SqlField("fpu",       ScorpioDynamicView.Fpu),
      SqlField("disperser", ScorpioDynamicView.Disperser)
    ) :: ScorpioDynamicView.Channels.map(channelMapping)
