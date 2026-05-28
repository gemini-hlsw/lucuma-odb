// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.GnirsDynamicView

trait GnirsAcquisitionMirrorOutMapping[F[_]] extends GnirsDynamicView[F]:

  private def acquisitionMirrorOutMappingAt(
    path: Path
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("synthetic_id", GnirsDynamicView.AcquisitionMirrorOut.SyntheticId, key = true, hidden = true),
      SqlField("prism",        GnirsDynamicView.AcquisitionMirrorOut.Prism),
      SqlField("grating",      GnirsDynamicView.AcquisitionMirrorOut.Grating),
      SqlObject("wavelength")
    )

  lazy val GnirsAcquisitionMirrorOutMappings: List[TypeMapping] =
    List(
      acquisitionMirrorOutMappingAt(StepRecordType / "gnirs" / "acquisitionMirrorOut")
    )
