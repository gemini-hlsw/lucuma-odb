// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.GnirsDynamicView

trait GnirsDynamicMapping[F[_]] extends GnirsDynamicView[F]:

  lazy val GnirsDynamicMapping: TypeMapping =
    ObjectMapping(StepRecordType / "gnirs")(
      SqlField("id",              GnirsDynamicView.Id, key = true, hidden = true),
      SqlObject("exposure"),
      SqlField("coadds",          GnirsDynamicView.Coadds),
      SqlObject("centralWavelength"),
      SqlField("filter",          GnirsDynamicView.Filter),
      SqlField("decker",          GnirsDynamicView.Decker),
      SqlField("fpuSlit",         GnirsDynamicView.FpuSlit),
      SqlField("fpuOther",        GnirsDynamicView.FpuOther),
      SqlField("fpuIfu",          GnirsDynamicView.FpuIfu),
      SqlObject("acquisitionMirrorOut"),
      SqlField("camera",          GnirsDynamicView.Camera),
      SqlField("focusMotorSteps", GnirsDynamicView.FocusMotorSteps),
      SqlField("readMode",        GnirsDynamicView.ReadMode)
    )
