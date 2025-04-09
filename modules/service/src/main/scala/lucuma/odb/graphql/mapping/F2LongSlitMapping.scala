// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.all.*
import grackle.Result
import grackle.skunk.SkunkMapping
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Decker
import lucuma.core.enums.F2ReadMode
import lucuma.core.enums.F2ReadoutMode
import lucuma.core.enums.F2Reads
import lucuma.core.enums.F2WindowCover
import lucuma.odb.graphql.table.*
import lucuma.odb.sequence.f2.longslit.Config

import scala.reflect.ClassTag
import grackle.TypeRef

trait F2LongSlitMapping[F[_]]
  extends F2LongSlitView[F] with OptionalFieldMapping[F] { this: SkunkMapping[F] =>


  // val defaultReadMode: FieldMapping         = SqlField("defaultReadMode", cc.ReadModeDefault)
  // val explicitReadMode: FieldMapping        = SqlField("explicitReadMode", cc.ReadMode)
  //
  // val decker: FieldMapping                  = explicitOrElseDefault[F2Decker]("decker", "explicitDecker", "defaultDecker")
  // val defaultDecker: FieldMapping           = SqlField("defaultDecker", cc.DeckerDefault)
  // val explicitDecker: FieldMapping          = SqlField("explicitDecker", cc.Decker)
  //
  // val readoutMode: FieldMapping             = SqlField("readoutMode", cc.ReadoutMode)
  // val explicitReadoutMode: FieldMapping     = SqlField("explicitReadoutMode", cc.ReadoutMode)
  //
  // val reads: FieldMapping                   = SqlField("reads", cc.Reads)
  // val explicitReads: FieldMapping           = SqlField("explicitReads", cc.Reads)
  //
  // val windowCover: FieldMapping             = SqlField("windowCover", cc.WindowCover)
  // val explicitWindowCover: FieldMapping     = SqlField("explicitWindowCover", cc.WindowCover)
  //
  // val useElectronicOffsetting: FieldMapping     = SqlField("useElectronicOffsetting", cc.UseElectronicOffsetting)
  // val explicitUseElectronicOffsetting: FieldMapping = SqlField("explicitUseElectronicOffsetting", cc.UseElectronicOffsetting)
  //
  // val imageQuality: FieldMapping            = SqlField("imageQuality", cc.ImageQuality, hidden = true)
  // val sourceProfile: FieldMapping           = SqlField("sourceProfile", cc.SourceProfile, hidden = true)

  lazy val F2LongSlitMapping: ObjectMapping = {

    ObjectMapping(F2LongSlitType)(

      SqlField("observationId", F2LongSlitView.ObservationId, key = true, hidden = true),

      SqlField("disperser", F2LongSlitView.Disperser),
      SqlField("filter",  F2LongSlitView.Filter),
      SqlField("fpu",     F2LongSlitView.Fpu),
      SqlField("readMode",     F2LongSlitView.ReadMode),
      SqlField("decker",     F2LongSlitView.Decker),
      SqlField("readoutMode",     F2LongSlitView.ReadoutMode),
      SqlField("reads",     F2LongSlitView.Reads),
      SqlField("windowCover",     F2LongSlitView.WindowCover),
      // explicitOrElseDefault[F2ReadMode]("readMode", "explicitReadMode", "defaultReadMode"),
      // SqlField("defaultReadMode", cc.ReadModeDefault),
      // SqlField("explicitReadMode", cc.ReadMode)
      // fields.readMode,
      // fields.defaultReadMode,
      // fields.explicitReadMode,
      //
      // // ---------------------
      // decker
      // ---------------------
      // fields.decker,
      // fields.defaultDecker,
      // fields.explicitDecker,
      //
      // // ---------------------
      // // Other parameters
      // // ---------------------
      // fields.readoutMode,
      // fields.explicitReadoutMode,
      // fields.reads,
      // fields.explicitReads,
      // fields.windowCover,
      // fields.explicitWindowCover,
      // fields.useElectronicOffsetting,
      // fields.explicitUseElectronicOffsetting,
      //
      // // ---------------------
      // // hidden view fields
      // // ---------------------
      // fields.imageQuality,
      // fields.sourceProfile,
      //
      // // ---------------------
      // // initialValues
      // // ---------------------
      // SqlField("initialDisperser", F2LongSlitView.InitialDisperser),
      // SqlField("initialFilter",    F2LongSlitView.InitialFilter),
      // SqlField("initialFpu",       F2LongSlitView.InitialFpu)
    )
  }

}

object F2LongSlitMapping {
  // Default values from F2 Config class
  val DefaultReadMode: F2ReadMode = F2ReadMode.Bright
  val DefaultDecker: F2Decker = F2Decker.Imaging
  val DefaultUseElectronicOffsetting: Boolean = false
}
