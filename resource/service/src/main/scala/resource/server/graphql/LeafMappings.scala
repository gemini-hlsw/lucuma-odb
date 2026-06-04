// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import grackle.skunk.SkunkMapping
import lucuma.core.enums.*
import lucuma.core.model.ProgramReference
import lucuma.core.util.Timestamp
import resource.model.TelescopeAvailability
import resource.model.TelescopeModeType
import resource.model.TooSupport

import java.time.LocalDate

trait LeafMappings[F[_]] extends BaseMapping[F]:
  this: SkunkMapping[F] =>

  lazy val LeafMappings: List[TypeMapping] =
    List(
      LeafMapping[BigDecimal](BigDecimalType),
      LeafMapping[LocalDate](DateType),
      LeafMapping[Long](LongType),
      LeafMapping[Site](SiteType),
      LeafMapping[TelescopeAvailability](TelescopeAvailabilityType),
      LeafMapping[ProgramReference](ProgramReferenceLabelType),
      LeafMapping[TelescopeModeType](TelescopeModeType),
      LeafMapping[Timestamp](TimestampType),
      LeafMapping[TooSupport](TooSupportType)
    )
