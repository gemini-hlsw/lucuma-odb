// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow.validator

import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.Itc
import lucuma.odb.data.ItcAcquisition
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.service.workflow.ObservationValidationInfo
import lucuma.odb.service.workflow.ObservationValidator

// An acquisition-capable mode whose acquisition ITC could not be produced
// (a cached deterministic failure) carries an ItcError.  Pre-execution this
// maps to Undefined and blocks Ready; during execution the frozen snapshot
// is present and execution-state dominance keeps the observation Ongoing,
// so it is a non-blocking standing error there.
case class AcquisitionValidator(itcFor: Observation.Id => Option[Itc]) extends ObservationValidator:
  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    if info.isVisitor then ObservationValidationMap.empty
    else itcFor(info.oid).foldMap:
      _.acquisition match
        case ItcAcquisition.Failed(msg) => ObservationValidationMap.singleton(ObservationValidation.itc(msg))
        case _                          => ObservationValidationMap.empty
