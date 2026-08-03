// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.Itc
import lucuma.odb.data.ItcAcquisition
import lucuma.odb.data.ObservationValidationMap

// An acquisition-capable mode whose acquisition ITC could not be produced
// (a cached deterministic failure) carries an ItcError.  Pre-execution this
// maps to Undefined and blocks Ready; during execution the frozen snapshot
// is present and execution-state dominance keeps the observation Ongoing,
// so it is a non-blocking standing error there.
def acquisitionValidator(itcFor: Observation.Id => Option[Itc]): ObservationValidator = info =>
  if info.isVisitor then ObservationValidationMap.empty
  else itcFor(info.oid).foldMap:
    _.acquisition match
      case ItcAcquisition.Failed(msg) => ObservationValidationMap.singleton(ObservationValidation.itc(msg))
      case _                          => ObservationValidationMap.empty

