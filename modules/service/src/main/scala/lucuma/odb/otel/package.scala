// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.otel

import cats.syntax.show.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.AttributeKey

val ProgramIdKey: AttributeKey[String] = AttributeKey("program.id")
val ObservationIdKey: AttributeKey[String] = AttributeKey("observation.id")
val IsCalibrationKey: AttributeKey[Boolean] = AttributeKey("observation.isCalibration")
val CalibrationRunKey: AttributeKey[Boolean] = AttributeKey("calibration.calculate")
val GroupIdKey: AttributeKey[String] = AttributeKey("group.id")

given attrFromProgId: Attribute.From[Program.Id, String] = _.show
given attrMakeProgId: Attribute.Make[Program.Id, String] = Attribute.Make.const(ProgramIdKey)
given attrFromObsId: Attribute.From[Observation.Id, String] = _.show
given attrMakeObsId: Attribute.Make[Observation.Id, String] = Attribute.Make.const(ObservationIdKey)
given attrFromGrpId: Attribute.From[Group.Id, String] = _.show
given attrMakeGrpId: Attribute.Make[Group.Id, String] = Attribute.Make.const(GroupIdKey)
given makeIsCalibKey: Attribute.Make[Boolean, Boolean] = Attribute.Make.const(IsCalibrationKey)
given makeCalibRunKey:Attribute.Make[Boolean, Boolean] = Attribute.Make.const(CalibrationRunKey)

