// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.otel

import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.Attribute

val ProgramIdKey: AttributeKey[String] = AttributeKey("program.id")
val ObservationIdKey: AttributeKey[String] = AttributeKey("observation.id")
val IsCalibrationKey: AttributeKey[Boolean] = AttributeKey("observation.isCalibration")

given attrFromProgId: Attribute.From[Program.Id, String] = _.show
given attrMakeProgId: Attribute.Make[Program.Id, String] = Attribute.Make.const(ProgramIdKey)
given attrFromObsId: Attribute.From[Observation.Id, String] = _.show
given attrMakeObsId: Attribute.Make[Observation.Id, String] = Attribute.Make.const(ObservationIdKey)
given Attribute.Make[Boolean, Boolean] = Attribute.Make.const(IsCalibrationKey)

