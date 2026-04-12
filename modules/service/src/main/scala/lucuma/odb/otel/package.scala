// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.otel

import cats.syntax.show.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Role
import lucuma.core.model.User
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.Attributes

val ProgramIdKey: AttributeKey[String] = AttributeKey("program.id")
val ObservationIdKey: AttributeKey[String] = AttributeKey("observation.id")
val UserIdKey: AttributeKey[String] = AttributeKey("user.id")
val UserAccessKey: AttributeKey[String] = AttributeKey("user.access")
val UserDisplayKey: AttributeKey[String] = AttributeKey("user.displayName")
val IsCalibrationKey: AttributeKey[Boolean] = AttributeKey("observation.isCalibration")
val CalibrationRunKey: AttributeKey[Boolean] = AttributeKey("calibration.calculate")
val GroupIdKey: AttributeKey[String] = AttributeKey("group.id")

given attrFromProgId: Attribute.From[Program.Id, String] = _.show
given attrMakeProgId: Attribute.Make[Program.Id, String] = Attribute.Make.const(ProgramIdKey)
given attrFromObsId: Attribute.From[Observation.Id, String] = _.show
given attrMakeObsId: Attribute.Make[Observation.Id, String] = Attribute.Make.const(ObservationIdKey)
given attrFromUsrId: Attribute.From[User.Id, String] = _.show
given attrMakeUsrId: Attribute.Make[User.Id, String] = Attribute.Make.const(UserIdKey)
given attrFromUsrRole: Attribute.From[Role, String] = _.access.tag
given attrMakeUsrRole: Attribute.Make[Role, String] = Attribute.Make.const(UserAccessKey)
given attrFromUsrDN: Attribute.From[User, String] = _.displayName
given attrMakeUsrDN: Attribute.Make[Role, String] = Attribute.Make.const(UserDisplayKey)
given attrFromGrpId: Attribute.From[Group.Id, String] = _.show
given attrMakeGrpId: Attribute.Make[Group.Id, String] = Attribute.Make.const(GroupIdKey)
given makeIsCalibKey: Attribute.Make[Boolean, Boolean] = Attribute.Make.const(IsCalibrationKey)
given makeCalibRunKey:Attribute.Make[Boolean, Boolean] = Attribute.Make.const(CalibrationRunKey)

given attrsUser: Attributes.Make[User] = u =>
  Attributes(
    Attribute.from(UserIdKey, u.id),
    Attribute.from(UserAccessKey, u.role),
    Attribute.from(UserDisplayKey, u)
  )
