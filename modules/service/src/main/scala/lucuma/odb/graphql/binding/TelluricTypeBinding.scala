// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.Value
import lucuma.core.model.TelluricType

object TelluricTypeBinding extends Matcher[TelluricType] {

  private def parseTelluricType(value: Value): Either[String, TelluricType] =
    value match {
      case Value.ObjectValue(fields) =>
        val fieldMap = fields.toList.toMap

        fieldMap.get("tag") match {
          case Some(Value.EnumValue(tag)) =>
            tag.toUpperCase match {
              case "HOT"    => Right(TelluricType.Hot)
              case "A0V"    => Right(TelluricType.A0V)
              case "SOLAR"  => Right(TelluricType.Solar)
              case "MANUAL" =>
                fieldMap.get("starTypes") match {
                  case Some(Value.ListValue(types)) =>
                    types.traverse {
                      case Value.StringValue(str) =>
                        Right(str)
                      case _                      =>
                        Left("Expected string in starTypes")
                    }.flatMap { typesList =>
                      NonEmptyList.fromList(typesList) match {
                        case Some(nel) => Right(TelluricType.Manual(nel))
                        case None => Left("starTypes must not be empty for MANUAL telluric type")
                      }
                    }
                  case None => Left("starTypes is required when tag is MANUAL")
                  case _    => Left("starTypes must be a list")
                }
              case other => Left(s"Unknown telluric type tag: $other")
            }
          case Some(_) => Left("tag must be an enum value")
          case None    => Left("tag field is required in telluricType")
        }
      case _ => Left("Expected object for telluricType")
    }

  override def validate(v: Value): Either[String, TelluricType] =
    parseTelluricType(v)
}
