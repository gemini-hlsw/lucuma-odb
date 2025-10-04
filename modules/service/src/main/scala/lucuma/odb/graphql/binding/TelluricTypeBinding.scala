// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.Value
import lucuma.core.model.TelluricType

object TelluricTypeBinding extends Matcher[TelluricType]:

  override def validate(value: Value): Either[String, TelluricType] =
    value match
      case Value.ObjectValue(fields) =>
        val fieldMap = fields.toMap

        fieldMap.get("tag") match {
          case Some(Value.EnumValue(tag)) =>
            tag.toUpperCase match {
              case "HOT"    => Right(TelluricType.Hot)
              case "A0V"    => Right(TelluricType.A0V)
              case "SOLAR"  => Right(TelluricType.Solar)
              case "MANUAL" =>
                fieldMap.get("starTypes") match {
                  case Some(Value.ListValue(starTypes)) =>
                    starTypes.traverse {
                      case Value.StringValue(str) =>
                        str.asRight
                      case _                      =>
                        "Expected string in starTypes".asLeft
                    }.flatMap { typesList =>
                      NonEmptyList.fromList(typesList) match {
                        case Some(st) => TelluricType.Manual(st).asRight
                        case None     => "starTypes must not be empty for Manual telluric type".asLeft
                      }
                    }
                  case None => "starTypes is required when tag is Manual".asLeft
                  case _    => "starTypes must be a list".asLeft
                }
              case other => s"Unknown telluric type tag: $other".asLeft
            }
          case Some(_) => "tag must be an enum value".asLeft
          case None    => "tag field is required in telluricType".asLeft
        }
      case _ => "Expected object for telluricType".asLeft
