// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.Result
import grackle.Value
import grackle.syntax.*
import lucuma.core.model.TelluricType

object TelluricTypeBinding extends Matcher[TelluricType]:

  override def validate(value: Value): Result[TelluricType] =
    value match
      case Value.ObjectValue(fields) =>
        val fieldMap = fields.toMap

        fieldMap.get("tag") match {
          case Some(Value.EnumValue(tag)) =>
            tag.toUpperCase match {
              case "HOT"         => TelluricType.Hot.success
              case "A0V"         => TelluricType.A0V.success
              case "SOLAR"       => TelluricType.Solar.success
              case "NO_TELLURIC" => TelluricType.NoTelluric.success
              case "MANUAL"      =>
                fieldMap.get("starTypes") match {
                  case Some(Value.ListValue(starTypes)) =>
                    starTypes.zipWithIndex.parTraverse {
                      case (Value.StringValue(str), _) =>
                        str.success
                      case (_, n)                      =>
                        Result.failure(s"Expected string in starTypes at index $n")
                    }.flatMap { typesList =>
                      NonEmptyList.fromList(typesList) match {
                        case Some(st) => TelluricType.Manual(st).success
                        case None     => Result.failure("starTypes must not be empty for Manual telluric type")
                      }
                    }
                  case None => Result.failure("starTypes is required when tag is Manual")
                  case _    => Result.failure("starTypes must be a list")
                }
              case other => Result.failure(s"Unknown telluric type tag: $other")
            }
          case Some(_) => Result.failure("tag must be an enum value")
          case None    => Result.failure("tag field is required in telluricType")
        }
      case _ => Result.failure("Expected object for telluricType")
