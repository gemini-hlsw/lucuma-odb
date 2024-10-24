// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Order.*
import cats.data.NonEmptyChain
import cats.syntax.all.*
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.model.ObservationValidation
import lucuma.core.util.NewType

import scala.collection.immutable.SortedMap

object ObservationValidationMap extends NewType[SortedMap[ObservationValidationCode, NonEmptyChain[String]]]:
  def empty: ObservationValidationMap = ObservationValidationMap(SortedMap.empty[ObservationValidationCode, NonEmptyChain[String]])

  def singleton(v: ObservationValidation): ObservationValidationMap =
    empty.add(v)

  def fromList(l: List[ObservationValidation]) =
    l.foldLeft(empty){ _.add(_)}

  extension (m: ObservationValidationMap.Type)
    def toMap: SortedMap[ObservationValidationCode, NonEmptyChain[String]] = m.value
    def add(validation: ObservationValidation): ObservationValidationMap = 
      addMessages(validation.code, validation.messages)
    def addMessages(code: ObservationValidationCode, msgs: NonEmptyChain[String]): ObservationValidationMap =
      ObservationValidationMap(
        m.value.updatedWith(code){
          case None        => msgs.some
          case Some(nec) => Some(nec |+| msgs)
        }
      )

    def toList: List[ObservationValidation] = m.value.toList.map(ObservationValidation.apply)

type ObservationValidationMap = ObservationValidationMap.Type
