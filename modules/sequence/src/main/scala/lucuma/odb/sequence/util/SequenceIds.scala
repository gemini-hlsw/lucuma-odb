// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.util

import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.util.Uid
import lucuma.odb.sequence.data.GeneratorParams

import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import java.util.UUID
import scala.util.Using


object SequenceIds {

  def namespace(
    observationId: Observation.Id,
    params:        GeneratorParams
  ): UUID =
    toUuid { s =>
      s.writeObject(observationId)
      s.writeObject(params)
    }

  def stepId(
    namespace:    UUID,
    sequenceType: SequenceType,
    index:        Int
  ): Step.Id =
    createUid[Step.Id](namespace, sequenceType, index)

  def atomId(
    namespace:    UUID,
    sequenceType: SequenceType,
    index:        Int
  ): Atom.Id =
    createUid[Atom.Id](namespace, sequenceType, index)

  private def createUid[A: Uid](
    namespace:    UUID,
    sequenceType: SequenceType,
    index:        Int
  ): A =
    Uid[A].isoUuid.reverseGet(
      toUuid { s =>
        s.writeLong(namespace.getMostSignificantBits)
        s.writeLong(namespace.getLeastSignificantBits)
        s.writeChar(Uid[A].tag.value)
        s.writeObject(sequenceType)
        s.writeInt(index)
      }
    )

  private def toUuid(
    update: ObjectOutputStream => Unit
  ): UUID = {
    val baos = new ByteArrayOutputStream
    UUID.nameUUIDFromBytes(
      Using.resource(new ObjectOutputStream(baos)) { s =>
        update(s)
        s.flush()
        baos.toByteArray
      }
    )
  }
}
