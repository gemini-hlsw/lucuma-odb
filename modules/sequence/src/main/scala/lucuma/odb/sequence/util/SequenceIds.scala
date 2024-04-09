// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.util

import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.util.Uid
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.syntax.hash.*

import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import java.util.UUID
import scala.util.Using


object SequenceIds {

  def namespace(
    commitHash:    CommitHash,
    observationId: Observation.Id,
    params:        GeneratorParams
  ): UUID =
    UUID.nameUUIDFromBytes(
      Array.concat(commitHash.hashBytes, observationId.hashBytes, params.hashBytes)
    )

  def atomId(
    namespace:    UUID,
    sequenceType: SequenceType,
    cycle:        Int,
    index:        Long
  ): Atom.Id =
    Uid[Atom.Id].isoUuid.reverseGet(
      toUuid { s =>
        writeNamespace[Atom.Id](namespace, sequenceType, s)
        s.writeInt(cycle)
        s.writeLong(index)
      }
    )

  def stepId(
    namespace:    UUID,
    sequenceType: SequenceType,
    atomId:       Atom.Id,
    index:        Int
  ): Step.Id =
    Uid[Step.Id].isoUuid.reverseGet(
      toUuid { s =>
        writeNamespace[Step.Id](namespace, sequenceType, s)
        s.writeLong(atomId.toUuid.getMostSignificantBits)
        s.writeLong(atomId.toUuid.getLeastSignificantBits)
        s.writeInt(index)
      }
    )

  private def writeNamespace[A: Uid](
    namespace:    UUID,
    sequenceType: SequenceType,
    out:          ObjectOutputStream
  ): Unit = {
    out.writeLong(namespace.getMostSignificantBits)
    out.writeLong(namespace.getLeastSignificantBits)
    out.writeChar(Uid[A].tag.value)
    out.writeObject(sequenceType)
  }

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
