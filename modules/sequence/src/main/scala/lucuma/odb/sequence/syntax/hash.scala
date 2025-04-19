// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.syntax

import lucuma.odb.sequence.util.HashBytes

trait ToHashBytesOps {

  extension [A: HashBytes](self: A) {
    def hashBytes: Array[Byte] =
      HashBytes[A].hashBytes(self)

    def md5: Array[Byte] =
      HashBytes[A].md5(self)
  }

}

object hash extends ToHashBytesOps