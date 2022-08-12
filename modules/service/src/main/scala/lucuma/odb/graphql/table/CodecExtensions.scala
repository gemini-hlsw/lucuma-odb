// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import edu.gemini.grackle.sql.FailedJoin
import skunk.Codec

extension [A](self: Codec[A])
  def embedded: Codec[Any] =
    self.opt.imap(_.getOrElse(FailedJoin))(x => Some(x.asInstanceOf[A])) // whee


