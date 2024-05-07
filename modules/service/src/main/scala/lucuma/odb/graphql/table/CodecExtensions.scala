// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import grackle.sql.FailedJoin
import skunk.Codec

extension [A](self: Codec[A])
  def embedded: Codec[A] =
    self.opt.imap(_.getOrElse(FailedJoin))(x => Some(x.asInstanceOf[A])).asInstanceOf[Codec[A]] // whee


