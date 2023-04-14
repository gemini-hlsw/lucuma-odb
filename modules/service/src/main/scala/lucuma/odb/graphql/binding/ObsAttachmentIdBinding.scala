// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.core.model.ObsAttachment

<<<<<<< main:modules/service/src/main/scala/lucuma/odb/graphql/binding/AttachmentIdBinding.scala
val AttachmentIdBinding: Matcher[ObsAttachment.Id] =
  gidBinding("attachment")
=======
val ObsAttachmentIdBinding: Matcher[ObsAttachment.Id] =
  gidBinding("observation attachment")
>>>>>>> Make current attachments specific to observations:modules/service/src/main/scala/lucuma/odb/graphql/binding/ObsAttachmentIdBinding.scala
