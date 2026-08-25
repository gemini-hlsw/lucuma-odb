-- Brings 'summary' under the partial unique index that makes the proposal
-- attachment types one-per-program.  The enum value itself already exists: it
-- arrived with the rest of the lucuma-core 0.236.0 catch-up in V1280.
--
-- The index was created in V0934 covering 'science' and 'team'.  AttachmentType
-- carries a `uniqueInProgram` flag that AttachmentFileService already checks
-- before inserting, but that check races; this index is what actually guarantees
-- the invariant.

DROP INDEX unique_proposal_attachments_index;

CREATE UNIQUE INDEX unique_proposal_attachments_index
  ON t_attachment (c_program_id, c_attachment_type)
  WHERE c_attachment_type IN ('science', 'team', 'summary');
