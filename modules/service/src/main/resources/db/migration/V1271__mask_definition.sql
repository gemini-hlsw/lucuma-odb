-- The design read from a MOS mask attachment's file at upload: instrument,
-- position angle and slits, stored as the query-format JSON served by the
-- GraphQL `mask` field (same convention as t_target.c_source_profile).
--
-- Parsing was introduced with this column and runs synchronously on upload,
-- so the column is populated exactly for masks accepted since then.

ALTER TABLE t_attachment
  ADD COLUMN c_mask_definition jsonb NULL,
  ADD CONSTRAINT attachment_mask_definition_check CHECK (
    c_mask_definition IS NULL OR c_attachment_type = 'mos_mask'
  );
