-- Proposal summary PDFs are generated per partner, so a program may hold several
-- 'summary' attachments.
-- The partner and the renderer style are recorded on the attachment.
-- Recording the style (rather than deriving it from the partner) keeps old PDFs
-- described correctly if the partner-to-style map changes.

-- The layouts the renderer offers (pyexplore pdf/styles.py); adding one is a
-- new value here and in lucuma.odb.data.SummaryStyle.
CREATE TYPE e_summary_style AS ENUM(
  'gemini_standard',
  'gemini_darp',
  'gemini_no_investigators',
  'gemini_investigators_at_end',
  'chile',
  'noirlab_darp'
);

ALTER TABLE t_attachment
  ADD COLUMN c_partner       d_tag           NULL REFERENCES t_partner(c_tag),
  ADD COLUMN c_summary_style e_summary_style NULL;

ALTER TABLE t_attachment
  ADD CONSTRAINT attachment_partner_only_for_summary
  CHECK (c_partner IS NULL OR c_attachment_type = 'summary');

ALTER TABLE t_attachment
  ADD CONSTRAINT attachment_summary_style_iff_summary
  CHECK ((c_attachment_type = 'summary') = (c_summary_style IS NOT NULL));

-- 'science' and 'team' stay one-per-program (V1286).  'summary' moves to
-- one-per-(program, partner); NULLS NOT DISTINCT makes the no-partner summary
-- unique as well.

DROP INDEX unique_proposal_attachments_index;

CREATE UNIQUE INDEX unique_proposal_attachments_index
  ON t_attachment (c_program_id, c_attachment_type)
  WHERE c_attachment_type IN ('science', 'team');

CREATE UNIQUE INDEX unique_summary_attachments_index
  ON t_attachment (c_program_id, c_partner)
  NULLS NOT DISTINCT
  WHERE c_attachment_type = 'summary';
