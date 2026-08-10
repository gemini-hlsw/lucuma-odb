-- A MOS mask attachment carries a Mask Name: the observatory's identifier for the
-- physical machined plate (for example 'GN2025AQ001-01'), which is what appears on the
-- mask cutting queue and used by observe to select the mask.
--
-- The name is derived on by stripping the '.fits' extension from the file name but eventually
-- also from a keyword inside the FITS file itself.

-- We'll likely add a name format for mask names.
CREATE DOMAIN d_mask_name AS text CHECK (length(VALUE) > 0);

ALTER TABLE t_attachment
  ADD COLUMN c_mask_name d_mask_name NULL;

-- Backfill existing MOS masks before the invariant is enforced.
UPDATE t_attachment
  SET c_mask_name = COALESCE(NULLIF(regexp_replace(c_file_name, '\.fits$', '', 'i'), ''), c_file_name)
  WHERE c_attachment_type = 'mos_mask';

-- The name is present exactly for MOS masks, so null means "not a MOS mask"
ALTER TABLE t_attachment
  ADD CONSTRAINT t_attachment_mask_name_check
    CHECK ((c_attachment_type = 'mos_mask') = (c_mask_name IS NOT NULL));

-- each mask in a program has a unique mask name
CREATE UNIQUE INDEX unique_mask_name_index
  ON t_attachment (c_program_id, c_mask_name)
  WHERE c_attachment_type = 'mos_mask';
