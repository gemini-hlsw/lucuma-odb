-- Mask names follow the standard ODF naming convention, in either the OCS form
--
--   G(N|S)YYYY(A|B)<type>PPP-XX_ODF.fits, e.g. GS2015AQ023-01_ODF.fits
--
-- or the GPP form built from the program reference with its dashes removed
--
--   GYYYY(A|B)PPPP<type>-XX_ODF.fits, e.g. G2027A1234Q-42_ODF.fits
--
-- In both the mask name is the file name with the '_ODF.fits' suffix removed.
-- V1247 stripped only the extension, which left the '_ODF' in place.
--
-- The shape is validated on upload rather than by the domain, so that the
-- convention can evolve without a migration.

DROP INDEX unique_mask_name_index;

UPDATE t_attachment
  SET c_mask_name = upper(regexp_replace(c_file_name, '_ODF\.fits$', '', 'i'))
  WHERE c_attachment_type = 'mos_mask';

CREATE UNIQUE INDEX unique_mask_name_index
  ON t_attachment (c_program_id, c_mask_name)
  WHERE c_attachment_type = 'mos_mask';
