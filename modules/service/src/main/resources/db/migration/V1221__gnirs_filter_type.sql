-- Add filter type to GNIRS filters.
ALTER TABLE t_gnirs_filter ADD COLUMN c_filter_type d_tag REFERENCES t_filter_type(c_tag);

UPDATE t_gnirs_filter SET c_filter_type = 'BroadBand'     WHERE c_tag = 'CrossDispersed';
UPDATE t_gnirs_filter SET c_filter_type = 'BroadBand'     WHERE c_tag = 'Order6';
UPDATE t_gnirs_filter SET c_filter_type = 'Spectroscopic' WHERE c_tag = 'Order5';
UPDATE t_gnirs_filter SET c_filter_type = 'BroadBand'     WHERE c_tag = 'Order4';
UPDATE t_gnirs_filter SET c_filter_type = 'Spectroscopic' WHERE c_tag = 'Order3';
UPDATE t_gnirs_filter SET c_filter_type = 'BroadBand'     WHERE c_tag = 'Order2';
UPDATE t_gnirs_filter SET c_filter_type = 'BroadBand'     WHERE c_tag = 'Order1';
UPDATE t_gnirs_filter SET c_filter_type = 'NarrowBand'    WHERE c_tag = 'H2';
UPDATE t_gnirs_filter SET c_filter_type = 'BroadBand'     WHERE c_tag = 'HNd100x';
UPDATE t_gnirs_filter SET c_filter_type = 'NarrowBand'    WHERE c_tag = 'H2Nd100x';
UPDATE t_gnirs_filter SET c_filter_type = 'NarrowBand'    WHERE c_tag = 'PAH';
UPDATE t_gnirs_filter SET c_filter_type = 'BroadBand'     WHERE c_tag = 'Y';
UPDATE t_gnirs_filter SET c_filter_type = 'BroadBand'     WHERE c_tag = 'J';
UPDATE t_gnirs_filter SET c_filter_type = 'BroadBand'     WHERE c_tag = 'K';

ALTER TABLE t_gnirs_filter ALTER COLUMN c_filter_type SET NOT NULL;
