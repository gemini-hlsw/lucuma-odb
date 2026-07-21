-- Add filter type and width to Flamingos-2 filters.

-- Width is the bandpass interval [lo, hi) in pm, matching Flamingos2Filter.width.
ALTER TABLE t_f2_filter ADD COLUMN c_filter_type d_tag            REFERENCES t_filter_type(c_tag);
ALTER TABLE t_f2_filter ADD COLUMN c_width        d_wavelength_pm_range;

-- All Flamingos-2 filters are broad-band.
UPDATE t_f2_filter SET c_filter_type = 'BroadBand';

UPDATE t_f2_filter SET c_width = '[975300,1064700)'   WHERE c_tag = 'Y';
UPDATE t_f2_filter SET c_width = '[1174500,1325500)'  WHERE c_tag = 'J';
UPDATE t_f2_filter SET c_width = '[1513000,1787000)'  WHERE c_tag = 'H';
UPDATE t_f2_filter SET c_width = '[940000,1840000)'   WHERE c_tag = 'JH';
UPDATE t_f2_filter SET c_width = '[1289750,2452250)'  WHERE c_tag = 'HK';
UPDATE t_f2_filter SET c_width = '[1084000,1216000)'  WHERE c_tag = 'JLow';
UPDATE t_f2_filter SET c_width = '[1900000,2500000)'  WHERE c_tag = 'KLong';
UPDATE t_f2_filter SET c_width = '[1991000,2309000)'  WHERE c_tag = 'KShort';
UPDATE t_f2_filter SET c_width = '[1946500,2173500)'  WHERE c_tag = 'KBlue';
UPDATE t_f2_filter SET c_width = '[2186000,2434000)'  WHERE c_tag = 'KRed';

ALTER TABLE t_f2_filter ALTER COLUMN c_filter_type SET NOT NULL;
ALTER TABLE t_f2_filter ALTER COLUMN c_width        SET NOT NULL;
