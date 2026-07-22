-- Add the GMOS North/South Multi-Object Spectroscopy (MOS) observing mode types
-- to the e_observing_mode_type enum.
ALTER TYPE e_observing_mode_type ADD VALUE 'gmos_north_mos';
ALTER TYPE e_observing_mode_type ADD VALUE 'gmos_south_mos';
