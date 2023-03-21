
-- Make the GMOS North FPU keys more intuitive, and match the core enum tag values.
-- The only potential reference at this point is from t_gmos_north_long_slit.
--
-- Ifu1 -> Ifu2Slits
-- Ifu2 -> IfuBlue
-- Ifu3 -> IfuRed

INSERT INTO t_gmos_north_fpu values ('Ifu2Slits', 'IFU-2', 'IFU 2 Slits', null, 33500000);
INSERT INTO t_gmos_north_fpu values ('IfuBlue', 'IFU-B', 'IFU Left Slit (blue)', null, 31750000);
INSERT INTO t_gmos_north_fpu values ('IfuRed', 'IFU-R', 'IFU Right Slit (red)', null, 35250000);

UPDATE t_gmos_north_long_slit SET c_fpu = 'Ifu2Slits' WHERE c_fpu = 'Ifu1';
UPDATE t_gmos_north_long_slit SET c_fpu = 'IfuBlue' WHERE c_fpu = 'Ifu2';
UPDATE t_gmos_north_long_slit SET c_fpu = 'IfuRed' WHERE c_fpu = 'Ifu3';

DELETE FROM t_gmos_north_fpu WHERE c_tag LIKE 'Ifu_';

-- Missing GMOS North Filters
INSERT INTO t_gmos_north_filter values ('Ri',   'r+i',  'ri_G0349',   705000, '[560000, 850000]', 'BroadBand');
INSERT INTO t_gmos_north_filter values ('OVI',  'OVI',  'OVI_G0345',  684000, '[681600, 686500]', 'NarrowBand');
INSERT INTO t_gmos_north_filter values ('OVIC', 'OVIC', 'OVIC_G0346', 679000, '[676100, 680900]', 'NarrowBand');