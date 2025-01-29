
--- Update Flamingos 2 enumerations

delete from t_f2_custom_slit_width;
delete from t_f2_fpu;;

insert into t_f2_fpu values ('Pinhole',       'Pinhole',         '2-Pixel Pinhole Grid', 0, 'Imaging', false);
insert into t_f2_fpu values ('SubPixPinhole', 'Sub-Pix Pinhole', 'Sub-Pixel Pinhole Gr', 0, 'Imaging', false);
insert into t_f2_fpu values ('LongSlit_1',     'Long Slit 1px',   '1-Pixel Long Slit',    1, 'LongSlit', false);
insert into t_f2_fpu values ('LongSlit_2',     'Long Slit 2px',   '2-Pixel Long Slit',    2, 'LongSlit', false);
insert into t_f2_fpu values ('LongSlit_3',     'Long Slit 3px',   '3-Pixel Long Slit',    3, 'LongSlit', false);
insert into t_f2_fpu values ('LongSlit_4',     'Long Slit 4px',   '4-Pixel Long Slit',    4, 'LongSlit', false);
insert into t_f2_fpu values ('LongSlit_6',     'Long Slit 6px',   '6-Pixel Long Slit',    6, 'LongSlit', false);
insert into t_f2_fpu values ('LongSlit_8',     'Long Slit 8px',   '8-Pixel Long Slit',    8, 'LongSlit', false);

insert into t_f2_custom_slit_width values ('CustomWidth_1_pix', '1 pix', 'CustomWidth 1 Pixel', 'LongSlit_1');
insert into t_f2_custom_slit_width values ('CustomWidth_2_pix', '2 pix', 'CustomWidth 2 Pixel', 'LongSlit_2');
insert into t_f2_custom_slit_width values ('CustomWidth_3_pix', '3 pix', 'CustomWidth 3 Pixel', 'LongSlit_3');
insert into t_f2_custom_slit_width values ('CustomWidth_4_pix', '4 pix', 'CustomWidth 4 Pixel', 'LongSlit_4');
insert into t_f2_custom_slit_width values ('CustomWidth_6_pix', '6 pix', 'CustomWidth 6 Pixel', 'LongSlit_6');
insert into t_f2_custom_slit_width values ('CustomWidth_8_pix', '8 pix', 'CustomWidth 8 Pixel', 'LongSlit_8');;
insert into t_f2_custom_slit_width values ('Other',             'Other', 'Other',               null);

