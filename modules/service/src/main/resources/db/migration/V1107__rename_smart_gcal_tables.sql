-- Rename smart gcal tables for naming consistency with underscore convention

-- IGRINS-2
ALTER TABLE t_smart_igrins2 RENAME TO t_smart_igrins_2;

-- Flamingos 2
ALTER TABLE t_smart_flamingos2 RENAME TO t_smart_flamingos_2;
ALTER TABLE t_smart_flamingos_2 RENAME CONSTRAINT "t_smart_flamingos2_c_instrument_c_gcal_id_fkey" TO "t_smart_flamingos_2_c_instrument_c_gcal_id_fkey";
ALTER TABLE t_smart_flamingos_2 RENAME CONSTRAINT "t_smart_flamingos2_c_disperser_fkey" TO "t_smart_flamingos_2_c_disperser_fkey";
ALTER TABLE t_smart_flamingos_2 RENAME CONSTRAINT "t_smart_flamingos2_c_filter_fkey" TO "t_smart_flamingos_2_c_filter_fkey";
ALTER TABLE t_smart_flamingos_2 RENAME CONSTRAINT "t_smart_flamingos2_c_fpu_fkey" TO "t_smart_flamingos_2_c_fpu_fkey";
