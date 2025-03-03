-- Add fluxDensitiesAttachment to SEDs

UPDATE t_target SET c_source_profile = REPLACE(c_source_profile::text, '"fluxDensities": null', '"fluxDensities": null, "fluxDensitiesAttachment": null')::jsonb;