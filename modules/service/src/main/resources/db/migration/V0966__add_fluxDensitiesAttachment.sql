UPDATE t_target SET c_source_profile = jsonb_set(c_source_profile, '{point,bandNormalized,sed,fluxDensitiesAttachment}', 'null')
	WHERE (c_source_profile #> '{point}') <> 'null';
UPDATE t_target SET c_source_profile = jsonb_set(c_source_profile, '{uniform,bandNormalized,sed,fluxDensitiesAttachment}', 'null')
	WHERE (c_source_profile #> '{uniform}') <> 'null';
UPDATE t_target SET c_source_profile = jsonb_set(c_source_profile, '{gaussian,bandNormalized,sed,fluxDensitiesAttachment}', 'null')
	WHERE (c_source_profile #> '{gaussian}') <> 'null';