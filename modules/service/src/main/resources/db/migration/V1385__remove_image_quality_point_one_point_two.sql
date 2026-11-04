-- Remove the 0.1" and 0.2" image quality presets, mapping observations to 0.3".

-- No configuration requests are valid yet. Deleting avoids unique key
-- collisions with existing 0.3" requests.
DELETE FROM t_configuration_request
  WHERE c_image_quality IN ('point_one', 'point_two');

UPDATE t_observation
  SET c_image_quality = 'point_three'
  WHERE c_image_quality IN ('point_one', 'point_two');

DELETE FROM t_image_quality
  WHERE c_tag IN ('point_one', 'point_two');
