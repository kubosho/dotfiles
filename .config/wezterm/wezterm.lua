local wezterm = require("wezterm")

local config = wezterm.config_builder()

config.automatically_reload_config = true

------------------------------
-- Appearance
------------------------------
config.color_scheme = 'One Light (Gogh)'
config.window_background_opacity = 0.9

return config
