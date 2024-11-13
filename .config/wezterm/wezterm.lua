local wezterm = require 'wezterm'

local config = wezterm.config_builder()

local color_palette = {
  east_light = "#83CBEC",
  west_light = "#FECA8B",
  south_light = "#F57988",
  north_light = "#58C3A9",

  east_dark = "#3A3895",
  west_dark = "#EE6A37",
  south_dark = "#D24573",
  north_dark = "#2D564C",
}

------------------------------
-- Automatically config reload
------------------------------
config.automatically_reload_config = true
wezterm.on('window-config-reloaded', function(window, pane)
  wezterm.log_info 'The config was reloaded!'
end)

------------------------------
-- Automatically check for updates
------------------------------
config.check_for_updates = true
config.check_for_updates_interval_seconds = 86400

------------------------------
-- Appearance
------------------------------
config.color_scheme = 'One Light (Gogh)'
config.window_background_opacity = 0.9
config.window_decorations = 'RESIZE'

config.window_background_gradient = {
  colors = {
    -- color_palette.east_light,
    color_palette.west_light,
    color_palette.south_light,
    -- color_palette.north_light,
  },
  orientation = { Linear = { angle = -45.0 } },
}

------------------------------
--- Text
------------------------------
config.font = wezterm.font_with_fallback {
  'Bizin Gothic',
  'Cascadia Code',
  'Monaco',
  'Consolas',
}
config.font_size = 16
config.line_height = 1.25

return config
