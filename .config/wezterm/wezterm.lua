local wezterm = require("wezterm")

local config = wezterm.config_builder()

------------------------------
-- Automatically config reload
------------------------------
config.automatically_reload_config = true
wezterm.on('window-config-reloaded', function(window, pane)
  wezterm.log_info 'The config was reloaded!'
end)

------------------------------
-- Appearance
------------------------------
config.color_scheme = 'One Light (Gogh)'
config.window_background_opacity = 0.9
config.window_decorations = 'RESIZE'

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
