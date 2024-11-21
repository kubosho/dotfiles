local wezterm = require "wezterm"

local color_palette = {
  east_light = "#83CBEC",
  west_light = "#FECA8B",
  south_light = "#F57988",
  north_light = "#58C3A9",

  east_dark = "#3A3895",
  west_dark = "#EE6A37",
  south_dark = "#D24573",
  north_dark = "#2D564C",

  black = "#000000",
  white = "#FFFFFF",
}

local icons = {
  cwd = wezterm.nerdfonts.fa_map_pin,
  date = wezterm.nerdfonts.fa_calendar,
  hostname = wezterm.nerdfonts.fa_desktop,
  workspace = wezterm.nerdfonts.fa_building,
}

local solid_left_arrow = utf8.char(0xe0b2)

local space_1 = ' '
local space_2 = '  '

return {
  color_palette = color_palette,
  icons = icons,
  solid_left_arrow = solid_left_arrow,
  space_1 = space_1,
  space_2 = space_2,
}
