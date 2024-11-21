local wezterm = require "wezterm"

local constants = require "./constants"

local function add_element(elements, colors, text)
  table.insert(elements, { Foreground = { Color = colors.Background } })
  table.insert(elements, { Text = constants.solid_left_arrow })
  table.insert(elements, { Foreground = { Color = colors.Foreground } })
  table.insert(elements, { Background = { Color = colors.Background } })
  table.insert(elements, { Text = constants.space_1 .. text .. constants.space_2 })
end

local function get_host_and_cwd(elements, pane)
  local cwd_uri = pane:get_current_working_dir()
  if not cwd_uri then
    return
  end

  local cwd = ""
  local hostname = ""

  if type(cwd_uri) == "userdata" then
    cwd = cwd_uri.file_path
    hostname = cwd_uri.host or wezterm.hostname()
  else
    cwd_uri = cwd_uri:sub(8)
    local slash = cwd_uri:find "/"
    if slash then
      hostname = cwd_uri:sub(1, slash - 1)
      cwd = cwd_uri:sub(slash):gsub("%%(%x%x)", function(hex)
        return string.char(tonumber(hex, 16))
      end)
    end
  end

  local dot = hostname:find "[.]"
  if dot then
    hostname = hostname:sub(1, dot - 1)
  end

  if hostname == "" then
    hostname = wezterm.hostname()
  end

  add_element(elements, { Background = constants.color_palette.east_light, Foreground = constants.color_palette.black },
    constants.icons.cwd .. constants.space_1 .. cwd)
  add_element(elements, { Background = constants.color_palette.west_light, Foreground = constants.color_palette.black },
    constants.icons.hostname .. constants.space_1 .. hostname)
end

local function get_active_workspace(elements, window)
  local workspace = window:active_workspace()

  add_element(elements, { Background = constants.color_palette.south_light, Foreground = constants.color_palette.black },
    constants.icons.workspace .. constants.space_1 .. workspace)
end

local function get_date(elements)
  local date = wezterm.strftime "%Y年%m月%e日"

  add_element(elements, { Background = constants.color_palette.north_light, Foreground = constants.color_palette.black },
    constants.icons.date .. constants.space_1 .. date)
end

local function update_right_status(window, pane)
  local statuses = {}

  get_host_and_cwd(statuses, pane)
  get_active_workspace(statuses, window)
  get_date(statuses)

  window:set_right_status(wezterm.format(statuses))
end

return {
  update_right_status = update_right_status
}
