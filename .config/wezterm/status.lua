local wezterm = require "wezterm"

local constants = require "./constants"

local function add_element(elements, colors, text)
  table.insert(elements, { Foreground = { Color = colors.Background } })
  table.insert(elements, { Text = constants.solid_left_arrow })
  table.insert(elements, { Foreground = { Color = colors.Foreground } })
  table.insert(elements, { Background = { Color = colors.Background } })
  table.insert(elements, { Text = constants.space_1 .. text .. constants.space_2 })
end

local function get_cwd_from_osc7(cwd_uri)
  if type(cwd_uri) == "userdata" then
    return cwd_uri["file_path"] or "", cwd_uri["host"] or wezterm.hostname()
  else
    local uri_str = cwd_uri:sub(8)
    local slash = uri_str:find "/"
    if slash then
      local hostname = uri_str:sub(1, slash - 1)
      local cwd = uri_str:sub(slash):gsub("%%(%x%x)", function(hex)
        return string.char(tonumber(hex, 16))
      end)
      return cwd, hostname
    end
  end
  return "", wezterm.hostname()
end

local function get_cwd_fallback()
  local is_wsl = os.getenv("WSL_DISTRO_NAME") ~= nil
  local tmux_env = os.getenv("TMUX")

  if tmux_env then
    if is_wsl then
      -- WSL + tmux: Use PWD environment variable (more reliable in WSL)
      local pwd = os.getenv("PWD")
      return pwd and pwd ~= "" and pwd or "(tmux/wsl)"
    else
      -- macOS + tmux: Try tmux display-message first, fallback to PWD
      local handle = io.popen("tmux display-message -p '#{pane_current_path}' 2>/dev/null")
      if handle then
        local tmux_cwd = handle:read("*a")
        handle:close()
        if tmux_cwd and tmux_cwd ~= "" then
          return tmux_cwd:gsub("\n", "")
        end
      end
      local pwd = os.getenv("PWD")
      return pwd and pwd ~= "" and pwd or "(tmux)"
    end
  else
    if is_wsl then
      -- WSL without tmux: Use PWD (process info may not work reliably)
      local pwd = os.getenv("PWD")
      return pwd and pwd ~= "" and pwd or "~"
    else
      -- macOS without tmux: Use process info
      local process_info = pane:get_foreground_process_info()
      if process_info and process_info.cwd then
        return process_info.cwd
      else
        local pwd = os.getenv("PWD")
        return pwd and pwd ~= "" and pwd or "~"
      end
    end
  end
end

local function normalize_hostname(hostname)
  local dot = hostname:find "[.]"
  if dot then
    hostname = hostname:sub(1, dot - 1)
  end
  if hostname == "" then
    hostname = wezterm.hostname()
  end
  return hostname
end

local function get_host_and_cwd(elements, pane)
  local cwd_uri = pane:get_current_working_dir()
  local cwd = ""
  local hostname = ""

  if cwd_uri then
    cwd, hostname = get_cwd_from_osc7(cwd_uri)
  else
    cwd = get_cwd_fallback()
    hostname = wezterm.hostname()
  end

  hostname = normalize_hostname(hostname)

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
