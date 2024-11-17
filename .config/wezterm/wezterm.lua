local wezterm = require "wezterm"

local session_manager = require "./plugins/wezterm-session-manager/session-manager"

local act = wezterm.action
local config = wezterm.config_builder()
local mux = wezterm.mux

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

local SOLID_LEFT_ARROW = utf8.char(0xe0b2)

local SPACE_1 = ' '
local SPACE_2 = '  '

------------------------------
-- Startup
------------------------------
wezterm.on("gui-startup", function(cmd)
  local _, _, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

------------------------------
-- Status
------------------------------
config.status_update_interval = 2000

local function add_element(elements, colors, text)
  table.insert(elements, { Foreground = { Color = colors.Background } })
  table.insert(elements, { Text = SOLID_LEFT_ARROW })
  table.insert(elements, { Foreground = { Color = colors.Foreground } })
  table.insert(elements, { Background = { Color = colors.Background } })
  table.insert(elements, { Text = SPACE_1 .. text .. SPACE_2 })
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

  add_element(elements, { Background = color_palette.east_light, Foreground = color_palette.black },
    icons.cwd .. SPACE_1 .. cwd)
  add_element(elements, { Background = color_palette.west_light, Foreground = color_palette.black },
    icons.hostname .. SPACE_1 .. hostname)
end

local function get_active_workspace(elements, window)
  local workspace = window:active_workspace()

  add_element(elements, { Background = color_palette.south_light, Foreground = color_palette.black },
    icons.workspace .. SPACE_1 .. workspace)
end

local function get_date(elements)
  local date = wezterm.strftime "%Y年%m月%e日"

  add_element(elements, { Background = color_palette.north_light, Foreground = color_palette.black },
    icons.date .. SPACE_1 .. date)
end

local function update_right_status(window, pane)
  local statuses = {}

  get_host_and_cwd(statuses, pane)
  get_active_workspace(statuses, window)
  get_date(statuses)

  window:set_right_status(wezterm.format(statuses))
end

wezterm.on("update-right-status", function(window, pane)
  update_right_status(window, pane)
end)

------------------------------
-- Automatically config reload
------------------------------
config.automatically_reload_config = true
wezterm.on("window-config-reloaded", function(window)
  local message = "The config was reloaded!"
  wezterm.log_info(message)
end)

------------------------------
-- Automatically check for updates
------------------------------
config.check_for_updates = true
config.check_for_updates_interval_seconds = 86400

------------------------------
-- General
------------------------------
config.pane_focus_follows_mouse = true
config.scrollback_lines = 5000

------------------------------
-- IME
------------------------------
config.macos_forward_to_ime_modifier_mask = "SHIFT|CTRL"
config.use_ime = true

------------------------------
-- Appearance
------------------------------
config.adjust_window_size_when_changing_font_size = false
config.color_scheme = "One Light (Gogh)"
config.window_background_opacity = 0.9
config.window_decorations = "RESIZE"
config.win32_system_backdrop = "Acrylic"
config.macos_window_background_blur = 8

config.window_background_gradient = {
  colors = {
    color_palette.east_light,
    color_palette.west_light,
    color_palette.south_light,
    color_palette.north_light,
  },
  orientation = { Linear = { angle = -45.0 } },
}

config.window_padding = {
  left = "1cell",
  right = "1cell",
  top = "0.5cell",
  bottom = "0.5cell",
}

------------------------------
-- Tab bar
------------------------------
config.colors = {
  tab_bar = {
    inactive_tab_edge = "none",
  },
}
config.tab_bar_at_bottom = true
config.use_fancy_tab_bar = true

wezterm.on("format-tab-title", function(tab)
  local pane = tab.active_pane
  local pane_title = pane.title

  if tab.tab_title and #tab.tab_title > 0 then
    pane_title = tab.tab_title
  end

  if tab.is_active then
    return {
      { Text = (tab.tab_index + 1) .. ": " .. pane_title .. "*" },
    }
  else
    return {
      { Text = (tab.tab_index + 1) .. ": " .. pane_title },
    }
  end
end)

------------------------------
-- Text
------------------------------
config.font = wezterm.font_with_fallback {
  "Bizin Gothic",
  "Cascadia Code",
  "Monaco",
  "Consolas",
}
config.font_size = 16
config.line_height = 1.25

------------------------------
-- State
------------------------------
wezterm.on("save_session", function(window) session_manager.save_state(window) end)
wezterm.on("load_session", function(window) session_manager.load_state(window) end)
wezterm.on("restore_session", function(window) session_manager.restore_state(window) end)

------------------------------
-- Key bindings
------------------------------
config.leader = {
  key = "t",
  mods = "CTRL",
  timeout_milliseconds = 2000
}
config.keys = {
  -- Edit tab name
  -- refs: https://github.com/wez/wezterm/issues/522#issuecomment-1496894508
  {
    key = ",",
    mods = "LEADER",
    action = act.PromptInputLine {
      description = "Enter new tab name",
      action = wezterm.action_callback(function(window, _, line)
        if line then
          window:active_tab():set_title(line)
        end
      end),
    },
  },

  -- Workspace
  -- refs: https://zenn.dev/sankantsu/articles/e713d52825dbbb
  {
    key = "W",
    mods = "LEADER|SHIFT",
    action = act.PromptInputLine {
      description = wezterm.format {
        { Attribute = { Intensity = "Bold" } },
        { Text = "Enter name for new workspace" },
      },
      action = wezterm.action_callback(function(window, pane, line)
        if line then
          window:perform_action(
            act.SwitchToWorkspace {
              name = line,
            },
            pane
          )
        end
      end),
    },
  },
  {
    key = "w",
    mods = "LEADER",
    action = wezterm.action_callback(function(window, pane)
      local workspaces = {}
      local current = mux.get_active_workspace()

      for i, name in ipairs(mux.get_workspace_names()) do
        table.insert(workspaces, {
          id = name,
          label = string.format("%d. %s", i, name),
        })
      end

      window:perform_action(act.InputSelector {
        choices = workspaces,
        fuzzy = true,
        fuzzy_description = string.format("Select workspace: %s -> ", current),
        action = wezterm.action_callback(function(_, _, id, label)
          if not id and not label then
            wezterm.log_info "Workspace selection canceled"
          else
            window:perform_action(act.SwitchToWorkspace { name = id }, pane)
          end
        end),
      }, pane)
    end),
  },
  {
    key = "$",
    mods = "LEADER|SHIFT",
    action = act.PromptInputLine {
      description = "(wezterm) Set new title for workspace:",
      action = wezterm.action_callback(function(_, _, line)
        if line then
          mux.rename_workspace(
            mux.get_active_workspace(),
            line
          )
        end
      end),
    },
  },

  -- Pane splitting
  {
    key = "-",
    mods = "LEADER",
    action = act.SplitVertical { domain = "CurrentPaneDomain" },
  },
  {
    key = "|",
    mods = "LEADER|SHIFT",
    action = act.SplitHorizontal { domain = "CurrentPaneDomain" },
  },

  -- Activate pane direction
  {
    key = "h",
    mods = "LEADER",
    action = act.ActivatePaneDirection "Left",
  },
  {
    key = "j",
    mods = "LEADER",
    action = act.ActivatePaneDirection "Down",
  },
  {
    key = "k",
    mods = "LEADER",
    action = act.ActivatePaneDirection "Up",
  },
  {
    key = "l",
    mods = "LEADER",
    action = act.ActivatePaneDirection "Right",
  },

  -- Activate copy mode
  {
    key = "[",
    mods = "LEADER",
    action = act.ActivateCopyMode,
  },

  -- Spawn new tab
  {
    key = "c",
    mods = "LEADER",
    action = act.SpawnTab "CurrentPaneDomain",
  },

  -- Session manager bindings
  {
    key = "S",
    mods = "LEADER|SHIFT",
    action = act({ EmitEvent = "save_session" })
  },
  {
    key = "L",
    mods = "LEADER|SHIFT",
    action = act({ EmitEvent = "load_session" }),
  },
  {
    key = "R",
    mods = "LEADER|SHIFT",
    action = act({ EmitEvent = "restore_session" }),
  },
}

return config
