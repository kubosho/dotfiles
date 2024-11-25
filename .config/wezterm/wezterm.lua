local wezterm = require "wezterm"

local constants = require"./constants"
local session_manager = require "./plugins/wezterm-session-manager/session-manager"
local status = require "./status"
local workspaces = require "./workspaces"

local act = wezterm.action
local config = wezterm.config_builder()
local mux = wezterm.mux

------------------------------
-- Startup
------------------------------
wezterm.on("gui-startup", function(cmd)
  local _, _, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)
-- Workaround: Gradient grainy
-- https://github.com/wez/wezterm/issues/4813
wezterm.on('window-resized', function()
  wezterm.reload_configuration()
end)

------------------------------
-- Status
------------------------------
config.status_update_interval = 2000

wezterm.on("update-right-status", function(window, pane)
  status.update_right_status(window, pane)
end)

------------------------------
-- Automatically config reload
------------------------------
config.automatically_reload_config = true
wezterm.on("window-config-reloaded", function()
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
config.pane_focus_follows_mouse = false
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
config.window_background_opacity = 0.95
config.window_decorations = "RESIZE"
config.win32_system_backdrop = "Acrylic"
config.macos_window_background_blur = 8

config.window_background_gradient = {
  colors = {
    -- constants.color_palette.east_light,
    constants.color_palette.west_light,
    constants.color_palette.south_light,
    -- constants.color_palette.north_light,
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
-- Workspaces
------------------------------
wezterm.on("save_workspace", function(window) workspaces.save(window) end)
wezterm.on("choose_workspace", function(window, pane) workspaces.choose(window, pane) end)

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
            act.Multiple {
              act.SwitchToWorkspace { name = line, },
              act({ EmitEvent = "save_workspace" }),
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
    action = act({ EmitEvent = "choose_workspace" }),
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
