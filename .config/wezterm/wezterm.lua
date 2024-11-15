local wezterm = require "wezterm"

local act = wezterm.action
local config = wezterm.config_builder()
local mux = wezterm.mux

local session_manager = require "./plugins/wezterm-session-manager/session-manager"

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
-- Startup
------------------------------
wezterm.on("gui-startup", function(cmd)
  local _, _, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
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
config.macos_forward_to_ime_modifier_mask = 'SHIFT|CTRL'
config.use_ime = true

------------------------------
-- Appearance
------------------------------
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
  left = '1cell',
  right = '1cell',
  top = '0.5cell',
  bottom = '0.5cell',
}

------------------------------
-- Tab bar
------------------------------
config.colors = {
  tab_bar = {
    inactive_tab_edge = "none",
  },
}
config.use_fancy_tab_bar = true

wezterm.on("format-tab-title", function(tab)
  local pane_title = tab.active_pane.title
  if tab.tab_title and #tab.tab_title > 0 then
    pane_title = tab.tab_title
  end

  if tab.is_active then
    return {
      { Background = { Color = color_palette.east_light } },
      { Foreground = { Color = color_palette.east_dark } },
      { Text = (tab.tab_index + 1) .. ": " .. pane_title .. " " },
    }
  else
    return {
      { Background = { Color = color_palette.north_light } },
      { Foreground = { Color = color_palette.north_dark } },
      { Text = (tab.tab_index + 1) .. ": " .. pane_title .. " " },
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

  -- refs: https://zenn.dev/sankantsu/articles/e713d52825dbbb
  {
    key = "W",
    mods = "LEADER|SHIFT",
    action = act.PromptInputLine {
      description = wezterm.format {
        { Attribute = { Intensity = 'Bold' } },
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
  -- refs: https://zenn.dev/sankantsu/articles/e713d52825dbbb
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
    key = '[',
    mods = 'LEADER',
    action = act.ActivateCopyMode,
  },

  -- Spawn new tab
  {
    key = 'c',
    mods = 'LEADER',
    action = act.SpawnTab 'CurrentPaneDomain',
  },

  -- Session manager bindings
  {
    key = "S",
    mods = "LEADER|SHIFT",
    action = act({ EmitEvent = "save_session" })
  },
  {
    key = 'L',
    mods = 'LEADER|SHIFT',
    action = act({ EmitEvent = "load_session" }),
  },
  {
    key = "R",
    mods = "LEADER|SHIFT",
    action = act({ EmitEvent = "restore_session" }),
  },
}

return config
