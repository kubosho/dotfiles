local wezterm = require "wezterm"

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
wezterm.on('window-config-reloaded', function(window, pane)
  wezterm.log_info 'The config was reloaded!'
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
-- Appearance
------------------------------
config.color_scheme = "One Light (Gogh)"
config.window_background_opacity = 0.8
config.window_decorations = "RESIZE"
config.win32_system_backdrop = "Acrylic"
config.macos_window_background_blur = 8

config.window_background_gradient = {
  colors = {
    -- color_palette.east_light,
    color_palette.west_light,
    color_palette.south_light,
    -- color_palette.north_light,
  },
  orientation = { Linear = { angle = -45.0 } },
}

config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}

------------------------------
-- Tab bar
------------------------------
config.use_fancy_tab_bar = true

config.colors = {
  tab_bar = {
    inactive_tab_edge = "none",
  },
}

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
      description = "Create new workspace",
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
      for i, name in ipairs(wezterm.mux.get_workspace_names()) do
        table.insert(workspaces, {
          id = name,
          label = string.format("%d. %s", i, name),
        })
      end

      window:perform_action(act.InputSelector {
        title = "Select workspace",
        choices = workspaces,
        fuzzy = true,
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
}

return config
