local wezterm = require("wezterm")

local constants = require("./constants")
local copy_mode = require("./copy_mode")
local llm_cli = require("./llm_cli")
local pane = require("./pane")
local status = require("./status")
local wsl = require("./wsl")
-- local session = require("./session")

local has_local, local_module = pcall(require, "./local")

local act = wezterm.action
local config = wezterm.config_builder()
local mux = wezterm.mux

local is_windows = wezterm.target_triple:find("windows")
local is_macos = wezterm.target_triple:find("darwin")

------------------------------
-- Startup
------------------------------
wezterm.on("gui-startup", function(cmd)
  local _, _, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

-- Workaround: Gradient grainy
-- https://github.com/wez/wezterm/issues/4813
wezterm.on("window-resized", function()
  wezterm.reload_configuration()
end)

wezterm.on("bell", function(window, pane)
  if llm_cli.is_claude(pane) then
    local claude_state = llm_cli.detect_claude_state(pane)
    local tab_id = llm_cli.get_tab_id(window, pane)
    local notification = llm_cli.notification_message_from_claude(tab_id, claude_state)

    if notification then
      window:toast_notification(
        notification.title,
        notification.message,
        nil,
        notification.timeout
      )
    end
  end
end)

------------------------------
-- WSL
------------------------------
if is_windows then
  config.default_domain = wsl.default_domain
  config.default_prog = wsl.default_prog
  config.wsl_domains = wsl.wsl_domains
end

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
  wezterm.log_info("The config was reloaded!")
  -- session.setup_auto_save()
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
config.color_scheme = "Tokyo Night Day"
config.window_background_opacity = 0.95
config.window_decorations = "RESIZE"

if is_windows then
  config.win32_system_backdrop = "Auto"
end

if is_macos then
  config.macos_window_background_blur = 8
end

config.window_background_gradient = {
  colors = {
    constants.color_palette.east_light,
    constants.color_palette.west_light,
    constants.color_palette.south_light,
    constants.color_palette.north_light,
  },
  orientation = { Linear = { angle = 45.0 } },
}

config.window_padding = {
  left = "0",
  right = "0",
  top = "0",
  bottom = "0",
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
config.font = wezterm.font_with_fallback({
  { family = "Inconsolata Nerd Font Mono" },
  { family = "Inconsolata Nerd Font Mono", assume_emoji_presentation = true },
  { family = "Bizin Gothic" },
  { family = "Cascadia Code" },
  { family = "Monaco" },
  { family = "Consolas" },
})

if is_windows then
  config.font_size = 14
  config.line_height = 1.25
end

if is_macos then
  config.font_size = 16
  config.line_height = 1.4
end

------------------------------
-- Key bindings
------------------------------
config.leader = {
  key = "t",
  mods = "CTRL",
  timeout_milliseconds = 2000,
}

config.keys = {
  -- Edit tab name
  -- refs: https://github.com/wez/wezterm/issues/522#issuecomment-1496894508
  {
    key = ",",
    mods = "LEADER",
    action = act.PromptInputLine({
      description = "Enter new tab name",
      action = wezterm.action_callback(function(window, _, line)
        if line then
          window:active_tab():set_title(line)
        end
      end),
    }),
  },

  -- Spawn new tab
  {
    key = "c",
    mods = "LEADER",
    action = act.SpawnTab("CurrentPaneDomain"),
  },

  -- Shift + Enter in CLI
  {
    key = "Enter",
    mods = "SHIFT",
    action = wezterm.action.SendString("\n"),
  },
}

-- Add session management key bindings
-- for _, key in ipairs(session.get_keys()) do
--   table.insert(config.keys, key)
-- end

-- Add pane key bindings
for _, key in ipairs(pane.get_keys()) do
  table.insert(config.keys, key)
end

-- Add copy mode key bindings
for _, key in ipairs(copy_mode.get_keys()) do
  table.insert(config.keys, key)
end

-- Windows specific key bindings
if is_windows then
  table.insert(config.keys, {
    key = "v",
    mods = "CTRL|SHIFT",
    action = act.PasteFrom("Clipboard"),
  })
end

------------------------------
-- Local config
------------------------------
if has_local and local_module and local_module.get_config then
  local local_config = local_module.get_config()

  for key, value in pairs(local_config) do
    config[key] = value
  end
end

return config
