local wezterm = require("wezterm")
local act = wezterm.action

wezterm.plugin.require("https://github.com/kubosho/wezterm-sessions")

local M = {}

local save_state = nil
local is_initialized = false

local function create_save_state()
  local last_save_time = 0

  return {
    get_last_save_time = function()
      return last_save_time
    end,

    should_save = function(interval_seconds)
      local current_time = os.time()
      return current_time - last_save_time >= interval_seconds
    end,

    update_save_time = function()
      last_save_time = os.time()
    end,
  }
end

function M.setup_auto_save(auto_save_interval_minutes)
  local interval_minutes = auto_save_interval_minutes or 5

  if not save_state then
    save_state = create_save_state()
  end

  if is_initialized then
    return
  end

  wezterm.on("update-right-status", function(window, pane)
    local interval_seconds = interval_minutes * 60

    if save_state and interval_seconds and save_state.should_save(interval_seconds) then
      window:perform_action(act({ EmitEvent = "save_session" }), pane)
      save_state.update_save_time()
    end
  end)

  wezterm.on("shutdown", function()
    local gui_windows = wezterm.gui.gui_windows()
    if #gui_windows == 0 then
      return
    end

    local gui_window = gui_windows[1]
    local mux_window = gui_window:mux_window()
    if not mux_window then
      return
    end

    local pane = mux_window:active_pane()
    if not pane then
      return
    end

    gui_window:perform_action(act({ EmitEvent = "save_session" }), pane)
  end)

  is_initialized = true

  wezterm.log_info("Auto-save system configured with " .. interval_minutes .. " minute interval")
end

function M.get_keys()
  return {
    {
      key = "d",
      mods = "LEADER",
      action = act({ EmitEvent = "delete_session" }),
    },
    {
      key = "r",
      mods = "LEADER",
      action = act({ EmitEvent = "restore_session" }),
    },
    {
      key = "s",
      mods = "LEADER",
      action = act({ EmitEvent = "save_session" }),
    },
  }
end

return M
