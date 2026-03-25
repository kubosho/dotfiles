local M = {}

-- Enum-like table with readonly protection
local STATE = setmetatable({
  busy          = "busy",
  waiting_input = "waiting_input",
  idle          = "idle",
}, {
  __newindex = function()
    error("STATE table is readonly")
  end
})

function M.notification_message_from_claude(tab_id, state)
  local notifications = {
    [STATE.busy] = {
      title = "Claude Code",
      message = "Processing... (Tab " .. tab_id .. ")",
      timeout = 2000
    },
    [STATE.waiting_input] = {
      title = "Claude Code",
      message = "Waiting for your input (Tab " .. tab_id .. ")",
      timeout = 4000
    },
    [STATE.idle] = {
      title = "Claude Code",
      message = "Task completed (Tab " .. tab_id .. ")",
      timeout = 4000
    }
  }

  return notifications[state]
end

function M.get_tab_id(window, pane)
  local mux_window = window:mux_window()
  for i, tab_info in ipairs(mux_window:tabs_with_info()) do
    for _, p in ipairs(tab_info.tab:panes()) do
      if p:pane_id() == pane:pane_id() then
        return i
      end
    end
  end
end

function M.is_claude(pane)
  local process = pane:get_foreground_process_info()
  if not process or not process.argv then
    return false
  end

  for _, arg in ipairs(process.argv) do
    if arg:find("claude") then
      return true
    end
  end
  return false
end

function M.detect_claude_state(pane)
  local content = pane:get_lines_as_text(1000)
  if not content then
    return STATE.idle
  end

  content = content:lower()

  -- Check for waiting prompts with box character
  if content:find("│ do you want") or content:find("│ would you like") then
    return STATE.waiting_input
  end

  -- Check for busy state
  if content:find("esc to interrupt") then
    return STATE.busy
  end

  -- Otherwise idle
  return STATE.idle
end

return M
