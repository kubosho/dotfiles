local wezterm = require "wezterm"
local io = require "io"

local act = wezterm.action
local mux = wezterm.mux

local workspaces_dir = wezterm.home_dir .. "/.config/wezterm/workspaces"
local prefix = "wezterm_workspace_"

local module = {}

local function get_workspaces()
  local workspaces = {}

  for _, dir in ipairs(wezterm.glob(workspaces_dir .. "/*")) do
    dir = dir:gsub(workspaces_dir .. "/", "")
    dir = dir:gsub(prefix, "")
    table.insert(workspaces, dir)
  end

  return workspaces
end

local function get_workspace_name(window)
  local workspace_name = window:active_workspace()

  return workspace_name
end

function module.save(window)
  local name = get_workspace_name(window)
  local file = io.open(workspaces_dir .. "/" .. prefix .. name, "w")

  if file then
    file:write(wezterm.json_encode(name))
    file:close()
    return true
  else
    return false
  end
end

function module.choose(window, pane)
  local choices = {}
  local current_workspace = mux.get_active_workspace()
  local workspaces = get_workspaces()

  for i, name in ipairs(workspaces) do
    table.insert(choices, {
      id = name,
      label = string.format("%d. %s", i, name),
    })
  end

  return window:perform_action(act.InputSelector {
    choices = choices,
    fuzzy = true,
    fuzzy_description = string.format("Select workspace: %s -> ", current_workspace),
    title = "Workspaces",
    action = wezterm.action_callback(function(_, _, id, label)
      if not id and not label then
        wezterm.log_info "Workspace selection canceled"
        return
      end

      window:perform_action(act.SwitchToWorkspace { name = id, }, pane)
    end),
  }, pane)
end

return module
