local wezterm = require "wezterm"

local wsl_domains = wezterm.default_wsl_domains()

for _, dom in ipairs(wsl_domains) do
  dom.default_cwd = "~"
end

return {
  default_domain = 'WSL:Ubuntu-24.04',
  default_prog = { "wsl.exe" },
  wsl_domains = wsl_domains,
}
