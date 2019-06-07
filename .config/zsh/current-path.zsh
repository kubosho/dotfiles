show_current_path () {
  local color path

  color="${fg[yellow]}"
  path="%d"

  echo "${color}${path}${reset_color}"
}

