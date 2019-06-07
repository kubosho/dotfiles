status_code () {
  local color face

  color="%(?.${fg[green]}.${fg[blue]})"
  face="%(?!(*'-') < !(*;-;%)? < )"

  echo "${color}${face}${reset_color}"
}
