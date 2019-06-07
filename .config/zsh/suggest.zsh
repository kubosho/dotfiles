suggest () {
  local color yes no abort edit message

  color="${fg[red]}"

  yes="${fg[green]}そう(y)${reset_color}"
  no="${fg[red]}違うよ(n)${reset_color}"
  abort="${fg[blue]}やーめた(a)${reset_color}"
  edit="${fg[cyan]}直すね(e)${reset_color}"

  message="${color}(*'~')?${reset_color} < もしかして ${color}%B%r%b${reset_color} かな？ [${yes},${no},${abort},${edit}]: "

  echo "${suggest}${message}"
}
