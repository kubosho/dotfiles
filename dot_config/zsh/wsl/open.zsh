open() {
  explorer.exe "$(wslpath -w "${1:-.}")"
}
