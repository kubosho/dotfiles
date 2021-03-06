##############################
# module import
##############################
Import-Module posh-git
Import-Module oh-my-posh

##############################
# common
##############################
# oh-my-posh theme
Set-Theme Paradox
# set encoding for Out-File
# https://stackoverflow.com/questions/40098771/changing-powershells-default-output-encoding-to-utf-8
$PSDefaultParameterValues['Out-File:Encoding'] = 'utf8'

##############################
# keybind
##############################
Set-PSReadlineOption -EditMode Emacs
Set-PSReadlineKeyHandler -Key 'Ctrl+p' -Function HistorySearchBackward
Set-PSReadlineKeyHandler -Key 'Ctrl+n' -Function HistorySearchForward

##############################
# utility functions
##############################
# https://blog.gmork.in/entry/2018/08/22/164537
function gh () {
  Set-Location $(ghq list --full-path | peco)
}

# busybox
function Busybox-Awk () {
  busybox awk $args
}

function Busybox-Ls () {
  busybox ls $args
}

function Busybox-Sed () {
  busybox sed $args
}

##############################
# alias
##############################
Set-Alias -name awk -value Busybox-Awk
# https://serverfault.com/questions/452430/how-to-override-the-default-dir-alias-in-powershell
Set-Alias -name ls -value Busybox-Ls -Option AllScope
Set-Alias -name sed -value Busybox-Sed

Set-Alias -name g -value git
Set-Alias -name open -value Invoke-Item
Set-Alias -name v -value vim

##############################
# Chocolatey profile
##############################
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}
