Import-Module posh-git
Import-Module oh-my-posh

Set-Theme Paradox

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

# https://qiita.com/twinkfrag/items/f3ecf79b68ea09eadec2
function Pause () {
    if ($psISE) {
        $null = Read-Host 'Press Enter Key...'
    }
    else {
        Write-Host "Press Any Key..."
        (Get-Host).UI.RawUI.ReadKey("NoEcho,IncludeKeyDown") | Out-Null
    }
}

# https://qiita.com/twinkfrag/items/3afb9032fd73eabe09be
function Invoke-CommandRunAs () {
    $cd = (Get-Location).Path
    $commands = "Set-Location $cd; Write-Host `"[Administrator] $cd> $args`"; $args; Pause; exit"
    $bytes = [System.Text.Encoding]::Unicode.GetBytes($commands)
    $encodedCommand = [Convert]::ToBase64String($bytes)
    Start-Process powershell.exe -Verb RunAs -ArgumentList "-NoExit","-encodedCommand",$encodedCommand
}

# https://qiita.com/twinkfrag/items/3afb9032fd73eabe09be
function Start-RunAs () {
    $cd = (Get-Location).Path
    $commands = "Set-Location $cd; (Get-Host).UI.RawUI.WindowTitle += `" [Administrator]`""
    $bytes = [System.Text.Encoding]::Unicode.GetBytes($commands)
    $encodedCommand = [Convert]::ToBase64String($bytes)
    Start-Process powershell.exe -Verb RunAs -ArgumentList "-NoExit","-encodedCommand",$encodedCommand
}

##############################
# alias
##############################
Set-Alias -name g -value git
Set-Alias -name open -value Invoke-Item
Set-Alias -name su -value Start-RunAs
Set-Alias -name sudo -value Invoke-CommandRunAs
Set-Alias -name v -value vim

##############################
# Chocolatey profile
##############################
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}
