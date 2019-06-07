# ref: https://qiita.com/nishina555/items/f4f1ddc6ed7b0b296825
git_current_branch () {
  local branch_name st branch_status branch_status_color

  if [ ! -e  ".git" ]; then
    return
  fi

  branch_name=`git rev-parse --abbrev-ref HEAD 2> /dev/null`
  st=`git status 2> /dev/null`

  if [[ -n `echo "$st" | grep "^nothing to"` ]]; then
    branch_status_color="${fg[green]}"
  elif [[ -n `echo "$st" | grep "^Untracked files"` ]]; then
    branch_status_color="${fg[red]}"
    branch_status="?"
  elif [[ -n `echo "$st" | grep "^Changes not staged for commit"` ]]; then
    branch_status_color="${fg[red]}"
    branch_status="+"
  elif [[ -n `echo "$st" | grep "^Changes to be committed"` ]]; then
    branch_status_color="${fg[yellow]}"
    branch_status="!"
  elif [[ -n `echo "$st" | grep "^rebase in progress"` ]]; then
    branch_status_color="${fg[red]}"
    branch_status="!"
    branch_name="(no branch)"
  else
    branch_status_color="${fg[blue]}"
  fi

  echo "${branch_status_color}${branch_status}[$branch_name]${reset_color}"
}
