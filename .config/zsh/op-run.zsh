opr () {
	who=$(op whoami)

	if [[ $? != 0 ]] then
		eval $(op signin)
	fi

	if [[ -f "$PWD/.env" ]] then
		op run --env-file=$PWD/.env -- $@
	else
		op run --env-file=${DOTFILES_DIR}/.env -- $@
	fi
}
