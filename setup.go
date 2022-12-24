package main

import (
	"log"
	"os"
	"path/filepath"
	"strings"
)

const gitDir = ".git"

func createSymlink(filename string) error {
	fi, err := os.Stat(filename)
	if err != nil {
		return err
	}

	if (strings.Contains(filename, gitDir) && fi.IsDir()) {
		return nil
	}

	home, err := os.UserHomeDir()
	if err != nil {
		return err
	}

	n := filepath.Base(filename)
	path := home + `/` + n

	if _, err := os.Lstat(path); err == nil {
		os.Remove(path)
	}

	return os.Symlink(filename, path)
}

func searchDotfiles() ([]string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return nil, err
	}

	paths, err := filepath.Glob(dir + `/\.*`)
	if err != nil {
		return nil, err
	}

	var ret []string

	for _, f := range paths {
		ret = append(ret, f)
	}

	return ret, nil
}

func main() {
	files, err := searchDotfiles()
	if err != nil {
		log.Fatal(err)
	}

	for _, f := range files {
		err := createSymlink(f)

		if err != nil {
			log.Fatal(err)
		}
	}
}
