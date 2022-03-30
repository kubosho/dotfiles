package main

import (
	"log"
	"os"
	"path/filepath"
	"strings"
)

func createSymlink(fn string) error {
	home, err := os.UserHomeDir()
	if err != nil {
		log.Fatal(err)
	}

	n := filepath.Base(fn)
	path := home + `/` + n

	if _, err := os.Lstat(path); err == nil {
		os.Remove(path)
	}

	return os.Symlink(fn, path)
}

func searchDotfiles() ([]string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return nil, err
	}

	files, err := filepath.Glob(dir + `/\.*`)
	if err != nil {
		return nil, err
	}

	var ret []string
	ignore := ".git"
	for _, f := range files {
		if !strings.Contains(f, ignore) {
			ret = append(ret, f)
		}
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
