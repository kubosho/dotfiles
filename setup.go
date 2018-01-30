package main

import (
	"log"
	"os"
	"path/filepath"
	"github.com/mitchellh/go-homedir"
)

func createSymlink(fn string) error {
	home, err := homedir.Dir()
	if err != nil {
		log.Fatal(err)
	}

	n := filepath.Base(fn)

	return os.Symlink(fn, home + `/` + n)
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

	return files, nil
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
