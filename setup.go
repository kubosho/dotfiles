package main

import (
	"log"
	"os"
	"path/filepath"
	"strings"
)

func createSymlink(filename string) error {
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

func readGitignorePatterns() ([]string, error) {
	data, err := os.ReadFile(".gitignore")
	if err != nil {
		return nil, err
	}

	var patterns []string
	lines := strings.Split(string(data), "\n")
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		// Ignore negation patterns
		if strings.HasPrefix(line, "!") {
			continue
		}
		patterns = append(patterns, line)
	}
	return patterns, nil
}

func matchPattern(path string, patterns []string) bool {
	for _, pat := range patterns {
		pat = strings.TrimSpace(pat)
		if pat == "" {
			continue
		}
		// Directory pattern
		if strings.HasSuffix(pat, "/") {
			pat = strings.TrimSuffix(pat, "/")
			if strings.HasSuffix(path, pat) || strings.Contains(path, pat+"/") {
				return true
			}
		}
		// Wildcard pattern
		matched, _ := filepath.Match(pat, filepath.Base(path))
		if matched {
			return true
		}
		// Subdirectory pattern
		matched, _ = filepath.Match(pat, path)
		if matched {
			return true
		}
	}
	return false
}

func searchDotfiles() ([]string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return nil, err
	}

	ignore_patterns := []string{".git"}
	gitignore, _ := readGitignorePatterns()
	if gitignore != nil {
		ignore_patterns = append(ignore_patterns, gitignore...)
	}

	paths, err := filepath.Glob(dir + `/\.*`)
	if err != nil {
		return nil, err
	}

	var ret []string

	for _, f := range paths {
		rel, _ := filepath.Rel(dir, f)
		if matchPattern(rel, ignore_patterns) {
			continue
		}
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
