package main

import (
	"log"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

func createSymlink(src, dst string) error {
	if _, err := os.Lstat(dst); err == nil {
		os.Remove(dst)
	}

	return os.Symlink(src, dst)
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

func isWSL() bool {
	if runtime.GOOS != "linux" {
		return false
	}

	data, err := os.ReadFile("/proc/version")
	if err != nil {
		return false
	}

	return strings.Contains(string(data), "WSL")
}

func linkCursorSettings() {
	home, _ := os.UserHomeDir()
	src := home + "/.cursor/settings.json"
	var dst string

	if !(runtime.GOOS == "darwin" || isWSL()) {
		log.Fatal("Unsupported OS")
	}

	if runtime.GOOS == "darwin" {
		dst = home + "/Library/Application Support/Cursor/User/settings.json"
		dir := filepath.Dir(dst)
		if _, err := os.Stat(dir); os.IsNotExist(err) {
			log.Fatalf("Directory does not exist: %s", dir)
		}

		err := createSymlink(src, dst)
		if err != nil {
			log.Fatal(err)
		}
	} else {
		windowsHome := os.Getenv("WINDOWS_HOME")
		if windowsHome == "" {
			log.Fatal("$WINDOWS_HOME is not set")
		}

		dst = windowsHome + "/AppData/Roaming/Cursor/User/settings.json"
		dir := filepath.Dir(dst)
		if _, err := os.Stat(dir); os.IsNotExist(err) {
			log.Fatalf("Directory does not exist: %s", dir)
		}

		input, err := os.ReadFile(src)
		if err != nil {
			log.Fatalf("Failed to read source file: %v", err)
		}

		err = os.WriteFile(dst, input, 0644)
		if err != nil {
			log.Fatalf("Failed to write destination file: %v", err)
		}
	}
}

func main() {
	files, err := searchDotfiles()
	if err != nil {
		log.Fatal(err)
	}

	home, _ := os.UserHomeDir()

	for _, f := range files {
		n := filepath.Base(f)
		dst := home + "/" + n
		err := createSymlink(f, dst)
		if err != nil {
			log.Fatal(err)
		}
	}

	linkCursorSettings()
}
