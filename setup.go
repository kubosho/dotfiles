package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

// COPY_TARGETS specifies the files and directories to be copied instead of symlinked
var COPY_TARGETS = []string{
	".claude",
	".claude/commands",
}

func createSymlink(src, dst string) error {
	if _, err := os.Lstat(dst); err == nil {
		os.Remove(dst)
	}

	return os.Symlink(src, dst)
}


func readDotfilesignorePatterns() ([]string, error) {
	data, err := os.ReadFile(".dotfilesignore")
	if err != nil {
		return nil, err
	}

	var patterns []string
	for line := range strings.SplitSeq(string(data), "\n") {
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
	
	dotfilesignore, _ := readDotfilesignorePatterns()
	if dotfilesignore != nil {
		ignore_patterns = append(ignore_patterns, dotfilesignore...)
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

func shouldCopyPath(path string) bool {
	for _, target := range COPY_TARGETS {
		if filepath.Base(path) == target {
			return true
		}
	}
	return false
}

func copyFile(src, dst string) error {
	srcFile, err := os.Open(src)
	if err != nil {
		return err
	}
	defer srcFile.Close()

	srcInfo, err := srcFile.Stat()
	if err != nil {
		return err
	}

	dstFile, err := os.OpenFile(dst, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, srcInfo.Mode())
	if err != nil {
		return err
	}
	defer dstFile.Close()

	_, err = io.Copy(dstFile, srcFile)
	return err
}

func copyFileOrDir(src, dst string) error {
	srcInfo, err := os.Stat(src)
	if err != nil {
		return err
	}

	if srcInfo.IsDir() {
		return copyDir(src, dst)
	}
	return copyFile(src, dst)
}

func copyDir(src, dst string) error {
	srcInfo, err := os.Stat(src)
	if err != nil {
		return err
	}

	err = os.MkdirAll(dst, srcInfo.Mode())
	if err != nil {
		return err
	}

	entries, err := os.ReadDir(src)
	if err != nil {
		return err
	}

	for _, entry := range entries {
		srcPath := filepath.Join(src, entry.Name())
		dstPath := filepath.Join(dst, entry.Name())

		// Skip if file is not managed by git
		if !isGitManaged(srcPath) {
			continue
		}

		if entry.IsDir() {
			err = copyDir(srcPath, dstPath)
			if err != nil {
				return err
			}
		} else {
			err = copyFile(srcPath, dstPath)
			if err != nil {
				return err
			}
		}
	}

	return nil
}

func isGitManaged(path string) bool {
	// Check if path is under git management
	// For now, we'll exclude .git directory and respect .gitignore
	if strings.Contains(path, ".git/") {
		return false
	}
	
	// In a real implementation, we would check .gitignore
	// For now, we'll assume all non-.git files are managed
	return true
}

func promptUserConfirmation(message string) bool {
	reader := bufio.NewReader(os.Stdin)
	fmt.Printf("%s [y/N]: ", message)
	
	response, err := reader.ReadString('\n')
	if err != nil {
		return false
	}
	
	response = strings.ToLower(strings.TrimSpace(response))
	return response == "y" || response == "yes"
}

func handleExistingPath(path string, dryRun bool) error {
	info, err := os.Lstat(path)
	if err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		return err
	}

	// Check if it's a symlink
	if info.Mode()&os.ModeSymlink != 0 {
		if dryRun {
			log.Printf("Would remove existing symlink: %s", path)
		} else {
			return os.Remove(path)
		}
	} else {
		// It's a regular file or directory
		if dryRun {
			log.Printf("Would ask to remove existing path: %s", path)
		} else {
			if promptUserConfirmation(fmt.Sprintf("Remove existing %s?", path)) {
				return os.RemoveAll(path)
			} else {
				return fmt.Errorf("skipping %s: user chose not to remove existing path", path)
			}
		}
	}
	
	return nil
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
	var dryRun bool
	flag.BoolVar(&dryRun, "dry-run", false, "Show what would be done without making any changes")
	flag.Parse()

	files, err := searchDotfiles()
	if err != nil {
		log.Fatal(err)
	}

	home, _ := os.UserHomeDir()

	for _, f := range files {
		n := filepath.Base(f)
		dst := home + "/" + n
		
		if dryRun {
			log.Printf("Would create symlink: %s -> %s", f, dst)
		} else {
			err := createSymlink(f, dst)
			if err != nil {
				log.Fatal(err)
			}
		}
	}

	if dryRun {
		log.Printf("Would link Cursor settings")
	} else {
		linkCursorSettings()
	}
}
