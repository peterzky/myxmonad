package main

import (
	"fmt"
	"os/exec"
	"strings"
)

func fcitx() string{
	cmd := exec.Command("fcitx-remote")
	stdout, err := cmd.Output()
	if err != nil {
		return ""
	}
	str := strings.TrimSuffix(string(stdout), "\n")
	return str
}

func colorize(color string,target string) string{
	str := "<fc=" + color + ">" + target + "</fc>"
	return str
}

func main() {
	iem := fcitx()
	switch iem{
	case "1":
		fmt.Println(colorize("#7fff00", "US"))
	case "2":
		fmt.Println(colorize("#ffd700", "CN"))
	default:
		fmt.Println(colorize("#696969", "NA"))
	}
}
