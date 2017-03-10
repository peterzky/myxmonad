package main

import (
	"fmt"
	// "log"
	"os/exec"
	"strings"
)

func emacsclient(s string) string {
	cmd := exec.Command("emacsclient", "-e", s)
	stdout, err := cmd.Output()
	if err != nil {
		// log.Fatal(err)
		return ""
	}
	str := string(stdout)
	return str
}

func clockP() bool {
	p := emacsclient("(org-pomodoro-active-p)")
	if p == "t\n" {
		return true
	} else {
		return false
	}

}

func getTitle() string {
	str := emacsclient("(org-clock-get-clock-string)")
	r := strings.NewReplacer("(", "", ")", "")
	strAlter := strings.Split(str, "\"")
	s := r.Replace(strAlter[1])
	array := strings.Split(s, " ")
	title := strings.Join(array[2:], " ")
	return title
}

func getTime() string {
	str := emacsclient("(org-pomodoro-format-seconds)")
	output := strings.Trim(str, "\n")
	time := strings.Replace(output, "\"", "", -1)
	return time
}

func colorize(color string, target string) string {
	str := "<fc=" + color + ">" + target + "</fc>"
	return str
}

func getState() string {
	str := emacsclient("(symbol-value 'org-pomodoro-state)")
	state := strings.Trim(str, "\n")
	return state
}

func stateDispatcher(state string, timer string) (output string) {
	switch state {
	case ":pomodoro":
		output = colorize("red", timer)
		return
	case ":short-break":
		output = colorize("cyan", timer)
		return
	case ":long-break":
		output = colorize("blue", timer)
		return
	default:
		output = ""
		return
	}

}

func main() {
	if clockP() {
		timer := getTime()
		title := getTitle()
		state := getState()
		t := stateDispatcher(state, timer)
		l := colorize("#D3B53D", title)
		fmt.Printf("[%s] %s", t, l)
	} else {
		fmt.Println("")
	}
}
