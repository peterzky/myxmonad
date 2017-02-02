package main

import (
	"fmt"
	// "log"
	"os/exec"
	"strings"
)

func emacsclient(s string) string{
	cmd := exec.Command("emacsclient", "-e",s)
	stdout, err := cmd.Output()
	if err != nil {
		// log.Fatal(err)
		return ""
	}
	str := string(stdout)
	return str
}

func clockP() bool{
	p := emacsclient("(org-clocking-p)")
	if p == "t\n"{
		return true
	}else{
		return false
	}
	
}

func parser() (string, string){
	str := emacsclient("(org-clock-get-clock-string)")
	r := strings.NewReplacer("(","",")","")
	r2 := strings.NewReplacer("[","","]","")
	strAlter := strings.Split(str,"\"")
	s := r.Replace(strAlter[1])
	array := strings.Split(s," ")
	time := r2.Replace(array[1])
	title := strings.Join(array[2:]," ")
	return time,title
}

func colorize(color string,target string) string{
	str := "<fc=" + color + ">" + target + "</fc>"
	return str
}


func main() {
	time, title := parser()
	if clockP() {
		t := colorize("darkgreen",time)
		l := colorize("#3399FF",title)
		fmt.Printf("[%s] %s",t,l)
	}else{
		fmt.Println("")
	}
}
