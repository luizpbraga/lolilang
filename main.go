package main

type b struct {
	a struct {
		a bool
	}
}

func main() {
	if (b{}).(a{}).b {
	}
}
