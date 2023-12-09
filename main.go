package main

import (
	"fmt"

	"github.com/gotzmann/6502/cpu"
)

type Memory struct {
	buffer [64 * 1024]uint8
}

func (mem Memory) Read(addr uint16) uint8 {
	return mem.buffer[addr]
}

func (mem Memory) Write(addr uint16, data uint8) {
	mem.buffer[addr] = data
}

func main() {
	fmt.Printf("\n[ START ]")
	mem := Memory{}
	cpu := cpu.New(mem)
	cpu.Tick()
}
