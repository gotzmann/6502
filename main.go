package main

import (
	"fmt"
	"os"

	"github.com/gotzmann/6502/cpu"
)

type Memory struct {
	Buffer []byte // FIXME: 64 * 1024
}

func (mem Memory) Read(addr uint16) uint8 {
	return mem.Buffer[addr]
}

func (mem Memory) Write(addr uint16, data uint8) {
	mem.Buffer[addr] = data
}

func main() {
	ROM := "./roms/test.rom"
	fmt.Printf("\n[ START ] Atari 800 XL")

	buffer, err := os.ReadFile(ROM)
	if err != nil {
		fmt.Printf("\n[ ERROR ] Could not read ROM from disk\n")
		return
	}

	mem := Memory{
		Buffer: buffer,
	}

	cpu := cpu.New(mem)
	// cpu.Reset()
	// 0xFFFC - reset vector
	cpu.PC = 0x400

	// cpu.JumpTo(0x0400);
	// cpu.BreakAt(0x3469);
	// while(!tc.IsTrap())
	// {
	//     cpu.Execute();
	// }
	// Assert(cpu.PC == 0x3469);

	//for i := 0; i < 400; i++ {
	// BRK     : 85_716_338
	// ADC DEC : 84_030_668
	// SBC DEC : 84_042_280
	maxCycles := uint64(84_042_300)
	endOfFunctionalTest := uint16(0x3469)
	for cpu.Cycles < maxCycles && cpu.PC != endOfFunctionalTest {
		// fmt.Printf(" - %d - ", i)
		cpu.Tick()
	}

	fmt.Printf("\n[ STOP ]\n")
}
