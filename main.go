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
	// SBC DEC : 84_030_700
	// SBC DEC : 84_031_307
	// SBC DEC : 84_031_318
	// SBC DEC : 84_152_215
	// OK : 84_170_000

	// 96_247_677

	for cpu.Cycles < 96_247_688 /*&& cpu.PC != 0x3469*/ {
		// fmt.Printf(" - %d - ", i)
		cpu.Tick()
	}

	fmt.Printf("\n[ STOP ]\n")
}
