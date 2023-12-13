package main

import (
	"fmt"
	"os"

	gui "github.com/gen2brain/raylib-go/raylib"
	"github.com/gotzmann/6502/cpu"
)

const (
	MEM_SIZE            = 65536
	CYCLES_PER_SEC      = 1792080
	FRAMES_PER_SEC      = 60
	MILLISEC_PER_FRAME  = 1000 / FRAMES_PER_SEC
	CYCLES_PER_FRAME    = CYCLES_PER_SEC / FRAMES_PER_SEC
	TOTAL_SCANLINES     = 262
	CYCLES_PER_SCANLINE = CYCLES_PER_FRAME / TOTAL_SCANLINES
	PLAYFIELD_NARROW    = 256
	PLAYFIELD_NORMAL    = 320
	PLAYFIELD_WIDE      = 384
	FRAME_WIDTH         = PLAYFIELD_WIDE
	FRAME_HEIGHT        = TOTAL_SCANLINES
	VISIBLE_SCANLINES   = 240
	TOP_SCANLINES       = 8
	VBLANK_SCANLINE     = TOP_SCANLINES + VISIBLE_SCANLINES
	FRAME_SIZE          = FRAME_WIDTH * FRAME_HEIGHT
	FRAME_BYTES         = FRAME_SIZE * 4 // 4 bytes per pixel
	SCREEN_WIDTH        = FRAME_WIDTH
	SCREEN_HEIGHT       = VISIBLE_SCANLINES
	SCREEN_SIZE         = SCREEN_WIDTH * SCREEN_HEIGHT * 4 // 4 bytes per pixel
	SCREEN_OFFSET       = SCREEN_WIDTH * TOP_SCANLINES
	SCREEN_SCALE        = 2
	FRAME_TIME          = 1.0 / FRAMES_PER_SEC
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
	// ROM := "./roms/test.rom"
	ROM := "./roms/os.rom"

	fmt.Printf("\n[ START ] Atari 800 XL")

	// Generally OS images you find around the place will be exactly 16,384 bytes so no need to strip headers.
	// The order is 4K that appears at $C000 then 2K Self Test at $5000 then 2K FP at $D800 then 8K at $E000.
	// The MMU takes care of making things appear where they should, the programmer only need ensure the sections
	// are placed in the right order.

	buffer, err := os.ReadFile(ROM)
	if err != nil {
		fmt.Printf("\n[ ERROR ] Could not read ROM from disk\n")
		return
	}

	mem := Memory{
		Buffer: make([]byte, 64*1024),
	}

	fmt.Printf("\n[ INFO ] Copy %dK ROM file into RAM starting from 0xC000...", len(buffer)/1024)
	for i := 0; i < len(buffer); i++ {
		mem.Buffer[0xC000+i] = buffer[i]
	}

	// Move Self Test for proper destination
	for i := 0; i <= 0x7FF; i++ {
		mem.Buffer[0x5000+i] = buffer[0x1000+i]
	}

	fmt.Printf("\n[ INFO ] 0xFFFC = %02X%02X", mem.Buffer[0xFFFC+1], mem.Buffer[0xFFFC])

	//os.Exit(0)

	cpu := cpu.New(mem)
	// cpu.Reset()
	// 0xFFFC - reset vector
	// cpu.PC = 0x400
	cpu.PC = 0xC2C8 // 0xC8FC // 0xC2AA // 0xFFFC // 0xFFFC = C2AA

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

	//for cpu.Cycles < 32 /*&& cpu.PC != 0x3469*/ {
	//for cpu.Cycles < 10_000_000 {
	//	cpu.Tick()
	//}

	gui.InitWindow(800, 450, "raylib [core] example - basic window")
	defer gui.CloseWindow()

	gui.SetTargetFPS(30)

	for cpu.Cycles < 10_000_000 && !gui.WindowShouldClose() {

		cpu.Tick()

		gui.BeginDrawing()

		gui.ClearBackground(gui.RayWhite)
		gui.DrawText("Congrats! You created your first window!", 190, 200, 20, gui.LightGray)

		gui.EndDrawing()
	}

	fmt.Printf("\n[ STOP ]\n")
}
