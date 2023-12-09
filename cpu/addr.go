package cpu

import "fmt"

const (
	AddrModeImp AddrMode = iota + 1
	AddrModeAcc
	AddrModeImm
	AddrModeZp
	AddrModeZpX
	AddrModeZpY
	AddrModeAbs
	AddrModeAbsX
	AddrModeAbsY
	AddrModeInd
	AddrModeIndX
	AddrModeIndY
	AddrModeRel
)

func (cpu *CPU) fetch(mode AddrMode) operand {
	switch mode {
	case AddrModeImp:
		return operand{
			mode: mode,
		}

	case AddrModeAcc:
		return operand{
			mode: mode,
		}

	case AddrModeImm:
		addr := cpu.PC
		cpu.PC++

		return operand{
			mode: mode,
			addr: addr,
		}

	case AddrModeZp:
		addr := uint16(cpu.mem.Read(cpu.PC))
		cpu.PC++

		return operand{
			mode: mode,
			addr: addr,
		}

	case AddrModeZpX:
		addr := (uint16(cpu.mem.Read(cpu.PC)) + uint16(cpu.X)) & 0x00FF
		cpu.PC++

		return operand{
			mode: mode,
			addr: addr,
		}

	case AddrModeZpY:
		addr := (uint16(cpu.mem.Read(cpu.PC)) + uint16(cpu.Y)) & 0x00FF
		cpu.PC++

		return operand{
			mode: mode,
			addr: addr,
		}

	case AddrModeAbs:
		addr := readWord(cpu.mem, cpu.PC)
		cpu.PC += 2

		return operand{
			mode: mode,
			addr: addr,
		}

	case AddrModeAbsX:
		addr := readWord(cpu.mem, cpu.PC)
		cpu.PC += 2

		addrX := addr + uint16(cpu.X)
		pageCross := addr&0xFF00 != addrX&0xFF00

		return operand{
			mode:      mode,
			addr:      addrX,
			pageCross: pageCross,
		}

	case AddrModeAbsY:
		addr := readWord(cpu.mem, cpu.PC)
		cpu.PC += 2

		addrY := addr + uint16(cpu.Y)
		pageCross := addr&0xFF00 != addrY&0xFF00

		return operand{
			mode:      mode,
			addr:      addrY,
			pageCross: pageCross,
		}

	case AddrModeInd:
		ptrAddr := readWord(cpu.mem, cpu.PC)
		addr := readWordBug(cpu.mem, ptrAddr)
		cpu.PC += 2

		return operand{
			mode: mode,
			addr: addr,
		}

	case AddrModeIndX:
		ptrAddr := (uint16(cpu.mem.Read(cpu.PC)) + uint16(cpu.X)) & 0x00FF
		addr := readWordBug(cpu.mem, ptrAddr)
		cpu.PC++

		return operand{
			mode: mode,
			addr: addr,
		}

	case AddrModeIndY:
		ptrAddr := uint16(cpu.mem.Read(cpu.PC))
		cpu.PC++

		addr := readWordBug(cpu.mem, ptrAddr)
		addrY := addr + uint16(cpu.Y)
		pageCross := addrY&0xFF00 != addr&0xFF00

		return operand{
			mode:      mode,
			addr:      addrY,
			pageCross: pageCross,
		}

	case AddrModeRel:
		rel := uint16(cpu.mem.Read(cpu.PC))
		cpu.PC++

		if rel&(1<<7) != 0 {
			rel |= 0xFF00
		}

		addr := cpu.PC + rel
		pageCross := addr&0xFF00 != cpu.PC&0xFF00

		return operand{
			mode:      mode,
			addr:      addr,
			pageCross: pageCross,
		}

	default:
		panic(fmt.Sprintf("invalid addressing mode: %d", mode))
	}
}
