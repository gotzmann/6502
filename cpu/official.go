package cpu

func (cpu *CPU) execute(instr Instruction, arg operand) bool {
	switch instr.Name {
	case "NOP":
		// do nothing
	case "LDA":
		cpu.lda(cpu.RAM, arg)
	case "STA":
		cpu.sta(cpu.RAM, arg)
	case "LDX":
		cpu.ldx(cpu.RAM, arg)
	case "STX":
		cpu.stx(cpu.RAM, arg)
	case "LDY":
		cpu.ldy(cpu.RAM, arg)
	case "STY":
		cpu.sty(cpu.RAM, arg)
	case "TAX":
		cpu.tax(cpu.RAM, arg)
	case "TXA":
		cpu.txa(cpu.RAM, arg)
	case "TAY":
		cpu.tay(cpu.RAM, arg)
	case "TYA":
		cpu.tya(cpu.RAM, arg)
	case "TSX":
		cpu.tsx(cpu.RAM, arg)
	case "TXS":
		cpu.txs(cpu.RAM, arg)
	case "PHA":
		cpu.pha(cpu.RAM, arg)
	case "PLA":
		cpu.pla(cpu.RAM, arg)
	case "PHP":
		cpu.php(cpu.RAM, arg)
	case "PLP":
		cpu.plp(cpu.RAM, arg)
	case "ADC":
		cpu.adc(cpu.RAM, arg)
	case "SBC":
		cpu.sbc(cpu.RAM, arg)
	case "AND":
		cpu.and(cpu.RAM, arg)
	case "ORA":
		cpu.ora(cpu.RAM, arg)
	case "EOR":
		cpu.eor(cpu.RAM, arg)
	case "CMP":
		cpu.cmp(cpu.RAM, arg)
	case "CPX":
		cpu.cpx(cpu.RAM, arg)
	case "CPY":
		cpu.cpy(cpu.RAM, arg)
	case "INC":
		cpu.inc(cpu.RAM, arg)
	case "DEC":
		cpu.dec(cpu.RAM, arg)
	case "INX":
		cpu.inx(cpu.RAM, arg)
	case "DEX":
		cpu.dex(cpu.RAM, arg)
	case "INY":
		cpu.iny(cpu.RAM, arg)
	case "DEY":
		cpu.dey(cpu.RAM, arg)
	case "ASL":
		cpu.asl(cpu.RAM, arg)
	case "LSR":
		cpu.lsr(cpu.RAM, arg)
	case "ROL":
		cpu.rol(cpu.RAM, arg)
	case "ROR":
		cpu.ror(cpu.RAM, arg)
	case "BIT":
		cpu.bit(cpu.RAM, arg)
	case "BCC":
		cpu.bcc(cpu.RAM, arg)
	case "BCS":
		cpu.bcs(cpu.RAM, arg)
	case "BEQ":
		cpu.beq(cpu.RAM, arg)
	case "BMI":
		cpu.bmi(cpu.RAM, arg)
	case "BNE":
		cpu.bne(cpu.RAM, arg)
	case "BPL":
		cpu.bpl(cpu.RAM, arg)
	case "BVC":
		cpu.bvc(cpu.RAM, arg)
	case "BVS":
		cpu.bvs(cpu.RAM, arg)
	case "CLC":
		cpu.clc(cpu.RAM, arg)
	case "CLD":
		cpu.cld(cpu.RAM, arg)
	case "CLI":
		cpu.cli(cpu.RAM, arg)
	case "CLV":
		cpu.clv(cpu.RAM, arg)
	case "SEC":
		cpu.sec(cpu.RAM, arg)
	case "SED":
		cpu.sed(cpu.RAM, arg)
	case "SEI":
		cpu.sei(cpu.RAM, arg)
	case "BRK":
		cpu.brk(cpu.RAM, arg)
	case "RTI":
		cpu.rti(cpu.RAM, arg)
	case "JMP":
		cpu.jmp(cpu.RAM, arg)
	case "JSR":
		cpu.jsr(cpu.RAM, arg)
	case "RTS":
		cpu.rts(cpu.RAM, arg)

	// -- illegal ?! opcodes

	case "*DCP":
		cpu.dcp(cpu.RAM, arg)
	case "*ISB":
		cpu.isb(cpu.RAM, arg)
	case "*SLO":
		cpu.slo(cpu.RAM, arg)
	case "*RLA":
		cpu.rla(cpu.RAM, arg)
	case "*SRE":
		cpu.sre(cpu.RAM, arg)
	case "*RRA":
		cpu.rra(cpu.RAM, arg)
	case "*LAX":
		cpu.lax(cpu.RAM, arg)
	case "*SAX":
		cpu.sax(cpu.RAM, arg)
	case "*SBC": // USBC
		cpu.sbc(cpu.RAM, arg)
	case "*NOP":
		cpu.nop(cpu.RAM, arg)
	case "???":
		cpu.nop(cpu.RAM, arg)

	default:
		return false
	}

	return true
}

// lda loads the accumulator with a value from memory.
func (cpu *CPU) lda(mem Memory, arg operand) {
	cpu.A = mem.Read(arg.addr)
	cpu.setZN(cpu.A)

	if arg.pageCross {
		cpu.Halt++
	}
}

// sta stores the accumulator in memory.
func (cpu *CPU) sta(mem Memory, arg operand) {
	mem.Write(arg.addr, cpu.A)
}

// ldx loads the X register with a value from memory.
func (cpu *CPU) ldx(mem Memory, arg operand) {
	cpu.X = mem.Read(arg.addr)
	cpu.setZN(cpu.X)

	if arg.pageCross {
		cpu.Halt++
	}
}

// stx stores the X register in memory.
func (cpu *CPU) stx(mem Memory, arg operand) {
	mem.Write(arg.addr, cpu.X)
}

// ldy loads the Y register with a value from memory.
func (cpu *CPU) ldy(mem Memory, arg operand) {
	cpu.Y = mem.Read(arg.addr)
	cpu.setZN(cpu.Y)

	if arg.pageCross {
		cpu.Halt++
	}
}

// sty stores the Y register in memory.
func (cpu *CPU) sty(mem Memory, arg operand) {
	mem.Write(arg.addr, cpu.Y)
}

// tax transfers the accumulator to the X register.
func (cpu *CPU) tax(mem Memory, arg operand) {
	cpu.X = cpu.A
	cpu.setZN(cpu.X)
}

// txa transfers the X register to the accumulator.
func (cpu *CPU) txa(mem Memory, arg operand) {
	cpu.A = cpu.X
	cpu.setZN(cpu.A)
}

// tay transfers the accumulator to the Y register.
func (cpu *CPU) tay(mem Memory, arg operand) {
	cpu.Y = cpu.A
	cpu.setZN(cpu.Y)
}

// tya transfers the Y register to the accumulator.
func (cpu *CPU) tya(mem Memory, arg operand) {
	cpu.A = cpu.Y
	cpu.setZN(cpu.A)
}

// tsx transfers the stack pointer to the X register.
func (cpu *CPU) tsx(mem Memory, arg operand) {
	cpu.X = cpu.SP
	cpu.setZN(cpu.X)
}

// txs transfers the X register to the stack pointer.
func (cpu *CPU) txs(mem Memory, arg operand) {
	cpu.SP = cpu.X
}

// pha pushes the accumulator onto the stack.
func (cpu *CPU) pha(mem Memory, arg operand) {
	cpu.push(cpu.A)
}

// pla pops a value from the stack into the accumulator.
func (cpu *CPU) pla(mem Memory, arg operand) {
	cpu.A = cpu.pop()
	cpu.setZN(cpu.A)
}

// php pushes the processor status onto the stack.
func (cpu *CPU) php(mem Memory, arg operand) {
	cpu.push(uint8(cpu.P) | 0x10)
}

// plp pops a value from the stack into the processor status.
func (cpu *CPU) plp(mem Memory, arg operand) {
	cpu.P = Flags(cpu.pop())&0xEF | 0x20
}

// inc increments a value in memory.
func (cpu *CPU) inc(mem Memory, arg operand) {
	val := mem.Read(arg.addr) + 1
	mem.Write(arg.addr, val)
	cpu.setZN(val)
}

// inx increments the X register.
func (cpu *CPU) inx(mem Memory, arg operand) {
	cpu.X++
	cpu.setZN(cpu.X)
}

// iny increments the Y register.
func (cpu *CPU) iny(mem Memory, arg operand) {
	cpu.Y++
	cpu.setZN(cpu.Y)
}

// dec decrements a value in memory.
func (cpu *CPU) dec(mem Memory, arg operand) {
	data := mem.Read(arg.addr) - 1
	mem.Write(arg.addr, data)
	cpu.setZN(data)
}

// dex decrements the X register.
func (cpu *CPU) dex(mem Memory, arg operand) {
	cpu.X--
	cpu.setZN(cpu.X)
}

// dey decrements the Y register.
func (cpu *CPU) dey(mem Memory, arg operand) {
	cpu.Y--
	cpu.setZN(cpu.Y)
}

// adc adds a value from memory to the accumulator with carry. The carry flag is
// set if the result is greater than 255. The overflow flag is set if the result
// is greater than 127 or less than -128 (incorrect sign bit).
func (cpu *CPU) adc(mem Memory, arg operand) {
	var (
		a = uint16(cpu.A)
		b = uint16(mem.Read(arg.addr))
	)

	r := a + b + uint16(cpu.carried())
	overflow := (a^b)&0x80 == 0 && (a^r)&0x80 != 0

	cpu.setFlag(flagCarry, r > 0xFF)
	cpu.setFlag(flagOverflow, overflow)
	cpu.A = uint8(r)
	cpu.setZN(cpu.A)

	if arg.pageCross {
		cpu.Halt++
	}
}

func (cpu *CPU) sbc(mem Memory, arg operand) {
	var (
		a = uint16(cpu.A)
		b = uint16(mem.Read(arg.addr))
	)

	r := a - b - uint16(1-cpu.carried())
	overflow := (a^b)&0x80 != 0 && (a^r)&0x80 != 0

	cpu.setFlag(flagCarry, r < 0x100)
	cpu.setFlag(flagOverflow, overflow)
	cpu.A = uint8(r)
	cpu.setZN(cpu.A)

	if arg.pageCross {
		cpu.Halt++
	}
}

func (cpu *CPU) and(mem Memory, arg operand) {
	cpu.A &= mem.Read(arg.addr)
	cpu.setZN(cpu.A)

	if arg.pageCross {
		cpu.Halt++
	}
}

func (cpu *CPU) ora(mem Memory, arg operand) {
	cpu.A |= mem.Read(arg.addr)
	cpu.setZN(cpu.A)

	if arg.pageCross {
		cpu.Halt++
	}
}

func (cpu *CPU) eor(mem Memory, arg operand) {
	cpu.A ^= mem.Read(arg.addr)
	cpu.setZN(cpu.A)

	if arg.pageCross {
		cpu.Halt++
	}
}

func (cpu *CPU) asl(mem Memory, arg operand) {
	var (
		write = func(v uint8) { mem.Write(arg.addr, v) }
		read  = func() uint8 { return mem.Read(arg.addr) }
	)

	if arg.mode == AddrModeAcc {
		write = func(v uint8) { cpu.A = v }
		read = func() uint8 { return cpu.A }
	}

	data := read()
	cpu.setFlag(flagCarry, data&0x80 != 0)
	data <<= 1
	cpu.setZN(data)
	write(data)
}

func (cpu *CPU) lsr(mem Memory, arg operand) {
	var (
		write = func(v uint8) { mem.Write(arg.addr, v) }
		read  = func() uint8 { return mem.Read(arg.addr) }
	)

	if arg.mode == AddrModeAcc {
		write = func(v uint8) { cpu.A = v }
		read = func() uint8 { return cpu.A }
	}

	data := read()
	cpu.setFlag(flagCarry, data&0x01 != 0)
	data >>= 1
	cpu.setZN(data)
	write(data)
}

func (cpu *CPU) rol(mem Memory, arg operand) {
	var (
		write = func(v uint8) { mem.Write(arg.addr, v) }
		read  = func() uint8 { return mem.Read(arg.addr) }
	)

	if arg.mode == AddrModeAcc {
		write = func(v uint8) { cpu.A = v }
		read = func() uint8 { return cpu.A }
	}

	data := read()
	carr := cpu.carried()

	cpu.setFlag(flagCarry, data&0x80 != 0)
	data = data<<1 | carr
	cpu.setZN(data)
	write(data)
}

func (cpu *CPU) ror(mem Memory, arg operand) {
	var (
		write = func(v uint8) { mem.Write(arg.addr, v) }
		read  = func() uint8 { return mem.Read(arg.addr) }
	)

	if arg.mode == AddrModeAcc {
		write = func(v uint8) { cpu.A = v }
		read = func() uint8 { return cpu.A }
	}

	data := read()
	carr := cpu.carried()

	cpu.setFlag(flagCarry, data&0x01 != 0)
	data = data>>1 | carr<<7
	cpu.setZN(data)
	write(data)
}

func (cpu *CPU) bit(mem Memory, arg operand) {
	data := mem.Read(arg.addr)
	cpu.setFlag(flagZero, cpu.A&data == 0)
	cpu.setFlag(flagOverflow, data&(1<<6) != 0)
	cpu.setFlag(flagNegative, data&(1<<7) != 0)
}

func (cpu *CPU) cmp(mem Memory, arg operand) {
	data := uint16(cpu.A) - uint16(mem.Read(arg.addr))
	cpu.setFlag(flagCarry, data < 0x100)
	cpu.setZN(uint8(data))

	if arg.pageCross {
		cpu.Halt++
	}
}

func (cpu *CPU) cpx(mem Memory, arg operand) {
	data := uint16(cpu.X) - uint16(mem.Read(arg.addr))
	cpu.setFlag(flagCarry, data < 0x100)
	cpu.setZN(uint8(data))

	if arg.pageCross {
		cpu.Halt++
	}
}

func (cpu *CPU) cpy(mem Memory, arg operand) {
	data := uint16(cpu.Y) - uint16(mem.Read(arg.addr))
	cpu.setFlag(flagCarry, data < 0x100)
	cpu.setZN(uint8(data))

	if arg.pageCross {
		cpu.Halt++
	}
}

func (cpu *CPU) jmp(mem Memory, arg operand) {
	cpu.PC = arg.addr
}

func (cpu *CPU) jsr(mem Memory, arg operand) {
	cpu.pushWord(cpu.PC - 1)
	cpu.PC = arg.addr
}

func (cpu *CPU) rts(mem Memory, arg operand) {
	addr := cpu.popWord()
	cpu.PC = addr + 1
}

func (cpu *CPU) bcc(mem Memory, arg operand) {
	if !cpu.getFlag(flagCarry) {
		cpu.PC = arg.addr
		cpu.Halt += 1

		if arg.pageCross {
			cpu.Halt += 2
		}
	}
}

func (cpu *CPU) bcs(mem Memory, arg operand) {
	if cpu.getFlag(flagCarry) {
		cpu.PC = arg.addr
		cpu.Halt += 1

		if arg.pageCross {
			cpu.Halt += 2
		}
	}
}

func (cpu *CPU) beq(mem Memory, arg operand) {
	if cpu.getFlag(flagZero) {
		cpu.PC = arg.addr
		cpu.Halt += 1

		if arg.pageCross {
			cpu.Halt += 2
		}
	}
}

func (cpu *CPU) bmi(mem Memory, arg operand) {
	if cpu.getFlag(flagNegative) {
		cpu.PC = arg.addr
		cpu.Halt += 1

		if arg.pageCross {
			cpu.Halt += 2
		}
	}
}

func (cpu *CPU) bne(mem Memory, arg operand) {
	if !cpu.getFlag(flagZero) {
		cpu.PC = arg.addr
		cpu.Halt += 1

		if arg.pageCross {
			cpu.Halt += 2
		}
	}
}

func (cpu *CPU) bpl(mem Memory, arg operand) {
	if !cpu.getFlag(flagNegative) {
		cpu.PC = arg.addr
		cpu.Halt += 1

		if arg.pageCross {
			cpu.Halt += 2
		}
	}
}

func (cpu *CPU) bvc(mem Memory, arg operand) {
	if !cpu.getFlag(flagOverflow) {
		cpu.PC = arg.addr
		cpu.Halt += 1

		if arg.pageCross {
			cpu.Halt += 2
		}
	}
}

func (cpu *CPU) bvs(mem Memory, arg operand) {
	if cpu.getFlag(flagOverflow) {
		cpu.PC = arg.addr
		cpu.Halt += 1

		if arg.pageCross {
			cpu.Halt += 2
		}
	}
}

func (cpu *CPU) brk(mem Memory, arg operand) {
	cpu.pushWord(cpu.PC)
	cpu.push(uint8(cpu.P & flagBreak))
	cpu.PC = readWord(mem, vecIRQ)
}

func (cpu *CPU) clc(mem Memory, arg operand) {
	cpu.setFlag(flagCarry, false)
}

func (cpu *CPU) cld(mem Memory, arg operand) {
	cpu.setFlag(flagDecimal, false)
}

func (cpu *CPU) cli(mem Memory, arg operand) {
	cpu.setFlag(flagInterrupt, false)
}

func (cpu *CPU) clv(mem Memory, arg operand) {
	cpu.setFlag(flagOverflow, false)
}

func (cpu *CPU) sec(mem Memory, arg operand) {
	cpu.setFlag(flagCarry, true)
}

func (cpu *CPU) sed(mem Memory, arg operand) {
	cpu.setFlag(flagDecimal, true)
}

func (cpu *CPU) sei(mem Memory, arg operand) {
	cpu.setFlag(flagInterrupt, true)
}

func (cpu *CPU) rti(mem Memory, arg operand) {
	cpu.P = Flags(cpu.pop())&0xEF | 0x20
	cpu.setFlag(flagBreak, false)
	cpu.PC = cpu.popWord()
}

// -- Illegal ?! opcodes

func (cpu *CPU) nop(mem Memory, arg operand) {
	if arg.pageCross {
		cpu.Halt += 1
	}
}

// dcp is dec + cmp
func (cpu *CPU) dcp(mem Memory, arg operand) {
	data := mem.Read(arg.addr) - 1
	mem.Write(arg.addr, data)

	data2 := uint16(cpu.A) - uint16(data)
	cpu.setFlag(flagCarry, data2 < 0x100)
	cpu.setZN(uint8(data2))
}

// isb is inc + sbc
func (cpu *CPU) isb(mem Memory, arg operand) {
	var (
		data = mem.Read(arg.addr) + 1
		a    = uint16(cpu.A)
		b    = uint16(data)
	)

	r := a - b - uint16(1-cpu.carried())
	overflow := (a^b)&0x80 != 0 && (a^r)&0x80 != 0

	mem.Write(arg.addr, data)
	cpu.setFlag(flagCarry, r < 0x100)
	cpu.setFlag(flagOverflow, overflow)
	cpu.A = uint8(r)
	cpu.setZN(cpu.A)
}

// lax is lda + ldx
func (cpu *CPU) lax(mem Memory, arg operand) {
	data := mem.Read(arg.addr)
	cpu.A, cpu.X = data, data
	cpu.setZN(cpu.X)

	if arg.pageCross {
		cpu.Halt += 1
	}
}

// rla is rol + and
func (cpu *CPU) rla(mem Memory, arg operand) {
	data := mem.Read(arg.addr)
	carr := cpu.carried()

	cpu.setFlag(flagCarry, data&0x80 != 0)
	data = (data << 1) | carr
	mem.Write(arg.addr, data)
	cpu.A &= data
	cpu.setZN(cpu.A)
}

// sax is sta + stx
func (cpu *CPU) sax(mem Memory, arg operand) {
	data := cpu.A & cpu.X
	mem.Write(arg.addr, data)
}

// slo is asl + ora
func (cpu *CPU) slo(mem Memory, arg operand) {
	data := mem.Read(arg.addr)
	cpu.setFlag(flagCarry, data&0x80 != 0)

	data <<= 1
	mem.Write(arg.addr, data)

	cpu.A |= data
	cpu.setZN(cpu.A)
}

// sre is lsr + eor
func (cpu *CPU) sre(mem Memory, arg operand) {
	data := mem.Read(arg.addr)
	cpu.setFlag(flagCarry, data&0x01 != 0)

	data >>= 1
	mem.Write(arg.addr, data)

	cpu.A ^= data
	cpu.setZN(cpu.A)
}

// rra is ror + adc
func (cpu *CPU) rra(mem Memory, arg operand) {
	data := mem.Read(arg.addr)
	carr := cpu.carried()

	// ror
	cpu.setFlag(flagCarry, data&0x01 != 0)
	data = data>>1 | carr<<7
	mem.Write(arg.addr, data)

	// adc
	a, b := uint16(cpu.A), uint16(data)
	r := a + b + uint16(cpu.carried())
	overflow := (a^b)&0x80 == 0 && (a^r)&0x80 != 0

	cpu.setFlag(flagOverflow, overflow)
	cpu.setFlag(flagCarry, r > 0xFF)
	cpu.A = uint8(r)
	cpu.setZN(cpu.A)
}
