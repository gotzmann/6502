package cpu

import (
	"fmt"
)

/*
void CPU::Reset()

	{
	    if(m_callbacks)
	    {
	        m_callbacks->OnReset();
	    }
	    A = X = Y    = 0;
	    F            = CPU::IGNORED_FLAG;
	    S            = 0xFF;
	    PC           = m_RAM->GetW(0xFFFC);
	    BRK          = 0xFFFC;
	    m_cycles     = 0;
	    m_seconds    = 0;
	    m_irqPending = false;
	    m_nmiPending = false;
	    m_callStack.clear();
	    m_callStack.push_back(make_pair(PC, "Entry"));
	}
*/
type CPU struct {

	// -- 6502 registers

	A  uint8
	X  uint8
	Y  uint8
	P  Flags
	SP uint8
	PC uint16

	// --

	RAM Memory // FIXME: Pointer?

	// AllowIllegal bool   // Handle illegal opcodes
	Cycles uint64 // Number of cycles executed
	Halt   int    // Number of cycles to wait

	interrupt Interrupt
}

func New(mem Memory) *CPU {
	return &CPU{
		RAM: mem,
	}
}

type (
	Flags     uint8
	AddrMode  int
	Interrupt int
)

/*

Naming From ASM Functional Tests

000000r 1               carry   equ %00000001   ;flag bits in status
000000r 1               zero    equ %00000010
000000r 1               intdis  equ %00000100
000000r 1               decmode equ %00001000
000000r 1               break   equ %00010000
000000r 1               reserv  equ %00100000
000000r 1               overfl  equ %01000000
000000r 1               minus   equ %10000000
*/

/*
https://www.masswerk.at/6502/6502_instruction_set.html#description
SR Flags (bit 7 to bit 0)
N	Negative
V	Overflow
-	ignored
B	Break
D	Decimal (use BCD for arithmetics)
I	Interrupt (IRQ disable)
Z	Zero
C	Carry
*/

const (
	flagCarry     Flags = 1 << 0
	flagZero            = 1 << 1
	flagInterrupt       = 1 << 2
	flagDecimal         = 1 << 3
	flagBreak           = 1 << 4
	flagReserved        = 1 << 5
	flagOverflow        = 1 << 6
	flagNegative        = 1 << 7
)

const (
	vecNMI   uint16 = 0xFFFA // Non-maskable interrupt vector
	vecReset uint16 = 0xFFFC // Reset vector
	vecIRQ   uint16 = 0xFFFE // Interrupt request vector
)

const (
	interruptNMI Interrupt = iota + 1
	interruptIRQ
)

type operand struct {
	mode      AddrMode
	addr      uint16
	pageCross bool
}

func (cpu *CPU) getFlag(flag Flags) bool {
	return cpu.P&flag != 0
}

func (cpu *CPU) setFlag(flag Flags, value bool) {
	if value {
		cpu.P |= flag
		return
	}

	cpu.P &= 0xFF - flag
}

// setZN sets the zero and negative flags based on the given value, which is
// assumed to be the result of an operation.
func (cpu *CPU) setZN(value uint8) {
	cpu.setFlag(flagZero, value == 0)
	cpu.setFlag(flagNegative, value&0x80 != 0)
}

// carried returns 1 if the carry flag is set, otherwise 0.
func (cpu *CPU) carried() uint8 {
	if cpu.getFlag(flagCarry) {
		return 1
	}

	return 0
}

// push pushes a byte onto the stack.
func (cpu *CPU) push(data uint8) {
	cpu.RAM.Write(0x0100|uint16(cpu.SP), data)
	cpu.SP--
}

// pop pops a byte from the stack.
func (cpu *CPU) pop() uint8 {
	cpu.SP++
	return cpu.RAM.Read(0x0100 | uint16(cpu.SP))
}

// pushWord pushes a word onto the stack in little-endian order.
func (cpu *CPU) pushWord(data uint16) {
	cpu.push(uint8(data >> 8))
	cpu.push(uint8(data))
}

// popWord pops a word from the stack in little-endian order.
func (cpu *CPU) popWord() uint16 {
	lo := uint16(cpu.pop())
	hi := uint16(cpu.pop())

	return hi<<8 | lo
}

/*
// fetchOpcode reads the next opcode from memory and increments the program counter.
func (cpu *CPU) fetchOpcode() uint8 {
	opcode := cpu.RAM.Read(cpu.PC)
	cpu.PC++

	return opcode
}
*/
// Reset resets the CPU to its initial state. To match the behaviour of the real
// CPU, the next 6 cycles are skipped after a reset.
func (cpu *CPU) Reset() {
	cpu.PC = readWord(cpu.RAM, vecReset)
	cpu.SP = 0xFD // FIXME: Research needed: http://forum.6502.org/viewtopic.php?p=2959
	cpu.P = 0x24
	cpu.A = 0x00
	cpu.X = 0x00
	cpu.Y = 0x00

	cpu.Cycles = 0
	cpu.Halt = 6

	cpu.interrupt = 0
}

func (cpu *CPU) nmi() {
	cpu.pushWord(cpu.PC)
	cpu.push(uint8(cpu.P))
	cpu.setFlag(flagInterrupt, true)
	cpu.PC = readWord(cpu.RAM, vecNMI)
	cpu.Halt += 7
}

// TriggerNMI triggers a non-maskable interrupt on the next CPU cycle.
func (cpu *CPU) TriggerNMI() {
	cpu.interrupt = interruptNMI
}

func (cpu *CPU) irq() {
	cpu.pushWord(cpu.PC)
	cpu.push(uint8(cpu.P))
	cpu.setFlag(flagInterrupt, true)
	cpu.PC = readWord(cpu.RAM, vecIRQ)
	cpu.Halt += 7
}

// TriggerIRQ triggers an interrupt on the next CPU cycle.
// If the interrupt flag is set, the interrupt is ignored.
func (cpu *CPU) TriggerIRQ() {
	if cpu.getFlag(flagInterrupt) {
		return
	}

	cpu.interrupt = interruptIRQ
}

// Tick executes a single CPU cycle, returning true if the CPU has finished
// executing the current instruction.
func (cpu *CPU) Tick() bool {
	cpu.Cycles++

	if cpu.Halt > 0 {
		cpu.Halt--
		return cpu.Halt == 0
	}

	switch cpu.interrupt {
	case interruptIRQ:
		cpu.irq()
		cpu.interrupt = 0
	case interruptNMI:
		cpu.nmi()
		cpu.interrupt = 0
	}

	//var (
	//opcode = cpu.fetchOpcode()
	//instr  Instruction
	//ok     bool
	//)
	ok := false
	instr := Instruction{}
	opcode := cpu.RAM.Read(cpu.PC)
	cpu.PC++

	if instr, ok = Instructions[opcode]; !ok {
		panic(fmt.Sprintf("unknown opcode: %02X", opcode))
	}

	// -- DEBUG

	bytes := fmt.Sprintf("%02X", cpu.RAM.Read(cpu.PC-1))
	operands := "        "
	if instr.Size == 2 {
		bytes += fmt.Sprintf("%02X", cpu.RAM.Read(cpu.PC))
		operands = fmt.Sprintf(" $%02X    ", cpu.RAM.Read(cpu.PC))
	} else if instr.Size == 3 {
		bytes += fmt.Sprintf("%02X", cpu.RAM.Read(cpu.PC))
		bytes += fmt.Sprintf("%02X", cpu.RAM.Read(cpu.PC+1))
		// remember of Little Endian
		operands = fmt.Sprintf(" $%02X", cpu.RAM.Read(cpu.PC+1))
		operands += fmt.Sprintf("%02X", cpu.RAM.Read(cpu.PC))
	}

	fmt.Printf("\n%04X : %s\t%s%s\t# %04d", cpu.PC-1, bytes, instr.Name, operands, cpu.Cycles)

	opr := cpu.fetch(instr.AddrMode)
	ok = cpu.execute(instr, opr)

	//	if !ok && cpu.AllowIllegal {
	//		ok = cpu.executeIllegal(cpu.RAM, instr, opr)
	//	}

	if !ok {
		panic(fmt.Sprintf("invalid instruction: %s", instr.Name))
	}

	cpu.Halt += instr.Cycles - 1

	return false
}
