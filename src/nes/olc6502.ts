import Bus from "./Bus";
import { Registers6502 } from "./Registers6502";

enum Flags6502 {
    C = (1 << 0),   // Carry bit
    Z = (1 << 1),   // Zero
    I = (1 << 2),   // Disable interrupts (except NMI)
    D = (1 << 3),   // Decimal mode (not used for this example)
    B = (1 << 4),   // Break
    U = (1 << 5),   // Unused
    V = (1 << 6),   // Overflow
    N = (1 << 7),   // Negative
}

class Instruction {
    constructor(
        public Mnemonic: string,
        public Operate: (() => number) | null = null,
        public AddressMode: (() => number) | null = null,
        public Cycles: number = 0
    ) {}
}

const STACKBASE = 0x0100

class Olc6502 {

    public Registers: Registers6502;

    private _fetched = 0x00;
    private _temp = 0x0000;
    private _addr_abs = 0x0000;
    private _addr_rel  = 0x0000;
    private _opcode = 0x00;
    private _cycles = 0x00;
    private _clock_count = 0x0000;

    private _bus: Bus | null = null;


    private _lookup: Instruction[] = [
        new Instruction ( "BRK", this.BRK, this.Immediate, 7 ),new Instruction ( "ORA", this.ORA, this.IndirectX, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 8 ),new Instruction ( "???", this.NOP, this.Implied, 3 ),new Instruction ( "ORA", this.ORA, this.ZeroPage, 3 ),new Instruction ( "ASL", this.ASL, this.ZeroPage, 5 ),new Instruction ( "???", this.Unknown, this.Implied, 5 ),new Instruction ( "PHP", this.PHP, this.Implied, 3 ),new Instruction ( "ORA", this.ORA, this.Immediate, 2 ),new Instruction ( "ASL", this.ASL, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "???", this.NOP, this.Implied, 4 ),new Instruction ( "ORA", this.ORA, this.Absolute, 4 ),new Instruction ( "ASL", this.ASL, this.Absolute, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 6 ),
        new Instruction ( "BPL", this.BPL, this.Relative, 2 ),new Instruction ( "ORA", this.ORA, this.IndirectY, 5 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 8 ),new Instruction ( "???", this.NOP, this.Implied, 4 ),new Instruction ( "ORA", this.ORA, this.ZeroPageX, 4 ),new Instruction ( "ASL", this.ASL, this.ZeroPageX, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 6 ),new Instruction ( "CLC", this.CLC, this.Implied, 2 ),new Instruction ( "ORA", this.ORA, this.AbsoluteY, 4 ),new Instruction ( "???", this.NOP, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 7 ),new Instruction ( "???", this.NOP, this.Implied, 4 ),new Instruction ( "ORA", this.ORA, this.AbsoluteX, 4 ),new Instruction ( "ASL", this.ASL, this.AbsoluteX, 7 ),new Instruction ( "???", this.Unknown, this.Implied, 7 ),
        new Instruction ( "JSR", this.JSR, this.Absolute, 6 ),new Instruction ( "AND", this.AND, this.IndirectX, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 8 ),new Instruction ( "BIT", this.BIT, this.ZeroPage, 3 ),new Instruction ( "AND", this.AND, this.ZeroPage, 3 ),new Instruction ( "ROL", this.ROL, this.ZeroPage, 5 ),new Instruction ( "???", this.Unknown, this.Implied, 5 ),new Instruction ( "PLP", this.PLP, this.Implied, 4 ),new Instruction ( "AND", this.AND, this.Immediate, 2 ),new Instruction ( "ROL", this.ROL, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "BIT", this.BIT, this.Absolute, 4 ),new Instruction ( "AND", this.AND, this.Absolute, 4 ),new Instruction ( "ROL", this.ROL, this.Absolute, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 6 ),
        new Instruction ( "BMI", this.BMI, this.Relative, 2 ),new Instruction ( "AND", this.AND, this.IndirectY, 5 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 8 ),new Instruction ( "???", this.NOP, this.Implied, 4 ),new Instruction ( "AND", this.AND, this.ZeroPageX, 4 ),new Instruction ( "ROL", this.ROL, this.ZeroPageX, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 6 ),new Instruction ( "SEC", this.SEC, this.Implied, 2 ),new Instruction ( "AND", this.AND, this.AbsoluteY, 4 ),new Instruction ( "???", this.NOP, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 7 ),new Instruction ( "???", this.NOP, this.Implied, 4 ),new Instruction ( "AND", this.AND, this.AbsoluteX, 4 ),new Instruction ( "ROL", this.ROL, this.AbsoluteX, 7 ),new Instruction ( "???", this.Unknown, this.Implied, 7 ),
        new Instruction ( "RTI", this.RTI, this.Implied, 6 ),new Instruction ( "EOR", this.EOR, this.IndirectX, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 8 ),new Instruction ( "???", this.NOP, this.Implied, 3 ),new Instruction ( "EOR", this.EOR, this.ZeroPage, 3 ),new Instruction ( "LSR", this.LSR, this.ZeroPage, 5 ),new Instruction ( "???", this.Unknown, this.Implied, 5 ),new Instruction ( "PHA", this.PHA, this.Implied, 3 ),new Instruction ( "EOR", this.EOR, this.Immediate, 2 ),new Instruction ( "LSR", this.LSR, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "JMP", this.JMP, this.Absolute, 3 ),new Instruction ( "EOR", this.EOR, this.Absolute, 4 ),new Instruction ( "LSR", this.LSR, this.Absolute, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 6 ),
        new Instruction ( "BVC", this.BVC, this.Relative, 2 ),new Instruction ( "EOR", this.EOR, this.IndirectY, 5 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 8 ),new Instruction ( "???", this.NOP, this.Implied, 4 ),new Instruction ( "EOR", this.EOR, this.ZeroPageX, 4 ),new Instruction ( "LSR", this.LSR, this.ZeroPageX, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 6 ),new Instruction ( "CLI", this.CLI, this.Implied, 2 ),new Instruction ( "EOR", this.EOR, this.AbsoluteY, 4 ),new Instruction ( "???", this.NOP, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 7 ),new Instruction ( "???", this.NOP, this.Implied, 4 ),new Instruction ( "EOR", this.EOR, this.AbsoluteX, 4 ),new Instruction ( "LSR", this.LSR, this.AbsoluteX, 7 ),new Instruction ( "???", this.Unknown, this.Implied, 7 ),
        new Instruction ( "RTS", this.RTS, this.Implied, 6 ),new Instruction ( "ADC", this.ADC, this.IndirectX, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 8 ),new Instruction ( "???", this.NOP, this.Implied, 3 ),new Instruction ( "ADC", this.ADC, this.ZeroPage, 3 ),new Instruction ( "ROR", this.ROR, this.ZeroPage, 5 ),new Instruction ( "???", this.Unknown, this.Implied, 5 ),new Instruction ( "PLA", this.PLA, this.Implied, 4 ),new Instruction ( "ADC", this.ADC, this.Immediate, 2 ),new Instruction ( "ROR", this.ROR, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "JMP", this.JMP, this.Indirect, 5 ),new Instruction ( "ADC", this.ADC, this.Absolute, 4 ),new Instruction ( "ROR", this.ROR, this.Absolute, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 6 ),
        new Instruction ( "BVS", this.BVS, this.Relative, 2 ),new Instruction ( "ADC", this.ADC, this.IndirectY, 5 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 8 ),new Instruction ( "???", this.NOP, this.Implied, 4 ),new Instruction ( "ADC", this.ADC, this.ZeroPageX, 4 ),new Instruction ( "ROR", this.ROR, this.ZeroPageX, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 6 ),new Instruction ( "SEI", this.SEI, this.Implied, 2 ),new Instruction ( "ADC", this.ADC, this.AbsoluteY, 4 ),new Instruction ( "???", this.NOP, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 7 ),new Instruction ( "???", this.NOP, this.Implied, 4 ),new Instruction ( "ADC", this.ADC, this.AbsoluteX, 4 ),new Instruction ( "ROR", this.ROR, this.AbsoluteX, 7 ),new Instruction ( "???", this.Unknown, this.Implied, 7 ),
        new Instruction ( "???", this.NOP, this.Implied, 2 ),new Instruction ( "STA", this.STA, this.IndirectX, 6 ),new Instruction ( "???", this.NOP, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 6 ),new Instruction ( "STY", this.STY, this.ZeroPage, 3 ),new Instruction ( "STA", this.STA, this.ZeroPage, 3 ),new Instruction ( "STX", this.STX, this.ZeroPage, 3 ),new Instruction ( "???", this.Unknown, this.Implied, 3 ),new Instruction ( "DEY", this.DEY, this.Implied, 2 ),new Instruction ( "???", this.NOP, this.Implied, 2 ),new Instruction ( "TXA", this.TXA, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "STY", this.STY, this.Absolute, 4 ),new Instruction ( "STA", this.STA, this.Absolute, 4 ),new Instruction ( "STX", this.STX, this.Absolute, 4 ),new Instruction ( "???", this.Unknown, this.Implied, 4 ),
        new Instruction ( "BCC", this.BCC, this.Relative, 2 ),new Instruction ( "STA", this.STA, this.IndirectY, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 6 ),new Instruction ( "STY", this.STY, this.ZeroPageX, 4 ),new Instruction ( "STA", this.STA, this.ZeroPageX, 4 ),new Instruction ( "STX", this.STX, this.ZeroPageY, 4 ),new Instruction ( "???", this.Unknown, this.Implied, 4 ),new Instruction ( "TYA", this.TYA, this.Implied, 2 ),new Instruction ( "STA", this.STA, this.AbsoluteY, 5 ),new Instruction ( "TXS", this.TXS, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 5 ),new Instruction ( "???", this.NOP, this.Implied, 5 ),new Instruction ( "STA", this.STA, this.AbsoluteX, 5 ),new Instruction ( "???", this.Unknown, this.Implied, 5 ),new Instruction ( "???", this.Unknown, this.Implied, 5 ),
        new Instruction ( "LDY", this.LDY, this.Immediate, 2 ),new Instruction ( "LDA", this.LDA, this.IndirectX, 6 ),new Instruction ( "LDX", this.LDX, this.Immediate, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 6 ),new Instruction ( "LDY", this.LDY, this.ZeroPage, 3 ),new Instruction ( "LDA", this.LDA, this.ZeroPage, 3 ),new Instruction ( "LDX", this.LDX, this.ZeroPage, 3 ),new Instruction ( "???", this.Unknown, this.Implied, 3 ),new Instruction ( "TAY", this.TAY, this.Implied, 2 ),new Instruction ( "LDA", this.LDA, this.Immediate, 2 ),new Instruction ( "TAX", this.TAX, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "LDY", this.LDY, this.Absolute, 4 ),new Instruction ( "LDA", this.LDA, this.Absolute, 4 ),new Instruction ( "LDX", this.LDX, this.Absolute, 4 ),new Instruction ( "???", this.Unknown, this.Implied, 4 ),
        new Instruction ( "BCS", this.BCS, this.Relative, 2 ),new Instruction ( "LDA", this.LDA, this.IndirectY, 5 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 5 ),new Instruction ( "LDY", this.LDY, this.ZeroPageX, 4 ),new Instruction ( "LDA", this.LDA, this.ZeroPageX, 4 ),new Instruction ( "LDX", this.LDX, this.ZeroPageY, 4 ),new Instruction ( "???", this.Unknown, this.Implied, 4 ),new Instruction ( "CLV", this.CLV, this.Implied, 2 ),new Instruction ( "LDA", this.LDA, this.AbsoluteY, 4 ),new Instruction ( "TSX", this.TSX, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 4 ),new Instruction ( "LDY", this.LDY, this.AbsoluteX, 4 ),new Instruction ( "LDA", this.LDA, this.AbsoluteX, 4 ),new Instruction ( "LDX", this.LDX, this.AbsoluteY, 4 ),new Instruction ( "???", this.Unknown, this.Implied, 4 ),
        new Instruction ( "CPY", this.CPY, this.Immediate, 2 ),new Instruction ( "CMP", this.CMP, this.IndirectX, 6 ),new Instruction ( "???", this.NOP, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 8 ),new Instruction ( "CPY", this.CPY, this.ZeroPage, 3 ),new Instruction ( "CMP", this.CMP, this.ZeroPage, 3 ),new Instruction ( "DEC", this.DEC, this.ZeroPage, 5 ),new Instruction ( "???", this.Unknown, this.Implied, 5 ),new Instruction ( "INY", this.INY, this.Implied, 2 ),new Instruction ( "CMP", this.CMP, this.Immediate, 2 ),new Instruction ( "DEX", this.DEX, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "CPY", this.CPY, this.Absolute, 4 ),new Instruction ( "CMP", this.CMP, this.Absolute, 4 ),new Instruction ( "DEC", this.DEC, this.Absolute, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 6 ),
        new Instruction ( "BNE", this.BNE, this.Relative, 2 ),new Instruction ( "CMP", this.CMP, this.IndirectY, 5 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 8 ),new Instruction ( "???", this.NOP, this.Implied, 4 ),new Instruction ( "CMP", this.CMP, this.ZeroPageX, 4 ),new Instruction ( "DEC", this.DEC, this.ZeroPageX, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 6 ),new Instruction ( "CLD", this.CLD, this.Implied, 2 ),new Instruction ( "CMP", this.CMP, this.AbsoluteY, 4 ),new Instruction ( "NOP", this.NOP, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 7 ),new Instruction ( "???", this.NOP, this.Implied, 4 ),new Instruction ( "CMP", this.CMP, this.AbsoluteX, 4 ),new Instruction ( "DEC", this.DEC, this.AbsoluteX, 7 ),new Instruction ( "???", this.Unknown, this.Implied, 7 ),
        new Instruction ( "CPX", this.CPX, this.Immediate, 2 ),new Instruction ( "SBC", this.SBC, this.IndirectX, 6 ),new Instruction ( "???", this.NOP, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 8 ),new Instruction ( "CPX", this.CPX, this.ZeroPage, 3 ),new Instruction ( "SBC", this.SBC, this.ZeroPage, 3 ),new Instruction ( "INC", this.INC, this.ZeroPage, 5 ),new Instruction ( "???", this.Unknown, this.Implied, 5 ),new Instruction ( "INX", this.INX, this.Implied, 2 ),new Instruction ( "SBC", this.SBC, this.Immediate, 2 ),new Instruction ( "NOP", this.NOP, this.Implied, 2 ),new Instruction ( "???", this.SBC, this.Implied, 2 ),new Instruction ( "CPX", this.CPX, this.Absolute, 4 ),new Instruction ( "SBC", this.SBC, this.Absolute, 4 ),new Instruction ( "INC", this.INC, this.Absolute, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 6 ),
        new Instruction ( "BEQ", this.BEQ, this.Relative, 2 ),new Instruction ( "SBC", this.SBC, this.IndirectY, 5 ),new Instruction ( "???", this.Unknown, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 8 ),new Instruction ( "???", this.NOP, this.Implied, 4 ),new Instruction ( "SBC", this.SBC, this.ZeroPageX, 4 ),new Instruction ( "INC", this.INC, this.ZeroPageX, 6 ),new Instruction ( "???", this.Unknown, this.Implied, 6 ),new Instruction ( "SED", this.SED, this.Implied, 2 ),new Instruction ( "SBC", this.SBC, this.AbsoluteY, 4 ),new Instruction ( "NOP", this.NOP, this.Implied, 2 ),new Instruction ( "???", this.Unknown, this.Implied, 7 ),new Instruction ( "???", this.NOP, this.Implied, 4 ),new Instruction ( "SBC", this.SBC, this.AbsoluteX, 4 ),new Instruction ( "INC", this.INC, this.AbsoluteX, 7 ),new Instruction ( "???", this.Unknown, this.Implied, 7 ),
    ];


    constructor() {

        this.Registers =  {
            A: 0,
            X: 0,
            Y: 0,
            SP: 0,
            PC: 0,
            Status: 0,
        };

    }
    
    private GetFlag(f: Flags6502): number {
        return (this.Registers.Status & f) > 0 ? 1 : 0;
    }

    private SetFlag(f: Flags6502, value: boolean): void {
        if (value) {
            this.Registers.Status |= f;
        } else {
            this.Registers.Status &= ~f;
        }
    }    

    private ReadWord(address: number): number {
        const lo = this.Read(address);
        const hi = this.Read(address, 1);
        return (hi << 8) | lo;
    }

    private Read(address: number, offset: number = 0): number {
        return this._bus?.CpuRead(address + offset, false) || 0;
    }

    private Write(address: number, value: number, offset: number = 0): void {
        this._bus?.CpuWrite(address + offset, value);
    }

    private Fetch(): number
    {
        if (this._lookup[this._opcode].AddressMode != this.Implied)
            this._fetched = this.Read(this._addr_abs);

        return this._fetched;
    }    

    // Address Mode: Implied
    // There is no additional data required for this instruction. The instruction
    // does something very simple like like sets a status bit. However, we will
    // target the accumulator, for instructions like PHA    
    private Implied(): number {
        this._fetched = this.Registers.A;
        return 0;
    }

    // Address Mode: Immediate
    // The instruction expects the next byte to be used as a value, so we'll prep
    // the read address to point to the next byte
    private Immediate(): number {
        this._addr_abs = this.Registers.PC++;
        return 0;
    }

    // Address Mode: Zero Page
    // To save program bytes, zero page addressing allows you to absolutely address
    // a location in first 0xFF bytes of address range. Clearly this only requires
    // one byte instead of the usual two.
    private ZeroPage(): number {
        this._addr_abs = this.Read(this.Registers.PC);
        this.Registers.PC++;

        this._addr_abs &= 0x00FF;
        return 0;
    }

    // Address Mode: Zero Page with X Offset
    // Fundamentally the same as Zero Page addressing, but the contents of the X Register
    // is added to the supplied single byte address. This is useful for iterating through
    // ranges within the first page.
    private ZeroPageX(): number   {
        this._addr_abs = this.Read(this.Registers.PC) + this.Registers.X;
        this.Registers.PC++;
        this._addr_abs &= 0x00FF;
        return 0;
    }    

    // Address Mode: Zero Page with Y Offset
    // Same as above but uses Y Register for offset
    private ZeroPageY(): number  {
        this._addr_abs = this.Read(this.Registers.PC) + this.Registers.Y;
        this.Registers.PC++;
        this._addr_abs &= 0x00FF;
        return 0;
    }    

    // Address Mode: Relative
    // This address mode is exclusive to branch instructions. The address
    // must reside within -128 to +127 of the branch instruction, i.e.
    // you cant directly branch to any address in the addressable range.
    private Relative(): number  {
        this._addr_rel = this.Read(this.Registers.PC);
        this.Registers.PC++;
        if ((this._addr_rel & 0x80) != 0)
        {
            this._addr_rel |= 0xFF00;
        }

        return 0;
    }    

    // Address Mode: Absolute 
    // A full 16-bit address is loaded and used
    private Absolute(): number  {
        var lo = this.Read(this.Registers.PC);
        this.Registers.PC++;
        var hi = this.Read(this.Registers.PC);
        this.Registers.PC++;

        this._addr_abs = ((hi << 8) | lo);
        return 0;
    }    

    // Address Mode: Absolute with X Offset
    // Fundamentally the same as absolute addressing, but the contents of the X Register
    // is added to the supplied two byte address. If the resulting address changes
    // the page, an additional clock cycle is required
    private AbsoluteX(): number    {
        var lo = this.Read(this.Registers.PC);
        this.Registers.PC++;
        var hi = this.Read(this.Registers.PC);
        this.Registers.PC++;

        this._addr_abs = ((hi << 8) | lo);
        this._addr_abs += this.Registers.X;

        return (this._addr_abs & 0xFF00) != (hi << 8) ? 1 : 0;
    }

    // Address Mode: Absolute with Y Offset
    // Fundamentally the same as absolute addressing, but the contents of the Y Register
    // is added to the supplied two byte address. If the resulting address changes
    // the page, an additional clock cycle is required
    private AbsoluteY(): number {
        var lo = this.Read(this.Registers.PC);
        this.Registers.PC++;
        var hi = this.Read(this.Registers.PC);
        this.Registers.PC++;

        this._addr_abs = ((hi << 8) | lo);
        this._addr_abs += this.Registers.Y;

        return (this._addr_abs & 0xFF00) != (hi << 8) ? 1 : 0;
    }    

    // Note: The next 3 address modes use indirection (aka Pointers!)

    // Address Mode: Indirect
    // The supplied 16-bit address is read to get the actual 16-bit address. This is
    // instruction is unusual in that it has a bug in the hardware! To emulate its
    // function accurately, we also need to emulate this bug. If the low byte of the
    // supplied address is 0xFF, then to read the high byte of the actual address
    // we need to cross a page boundary. This doesnt actually work on the chip as 
    // designed, instead it wraps back around in the same page, yielding an 
    // invalid actual address
    private Indirect(): number {
        var lo = (this.Read(this.Registers.PC));
        this.Registers.PC++;
        var hi = (this.Read(this.Registers.PC));
        this.Registers.PC++;

        var ptr = ((hi << 8) | lo);

        if (lo == 0x0FF) // Simulate page boundary hardware bug
        {
            this._addr_abs = ((this.Read((ptr & 0xFF00)) << 8) | (this.Read((ptr + 0))));
        }
        else
        {
            this._addr_abs = ((this.Read((ptr + 1)) << 8) | (this.Read((ptr + 0))));
        }

        return 0x00; 

    }

    // Address Mode: Indirect X
    // The supplied 8-bit address is offset by X Register to index
    // a location in page 0x00. The actual 16-bit address is read 
    // from this location
    private IndirectX(): number
    {
        var t = this.Read(this.Registers.PC);
        this.Registers.PC++;

        var lo = this.Read(((t + this.Registers.X) & 0x00FF));
        var hi = this.Read(((t + this.Registers.X + 1) & 0x00FF));


        this._addr_abs = ((hi << 8) | lo);

        return 0x00;
    }

    // Address Mode: Indirect Y
    // The supplied 8-bit address indexes a location in page 0x00. From 
    // here the actual 16-bit address is read, and the contents of
    // Y Register is added to it to offset it. If the offset causes a
    // change in page then an additional clock cycle is required.
    private IndirectY(): number  {
        var t = this.Read(this.Registers.PC);
        this.Registers.PC++;

        var lo = this.Read((t & 0x00FF));
        var hi = this.Read(((t + 1) & 0x00FF));

        this._addr_abs = ((hi << 8) | lo);

        return (this._addr_abs & 0xff00) != (hi << 8) ? 1 : 0;
    }    

    // Opcodes ======================================================
    // There are 56 "legitimate" opcodes provided by the 6502 CPU. I
    // have not modelled "unofficial" opcodes. As each opcode is 
    // defined by 1 byte, there are potentially 256 possible codes.
    // Codes are not used in a "switch case" style on a processor,
    // instead they are repsonisble for switching individual parts of
    // CPU circuits on and off. The opcodes listed here are official, 
    // meaning that the functionality of the chip when provided with
    // these codes is as the developers intended it to be. Unofficial
    // codes will of course also influence the CPU circuitry in 
    // interesting ways, and can be exploited to gain additional
    // functionality!
    //
    // These functions return 0 normally, but some are capable of
    // requiring more clock cycles when executed under certain
    // conditions combined with certain addressing modes. If that is 
    // the case, they return 1.
    //
    // I have included detailed explanations of each function in 
    // the class implementation file. Note they are listed in
    // alphabetical order here for ease of finding.

    ///////////////////////////////////////////////////////////////////////////////
    // INSTRUCTION IMPLEMENTATIONS

    // Note: Ive started with the two most complicated instructions to emulate, which
    // ironically is addition and subtraction! Ive tried to include a detailed 
    // explanation as to why they are so complex, yet so fundamental. Im also NOT
    // going to do this through the explanation of 1 and 2's complement.

    // Instruction: Add with Carry In
    // Function:    A = A + M + C
    // Flags Out:   C, V, N, Z
    //
    // Explanation:
    // The purpose of this function is to add a value to the accumulator and a carry bit. If
    // the result is > 255 there is an overflow setting the carry bit. Ths allows you to
    // chain together ADC instructions to add numbers larger than 8-bits. This in itself is
    // simple, however the 6502 supports the concepts of Negativity/Positivity and Signed Overflow.
    //
    // 10000100 = 128 + 4 = 132 in normal circumstances, we know this as unsigned and it allows
    // us to represent numbers between 0 and 255 (given 8 bits). The 6502 can also interpret 
    // this word as something else if we assume those 8 bits represent the range -128 to +127,
    // i.e. it has become signed.
    //
    // Since 132 > 127, it effectively wraps around, through -128, to -124. This wraparound is
    // called overflow, and this is a useful to know as it indicates that the calculation has
    // gone outside the permissable range, and therefore no longer makes numeric sense.
    //
    // Note the implementation of ADD is the same in binary, this is just about how the numbers
    // are represented, so the word 10000100 can be both -124 and 132 depending upon the 
    // context the programming is using it in. We can prove this!
    //
    //  10000100 =  132  or  -124
    // +00010001 = + 17      + 17
    //  ========    ===       ===     See, both are valid additions, but our interpretation of
    //  10010101 =  149  or  -107     the context changes the value, not the hardware!
    //
    // In principle under the -128 to 127 range:
    // 10000000 = -128, 11111111 = -1, 00000000 = 0, 00000000 = +1, 01111111 = +127
    // therefore negative numbers have the most significant set, positive numbers do not
    //
    // To assist us, the 6502 can set the overflow flag, if the result of the addition has
    // wrapped around. V <- ~(A^M) & A^(A+M+C) :D lol, let's work out why!
    //
    // Let's suppose we have A = 30, M = 10 and C = 0
    //          A = 30 = 00011110
    //          M = 10 = 00001010+
    //     RESULT = 40 = 00101000
    //
    // Here we have not gone out of range. The resulting significant bit has not changed.
    // So let's make a truth table to understand when overflow has occurred. Here I take
    // the MSB of each component, where R is RESULT.
    //
    // A  M  R | V | A^R | A^M |~(A^M) | 
    // 0  0  0 | 0 |  0  |  0  |   1   |
    // 0  0  1 | 1 |  1  |  0  |   1   |
    // 0  1  0 | 0 |  0  |  1  |   0   |
    // 0  1  1 | 0 |  1  |  1  |   0   |  so V = ~(A^M) & (A^R)
    // 1  0  0 | 0 |  1  |  1  |   0   |
    // 1  0  1 | 0 |  0  |  1  |   0   |
    // 1  1  0 | 1 |  1  |  0  |   1   |
    // 1  1  1 | 0 |  0  |  0  |   1   |
    //
    // We can see how the above equation calculates V, based on A, M and R. V was chosen
    // based on the following hypothesis:
    //       Positive Number + Positive Number = Negative Result -> Overflow
    //       Negative Number + Negative Number = Positive Result -> Overflow
    //       Positive Number + Negative Number = Either Result -> Cannot Overflow
    //       Positive Number + Positive Number = Positive Result -> OK! No Overflow
    //       Negative Number + Negative Number = Negative Result -> OK! NO Overflow    
    private ADC(): number {
        // Grab the data that we are adding to the accumulator
        this.Fetch();

        // Add is performed in 16-bit domain for emulation to capture any
        // carry bit, which will exist in bit 8 of the 16-bit word
        this._temp = (this.Registers.A + this._fetched + this.GetFlag(Flags6502.C));

        // The carry flag out exists in the high byte bit 0
        this.SetFlag(Flags6502.C, this._temp > 255);

        // The Zero flag is set if the result is 0
        this.SetFlag(Flags6502.Z, (this._temp & 0X00FF) == 0);

        // The signed Overflow flag is set based on that up there!
        this.SetFlag(Flags6502.V, ((~(this.Registers.A ^ this._fetched) & (this.Registers.A ^ this._temp)) & 0x0080) != 0);

        // The negative flag is set to the most significate bit of the result
        this.SetFlag(Flags6502.N, (this._temp & 0x080) != 0);

        // Load the result into the accumulator 
        this.Registers.A = (this._temp & 0x00FF);

        // this instruction has the potential to require an additional clock cycle
        return 1;        
    }

    // Instruction: Bitwise Logic AND
    // Function:    A = A & M
    // Flags Out:   N, Z    
    private AND(): number {
        this.Fetch();

        this.Registers.A &= this._fetched;
        this.SetFlag(Flags6502.Z, (this.Registers.A == 0x00));
        this.SetFlag(Flags6502.N, (this.Registers.A & 0x80) != 0x00);

        return 1;        
    }

    // Instruction: Arithmetic Shift Left
    // Function:    A = C <- (A << 1) <- 0
    // Flags Out:   N, Z, C
    private ASL(): number {
        this.Fetch();

        this._temp = (this._fetched << 1);
        this.SetFlag(Flags6502.C, (this._temp & 0xFF00) > 0);

        this.SetFlag(Flags6502.Z, (this._temp == 0x00));
        this.SetFlag(Flags6502.N, (this._temp & 0x80) != 0x00);

        if (this._lookup[this._opcode].AddressMode == this.Implied)
        {
            this.Registers.A = (this._temp & 0x00FF);
        }
        else
        {
            this.Write(this._addr_abs, (this._temp & 0x00FF));
        }

        return 0;
    }

    // Instruction: Branch if Carry Clear
    // Function:    if(C == 0) pc = address
    private BCC() : number {
        if(this.GetFlag(Flags6502.C) != 0) return 0;

        this._cycles++;
        this._addr_abs = this.Registers.PC + this._addr_rel;

        if(this.CheckPageBoundary())
            this._cycles

        this.Registers.PC = this._addr_abs
        return 0;
    }

    // Instruction: Branch if Carry Set
    // Function:    if(C == 1) pc = address
    private BCS() : number {
        if(this.GetFlag(Flags6502.C) == 0) return 0;

        this._cycles++;
        this._addr_abs = this.Registers.PC + this._addr_rel;

        if(this.CheckPageBoundary())
            this._cycles

        this.Registers.PC = this._addr_abs
        return 0;
        
    }

    // Instruction: Branch if Equal
    // Function:    if(Z == 1) pc = address
    private BEQ() : number {

        if(this.GetFlag(Flags6502.Z) == 0) return 0;

        this._cycles++;
        this._addr_abs = this.Registers.PC + this._addr_rel;

        if(this.CheckPageBoundary())
            this._cycles

        this.Registers.PC = this._addr_abs
        return 0;

    }

    private BIT() : number {
        this.Fetch();
        this._temp = this.Registers.A & this._fetched;
        this.SetFlag(Flags6502.Z, (this._temp == 0x00));
        this.SetFlag(Flags6502.N, (this._fetched & (1 << 7)) != 0);
        this.SetFlag(Flags6502.V, (this._fetched & (1 << 6)) != 0);

        return 0;        
    }

    // Instruction: Branch if Negative
    // Function:    if(N == 1) pc = address
    private BMI() : number {

        if(this.GetFlag(Flags6502.N) == 0) return 0;

        this._cycles++;
        this._addr_abs = this.Registers.PC + this._addr_rel;

        if(this.CheckPageBoundary())
            this._cycles

        this.Registers.PC = this._addr_abs
        return 0;

    }

    // Instruction: Branch if Not Equal
    // Function:    if(Z == 0) pc = address
    private BNE() : number {
        if(this.GetFlag(Flags6502.Z) != 0) return 0;

        this._cycles++;
        this._addr_abs = this.Registers.PC + this._addr_rel;

        if(this.CheckPageBoundary())
            this._cycles

        this.Registers.PC = this._addr_abs
        return 0;
    }

    // Instruction: Branch if Positive
    // Function:    if(N == 0) pc = address
    private BPL() : number {
        if(this.GetFlag(Flags6502.N) != 0) return 0;

        this._cycles++;
        this._addr_abs = this.Registers.PC + this._addr_rel;

        if(this.CheckPageBoundary())
            this._cycles

        this.Registers.PC = this._addr_abs
        return 0;
    }

    // Instruction: Break
    // Function:    Program Sourced Interrupt
    private BRK(): number {
        this.Registers.PC++;

        this.SetFlag(Flags6502.I, true); 
        this.StackPush((this.Registers.PC >> 7) & 0x00FF);
        this.StackPush((this.Registers.PC & 0x00FF));

        this.SetFlag(Flags6502.B, true);
        this.StackPush(this.Registers.Status);

        this.SetFlag(Flags6502.B, false);

        this.Registers.PC = this.ReadWord(0xFFFE);
        return 0;
    }


    // Instruction: Branch if Overflow Clear
    // Function:    if(V == 0) pc = address
    private BVC() : number {
        if(this.GetFlag(Flags6502.V) != 0) return 0;

        this._cycles++;
        this._addr_abs = this.Registers.PC + this._addr_rel;

        if(this.CheckPageBoundary())
            this._cycles

        this.Registers.PC = this._addr_abs
        return 0;
    }

    // Instruction: Branch if Overflow Set
    // Function:    if(V == 1) pc = address
    private BVS() : number {
        if(this.GetFlag(Flags6502.V) == 0) return 0;

        this._cycles++;
        this._addr_abs = this.Registers.PC + this._addr_rel;

        if(this.CheckPageBoundary())
            this._cycles

        this.Registers.PC = this._addr_abs
        return 0;
    }

    // Instruction: Clear Carry Flag
    // Function:    C = 0
    private CLC(): number {
        this.SetFlag(Flags6502.C, false);
        return 0;
    }

    // Instruction: Clear Decimal Flag
    // Function:    D = 0
    private CLD(): number {
        this.SetFlag(Flags6502.D, false);
        return 0;
    }

    // Instruction: Disable Interrupts / Clear Interrupt Flag
    // Function:    I = 0
    private CLI(): number {
        this.SetFlag(Flags6502.I, false);
        return 0;
    }

    // Instruction: Clear Overflow Flag
    // Function:    V = 0
    private CLV(): number {
        this.SetFlag(Flags6502.V, false);
        return 0;
    }

    // Instruction: Compare Accumulator
    // Function:    C <- A >= M      Z <- (A - M) == 0
    // Flags Out:   N, C, Z
    private CMP(): number {
        this.Fetch();

        this._temp = (this.Registers.A - this._fetched);

        this.SetFlag(Flags6502.C, this.Registers.A >= this._fetched);
        this.SetFlag(Flags6502.Z, (this._temp & 0x00FF) == 0);
        this.SetFlag(Flags6502.N, (this._temp & 0x0080) != 0);
        return 1;
    }    
    
    // Instruction: Compare X Register
    // Function:    C <- X >= M      Z <- (X - M) == 0
    // Flags Out:   N, C, Z
    private CPX() : number   {
        this.Fetch();

        this._temp = (this.Registers.X - this._fetched);

        this.SetFlag(Flags6502.C, this.Registers.X >= this._fetched);
        this.SetFlag(Flags6502.Z, (this._temp & 0x00FF) == 0);
        this.SetFlag(Flags6502.N, (this._temp & 0x0080) != 0);
        return 0;
    }

    // Instruction: Compare Y Register
    // Function:    C <- Y >= M      Z <- (Y - M) == 0
    // Flags Out:   N, C, Z
    private CPY(): number   {
        this.Fetch();

        this._temp = (this.Registers.Y - this._fetched);

        this.SetFlag(Flags6502.C, this.Registers.Y >= this._fetched);
        this.SetFlag(Flags6502.Z, (this._temp & 0x00FF) == 0);
        this.SetFlag(Flags6502.N, (this._temp & 0x0080) != 0);
        return 0;
    }

    // Instruction: Decrement Value at Memory Location
    // Function:    M = M - 1
    // Flags Out:   N, Z
    private DEC() : number {
        this.Fetch();

        this._temp = (this._fetched - 1);
        this.Write(this._addr_abs, (this._temp & 0x00FF));
        this.SetFlag(Flags6502.Z, (this._temp & 0x00ff) == 0);
        this.SetFlag(Flags6502.N, (this._temp & 0x0080) != 0);

        return 0;
    }
    // Instruction: Decrement X Register
    // Function:    X = X - 1
    // Flags Out:   N, Z
    private DEX(): number  {
        this.Registers.X--;
        this.SetFlag(Flags6502.Z, this.Registers.X == 0);
        this.SetFlag(Flags6502.N, (this.Registers.X & 0x0080) != 0);
        return 0;
    }

    // Instruction: Decrement Y Register
    // Function:    Y = Y - 1
    // Flags Out:   N, Z
    private DEY(): number  {
        this.Registers.Y--;
        this.SetFlag(Flags6502.Z, this.Registers.Y == 0);
        this.SetFlag(Flags6502.N, (this.Registers.Y & 0x80) != 0);
        return 0;
    }

    // Instruction: Bitwise Logic XOR
    // Function:    A = A xor M
    // Flags Out:   N, Z
    private EOR(): number {
        this.Fetch();

        this.Registers.A ^= this._fetched;
        this.SetFlag(Flags6502.Z, this.Registers.A == 0);
        this.SetFlag(Flags6502.N, (this.Registers.A & 0x80) != 0);

        return 0;
    }

    // Instruction: Increment Value at Memory Location
    // Function:    M = M + 1
    // Flags Out:   N, Z
    private INC(): number
    {
        this.Fetch();

        this._temp = (this._fetched + 1);
        this.Write(this._addr_abs, (this._temp & 0x00FF));
        this.SetFlag(Flags6502.Z, (this._temp & 0x00FF) == 0);
        this.SetFlag(Flags6502.N, (this._temp & 0x0080) != 0);

        return 0;
    }

    // Instruction: Increment X Register
    // Function:    X = X + 1
    // Flags Out:   N, Z
    private INX(): number  {
        this.Registers.X++;
        this.SetFlag(Flags6502.Z, this.Registers.X == 0);
        this.SetFlag(Flags6502.N, (this.Registers.X & 0x0080) != 0);
        return 0;

    }
    // Instruction: Increment Y Register
    // Function:    Y = Y + 1
    // Flags Out:   N, Z
    private INY(): number {
        this.Registers.Y++;
        this.SetFlag(Flags6502.Z, this.Registers.Y == 0);
        this.SetFlag(Flags6502.N, (this.Registers.Y & 0x0080) != 0);
        return 0;
    }


    // Instruction: Jump To Sub-Routine
    // Function:    Push current pc to stack, pc = address
    private JSR(): number {
        this.Registers.PC++;

        this.StackPush(((this.Registers.PC >> 8) & 0x00FF) );
        this.StackPush((this.Registers.PC | 0x00FF));

        this.Registers.PC = this._addr_abs;

        return 0;
    }

    // Instruction: Jump To Location
    // Function:    pc = address
    private JMP(): number  {
        this.Registers.PC = this._addr_abs;
        return 0;
    }

    // Instruction: Load The Accumulator
    // Function:    A = M
    // Flags Out:   N, Z
    private LDA(): number  {
        this.Fetch();

        this.Registers.A = this._fetched;
        this.SetFlag(Flags6502.Z, this.Registers.A == 0x00);
        this.SetFlag(Flags6502.N, (this.Registers.A & 0x80) == 1);

        return 1;
    }

    // Instruction: Load The X Register
    // Function:    X = M
    // Flags Out:   N, Z
    private LDX(): number  {
        this.Fetch();

        this.Registers.X = this._fetched;
        this.SetFlag(Flags6502.Z, this.Registers.X == 0x00);
        this.SetFlag(Flags6502.N, (this.Registers.X & 0x80) == 1);

        return 1;
    }

    // Instruction: Load The Y Register
    // Function:    Y = M
    // Flags Out:   N, Z
    private LDY(): number {
        this.Fetch();

        this.Registers.Y = this._fetched;
        this.SetFlag(Flags6502.Z, this.Registers.Y == 0x00);
        this.SetFlag(Flags6502.N, (this.Registers.Y & 0x80) == 1);

        return 1;
    }

    private LSR(): number {
        this.Fetch();
        this.SetFlag(Flags6502.C, (this._fetched & 0x0001) == 1);
        this._temp = (this._fetched >> 1);
        this.SetFlag(Flags6502.Z, (this._temp & 0x00FF) == 0);
        this.SetFlag(Flags6502.N, (this._temp & 0x0080) != 0);

        if (this._lookup[this._opcode].AddressMode == this.Implied)
        {
            this.Registers.A = (this._temp & 0x00ff);
        }
        else
        {
            this.Write(this._addr_abs, (this._temp & 0x00ff));
        }

        return 0;
    }    

    private NOP(): number  {
        // Sadly not all NOPs are equal, Ive added a few here
        // based on https://wiki.nesdev.com/w/index.php/CPU_unofficial_opcodes
        // and will add more based on game compatibility, and ultimately
        // I'd like to cover all illegal opcodes too
        switch (this._opcode)
        {
            case 0x1C:
            case 0x3C:
            case 0x5C:
            case 0x7C:
            case 0xDC:
            case 0xFC:
                return 1;
                break;
        }
        return 0;
    }

    // Instruction: Bitwise Logic OR
    // Function:    A = A | M
    // Flags Out:   N, Z
    private ORA(): number {
        this.Fetch();

        this.Registers.A |= this._fetched;
        this.SetFlag(Flags6502.Z, this.Registers.A == 0x00);
        this.SetFlag(Flags6502.N, (this.Registers.A & 0x80) == 1);

        return 1;
    }

    // Instruction: Push Accumulator to Stack
    // Function:    A -> stack
    private PHA(): number  {
        this.StackPush(this.Registers.A);

        return 0;
    }

    // Instruction: Push Status Register to Stack
    // Function:    status -> stack
    // Note:        Break flag is set to 1 before push
    private PHP(): number  {
        this.StackPush((this.Registers.Status | Flags6502.B | Flags6502.U));
        this.SetFlag(Flags6502.B, false);
        this.SetFlag(Flags6502.U, false);

        return 0;
    }

    // Instruction: Pop Accumulator off Stack
    // Function:    A <- stack
    // Flags Out:   N, Z
    private PLA(): number  {
        this.Registers.A = this.StackPop();

        this.SetFlag(Flags6502.Z, this.Registers.A == 0x00);
        this.SetFlag(Flags6502.N, (this.Registers.A & 0x80) == 1);
        return 0;
    }

    // Instruction: Pop Status Register off Stack
    // Function:    Status <- stack
    private PLP(): number  {
    
        this.Registers.Status = this.StackPop();
        this.SetFlag(Flags6502.U, true);
        return 0;
    }

    private ROL(): number  {
    
        this.Fetch();

        this._temp = ((this._fetched << 1) | this.GetFlag(Flags6502.C));

        this.SetFlag(Flags6502.C, (this._temp & 0xFF00) != 0);
        this.SetFlag(Flags6502.Z, (this._temp & 0x00FF) == 0);
        this.SetFlag(Flags6502.N, (this._temp & 0x0080) != 0);
        if (this._lookup[this._opcode].AddressMode == this.Implied)
        {
            this.Registers.A = (this._temp & 0x00ff);
        }
        else
        {
            this.Write(this._addr_abs, (this._temp & 0x00ff));
        }

        return 0;
    }

    private ROR(): number  {
    
        this.Fetch();

        this._temp = ((this.GetFlag(Flags6502.C) << 7) | (this._fetched >> 1));

        this.SetFlag(Flags6502.C, (this._temp & 0xFF00) != 0);
        this.SetFlag(Flags6502.Z, (this._temp & 0x00FF) == 0);
        this.SetFlag(Flags6502.N, (this._temp & 0x0080) != 0);
        if (this._lookup[this._opcode].AddressMode == this.Implied)
        {
            this.Registers.A = (this._temp & 0x00ff);
        }
        else
        {
            this.Write(this._addr_abs, (this._temp & 0x00ff));
        }

        return 0;
    }

    private RTI(): number  {
    
        this.Registers.Status = this.StackPop();
        this.Registers.Status &= ~Flags6502.B;
        this.Registers.Status &= ~Flags6502.U;

        this.Registers.PC = this.StackPop();
        this.Registers.PC |= (this.StackPop() << 8);

        this.Registers.PC++;
        return 0;
    }

    private RTS(): number  {
    
        this.Registers.PC = this.StackPop();
        this.Registers.PC |= (this.StackPop() << 8);

        this.Registers.PC++;
        return 0;
    }

    // Instruction: Subtraction with Borrow In
    // Function:    A = A - M - (1 - C)
    // Flags Out:   C, V, N, Z
    //
    // Explanation:
    // Given the explanation for ADC above, we can reorganise our data
    // to use the same computation for addition, for subtraction by multiplying
    // the data by -1, i.e. make it negative
    //
    // A = A - M - (1 - C)  ->  A = A + -1 * (M - (1 - C))  ->  A = A + (-M + 1 + C)
    //
    // To make a signed positive number negative, we can invert the bits and add 1
    // (OK, I lied, a little bit of 1 and 2s complement :P)
    //
    //  5 = 00000101
    // -5 = 11111010 + 00000001 = 11111011 (or 251 in our 0 to 255 range)
    //
    // The range is actually unimportant, because if I take the value 15, and add 251
    // to it, given we wrap around at 256, the result is 10, so it has effectively 
    // subtracted 5, which was the original intention. (15 + 251) % 256 = 10
    //
    // Note that the equation above used (1-C), but this got converted to + 1 + C.
    // This means we already have the +1, so all we need to do is invert the bits
    // of M, the data(!) therfore we can simply add, exactly the same way we did 
    // before.
    private SBC() : number {
    
        this.Fetch();

        var value = ((this._fetched) ^ 0x00ff);

        this._temp = (this.Registers.A + value + this.GetFlag(Flags6502.C));

        this.SetFlag(Flags6502.C, (this._temp & 0xFF00) != 0);
        this.SetFlag(Flags6502.Z, ((this._temp & 0x00FF) == 0));
        this.SetFlag(Flags6502.V, ((this._temp ^ this.Registers.A) & (this._temp ^ value) & 0x0080) != 0);
        this.SetFlag(Flags6502.N, (this._temp & 0x0080) != 0);

        this.Registers.A = (this._temp & 0x00ff);
        return 1;
    }

    // Instruction: Set Carry Flag
    // Function:    C = 1
    private SEC() : number {
    
        this.SetFlag(Flags6502.C, true);
        return 0;
    }

    // Instruction: Set Decimal Flag
    // Function:    D = 1
    private SED() : number {
    
        this.SetFlag(Flags6502.D, true);
        return 0;
    }

    // Instruction: Set Interrupt Flag / Enable Interrupts
    // Function:    I = 1
    private SEI() : number {
    
        this.SetFlag(Flags6502.I, true);
        return 0;
    }

    // Instruction: Store Accumulator at Address
    // Function:    M = A
    private STA() : number {
    
        this.Write(this._addr_abs, this.Registers.A);
        return 0;
    }

    // Instruction: Store X Register at Address
    // Function:    M = X
    private STX() : number {
    
        this.Write(this._addr_abs, this.Registers.X);
        return 0;
    }

    // Instruction: Store Y Register at Address
    // Function:    M = Y
    private STY() : number {
    
        this.Write(this._addr_abs, this.Registers.Y);
        return 0;
    }

    // Instruction: Transfer Accumulator to X Register
    // Function:    X = A
    // Flags Out:   N, Z
    private TAX() : number {
    
        this.Registers.X = this.Registers.A;

        this.SetFlag(Flags6502.Z, this.Registers.X == 0x00);
        this.SetFlag(Flags6502.N, (this.Registers.X & 0x80) == 1);

        return 0;
    }

    // Instruction: Transfer Accumulator to Y Register
    // Function:    Y = A
    // Flags Out:   N, Z
    private TAY() : number {
    
        this.Registers.Y = this.Registers.A;

        this.SetFlag(Flags6502.Z, this.Registers.Y == 0x00);
        this.SetFlag(Flags6502.N, (this.Registers.Y & 0x80) == 1);

        return 0;
    }

    // Instruction: Transfer Stack Pointer to X Register
    // Function:    X = stack pointer
    // Flags Out:   N, Z
    private TSX() : number {
    
        this.Registers.X = this.Registers.SP;
        this.SetFlag(Flags6502.Z, this.Registers.X == 0x00);
        this.SetFlag(Flags6502.N, (this.Registers.X & 0x80) == 1);

        return 0;
    }

    // Instruction: Transfer X Register to Accumulator
    // Function:    A = X
    // Flags Out:   N, Z
    private TXA() : number {
    
        this.Registers.A = this.Registers.X;
        this.SetFlag(Flags6502.Z, this.Registers.A == 0x00);
        this.SetFlag(Flags6502.N, (this.Registers.A & 0x80) == 1);

        return 0;
    }

    // Instruction: Transfer X Register to Stack Pointer
    // Function:    stack pointer = X
    private TXS() : number {
    
        this.Registers.SP = this.Registers.X;
        return 0;
    }

    // Instruction: Transfer Y Register to Accumulator
    // Function:    A = Y
    // Flags Out:   N, Z
    private TYA() : number {
    
        this.Registers.A = this.Registers.Y;

        this.SetFlag(Flags6502.Z, this.Registers.A == 0x00);
        this.SetFlag(Flags6502.N, (this.Registers.A & 0x80) == 1);

        return 0;
    }


    private Unknown() : number {
        return this.NOP();
    }
 
    
    private CheckPageBoundary(): boolean  {
        return ((this._addr_abs & 0xFF00) != (this.Registers.PC & 0xFF00));
    }    

    private StackPop(): number {
        this.Registers.SP++;
        return this.Read(STACKBASE + this.Registers.SP);
    }

    private StackPush(data: number) : void   {
        this.Write((STACKBASE + this.Registers.SP), data);
        this.Registers.SP--;
    }    
}