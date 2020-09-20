import scala.collection.mutable.ArrayBuffer

class VirtualMachine(memory: Array[Int]) extends Loggable {

  import VirtualMachine._

  require(memory.size == MEMORY_MAX_SIZE)

  // 10 registers - 8 general registers with program counter + condition registers
  private val _registers: ArrayBuffer[Int] = ArrayBuffer.fill(10)(0)

  // VM memory
  private val _memory: Array[Int] = memory

  def debugRegisters(): Unit = {
    debug(s"VM registers: [${_registers.mkString(" ")}]")
  }

  def readRegister(location: Int): Int = {
    val actualLocation = location & 0xffff

    if (actualLocation >= Registers.R_COUNT.id || actualLocation < 0) {
      outLn(s"invalid register read location: $actualLocation")
      System.exit(1)
    }

    _registers(actualLocation)
  }

  def readMemory(location: Int): Int = {
    val actualLocation = location & 0xffff

    // bound check
    if (actualLocation >= MEMORY_MAX_SIZE || actualLocation < 0) {
      outLn(s"invalid memory read location: $actualLocation")
      System.exit(1)
    }

    // check if KBSR
    if (actualLocation == MR_KBSR) {
      if (System.in.available > 0) {
        _memory(MR_KBSR) = (1 << 15);
        _memory(MR_KBDR) = System.in.read.toInt & 0xffff
      } else {
        _memory(MR_KBSR) = 0
      }
    }

    _memory(actualLocation)
  }

  def writeRegister(location: Int, value: Int): Unit = {
    val actualLocation = location & 0xffff

    if (actualLocation >= Registers.R_COUNT.id || actualLocation < 0) {
      outLn(s"invalid register write location: $actualLocation")
      System.exit(1)
    }

    // make sure always writing 16-bit values to registers
    _registers(actualLocation) = value & 0xffff
  }

  def writeMemory(location: Int, value: Int): Unit = {
    val actualLocation = location & 0xffff

    if (actualLocation >= MEMORY_MAX_SIZE || actualLocation < 0) {
      outLn(s"invalid memory write location: $actualLocation")
      System.exit(1)
    }

    // make sure always writing 16-bit values to registers
    _memory(actualLocation) = value & 0xffff
  }

  def updateConditionFlags(registerNewValueIdx: Int): Unit = {
    _registers(registerNewValueIdx) match {
      case 0 =>
        _registers(Registers.R_COND.id) = (1 << 1) // zero case
      case _ if ((_registers(registerNewValueIdx) >> 0xf) == 1) =>
        _registers(Registers.R_COND.id) = (1 << 2) // negative case
      case _ =>
        _registers(Registers.R_COND.id) = (1 << 0) // positive case
    }
  }

  def trapPuts(location: Int): Unit = {
    if (memory(location) != 0) {
      print(memory(location).toChar)
      trapPuts(location + 1)
    }
  }

  // 2 characters per memory location
  def trapPutsSp(location: Int): Unit = {
    if (memory(location) != 0) {
      val firstChar = (memory(location) & 0xff).toChar
      print(firstChar)

      // double check if secondChar exists - if ASCII string to output has odd
      // # characters, secondChar at final memory location would be 0
      if ((memory(location) >> 0x8) != 0) {
        val secondChar = ((memory(location) >> 0x8) & 0xff).toChar
        print(secondChar)
      }

      trapPuts(location + 1)
    }
  }

}

object VirtualMachine {

  // 2^16 addressable memory space, each storing 16-bit value
  val MEMORY_MAX_SIZE: Int = Math.pow(2, 16).toInt

  object Registers extends Enumeration {
    val R_R0, R_R1, R_R2, R_R3, R_R4, R_R5, R_R6, R_R7, R_PC, R_COND,
        R_COUNT // not actually a register
    = Value
  }

  val MR_KBSR = 0xfe00 // keyboard status
  val MR_KBDR = 0xfe02 // keyboard data

  object InstructionSet extends Enumeration {
    val OP_BR, /* 0 - branch */
    OP_ADD, /* 1 - add  */
    OP_LD, /* 2 - load */
    OP_ST, /* 3 - store */
    OP_JSR, /* 4 - jump register */
    OP_AND, /* 5 - bitwise and */
    OP_LDR, /* 6 - load register */
    OP_STR, /* 7 - store register */
    OP_RTI, /* 8 - unused */
    OP_NOT, /* 9 - bitwise not */
    OP_LDI, /* 10 - load indirect */
    OP_STI, /* 11 - store indirect */
    OP_JMP, /* 12 - jump */
    OP_RES, /* 13 - reserved (unused) */
    OP_LEA, /* 14 - load effective address */
    OP_TRAP /* 15 - execute trap */
    = Value
  }

  implicit def registerToId(v: VirtualMachine.Registers.Value) = v.id

  val TRAP_GETC =
    0x20 /* get character from keyboard, not echoed onto the terminal */
  val TRAP_OUT = 0x21 /* output a character */
  val TRAP_PUTS = 0x22 /* output a word string */
  val TRAP_IN = 0x23 /* get character from keyboard, echoed onto the terminal */
  val TRAP_PUTSP = 0x24 /* output a byte string */
  val TRAP_HALT = 0x25 /* halt the program */

}
