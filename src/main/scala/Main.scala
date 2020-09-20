import java.nio.file.Files
import java.nio.file.Paths
import java.io.{FileInputStream, BufferedInputStream}
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object Main extends App with Loggable {

  import NumberManipulation._
  import VirtualMachine.Registers._
  import VirtualMachine.InstructionSet._
  import VirtualMachine._

  // check for provided file paths
  if (args.length == 0) {
    outLn("usage: run [image-file1] ...\n")
    System.exit(1)
  }

  val byteArray: Array[Byte] = Files.readAllBytes(Paths.get(args(0)))

  // Read origin
  // The first 16 bits of the program file specify the address in memory where the program should start. This address is called the origin.
  // Combining 2 8-bit signed integers to get non-negative decimal index to start writing memory to.
  val origin: Int = (byteArray(0), byteArray(1))

  // get file data after origin
  val romData: Array[Byte] = byteArray.slice(2, byteArray.length)

  // first group 8-bit SI together
  val groupedArr: Array[Array[Byte]] = romData.grouped(2).toArray

  // then loop through to transfer into 16-bit words
  val fileData: Array[Int] = groupedArr.map(ba => {
    (ba(0), ba(1))
  })

  // then, construct VM memory
  val mem: Array[Int] =
    Array.fill(origin)(0) ++ fileData ++ Array.fill(
      VirtualMachine.MEMORY_MAX_SIZE - origin - fileData.size
    )(0)
  val VM = new VirtualMachine(mem)

  // set the PC to the memory origin
  VM.writeRegister(R_PC.id, origin)

  var running = true

  while (running) {

    // read instruction from program counter register
    val instruction = VM.readMemory(VM.readRegister(R_PC))

    // increment PC
    VM.writeRegister(R_PC.id, VM.readRegister(R_PC) + 1)

    // get the op code from first 4 bits - shift 16-bit 12 bits to the right
    val op = instruction >> 12

    op match {
      case _ if op == OP_ADD.id => {
        val dr = instruction.getDestinationRegister // 0x7 to get the 3 bits
        val sr1 = (instruction >> 0x6) & 0x7
        val mode = (instruction >> 0x5) & 0x1

        if (mode == 1) {
          // if bit 5 is 1, use immediate mode
          val imm5 = (instruction & 0x1f)
          VM.writeRegister(dr, VM.readRegister(sr1) + imm5.signExtend(5))
        } else {
          // otherwise, get value from register location indicated by sr2
          val sr2 = instruction & 0x7
          VM.writeRegister(dr, VM.readRegister(sr1) + VM.readRegister(sr2))
        }

        // update condition flags
        VM.updateConditionFlags(dr)
      }
      case _ if op == OP_AND.id => {
        val dr = instruction.getDestinationRegister // 0x7 to get the 3 bits
        val sr1 = (instruction >> 0x6) & 0x7
        val mode = (instruction >> 0x5) & 0x1

        if (mode == 1) {
          // if bit 5 is 1, use immediate mode
          val imm5 = (instruction & 0x1f)
          VM.writeRegister(dr, VM.readRegister(sr1) & imm5.signExtend(5))
        } else {
          // otherwise, get value from register location indicated by sr2
          val sr2 = instruction & 0x7
          VM.writeRegister(dr, VM.readRegister(sr1) & VM.readRegister(sr2))
        }

        // update condition flags
        VM.updateConditionFlags(dr)
      }
      case _ if op == OP_BR.id => {
        val conditionCodes = (instruction >> 0x9) & 0x7
        val pcOffset9 = instruction & 0x1ff

        if ((conditionCodes & VM.readRegister(R_COND)) != 0) {
          VM.writeRegister(
            R_PC,
            VM.readRegister(R_PC) + pcOffset9.signExtend(9)
          )
        }
      }

      case _ if op == OP_JMP.id => {
        val baseRegister = (instruction >> 0x6) & 0x7
        VM.writeRegister(R_PC, VM.readRegister(baseRegister))
      }
      case _ if op == OP_JSR.id => {
        // save PC to R7
        VM.writeRegister(7, VM.readRegister(R_PC))

        val mode = (instruction >> 0xb) & 0x1
        val pcOffset11 = instruction & 0x7ff
        val baseRegister = (instruction >> 0x6) & 0x7

        if (mode == 1) {
          VM.writeRegister(
            R_PC,
            VM.readRegister(R_PC) + pcOffset11.signExtend(11)
          )
        } else {
          VM.writeRegister(R_PC, VM.readRegister(baseRegister))
        }
      }
      case _ if op == OP_LD.id => {
        val pcOffset9 = instruction & 0x1ff
        val dr = instruction.getDestinationRegister
        VM.writeRegister(
          dr,
          VM.readMemory(VM.readRegister(R_PC) + pcOffset9.signExtend(9))
        )

        // update condition flags
        VM.updateConditionFlags(dr)
      }
      case _ if op == OP_LDI.id => {
        val pcOffset9 = instruction & 0x1ff
        val dr = instruction.getDestinationRegister
        VM.writeRegister(
          dr,
          VM.readMemory(
            VM.readMemory(VM.readRegister(R_PC) + pcOffset9.signExtend(9))
          )
        )

        // update condition flags
        VM.updateConditionFlags(dr)
      }
      case _ if op == OP_LDR.id => {
        val offset6 = instruction & 0x3f
        val dr = instruction.getDestinationRegister
        val baseRegister = (instruction >> 0x6) & 0x7

        VM.writeRegister(
          dr,
          VM.readMemory(VM.readRegister(baseRegister) + offset6.signExtend(6))
        )

        // update condition flags
        VM.updateConditionFlags(dr)
      }
      case _ if op == OP_LEA.id => {
        val pcOffset9 = instruction & 0x1ff
        val dr = instruction.getDestinationRegister

        VM.writeRegister(dr, VM.readRegister(R_PC) + pcOffset9.signExtend(9))

        // update condition flags
        VM.updateConditionFlags(dr)
      }
      case _ if op == OP_NOT.id => {
        val dr = instruction.getDestinationRegister
        val sr = (instruction >> 0x6) & 0x7

        VM.writeRegister(dr, ~VM.readRegister(sr))

        // update condition flags
        VM.updateConditionFlags(dr)
      }
      case _ if op == OP_ST.id => {
        val pcOffset9 = instruction & 0x1ff
        val sr = (instruction >> 0x9) & 0x7

        VM.writeMemory(
          VM.readRegister(R_PC) + pcOffset9.signExtend(9),
          VM.readMemory(sr)
        )
      }
      case _ if op == OP_STI.id => {
        val pcOffset9 = instruction & 0x1ff
        val sr = (instruction >> 0x9) & 0x7

        VM.writeMemory(
          VM.readMemory(VM.readRegister(R_PC) + pcOffset9.signExtend(9)),
          VM.readMemory(sr)
        )
      }
      case _ if op == OP_STR.id => {
        val offset6 = instruction & 0x3f
        val baseRegister = (instruction >> 0x6) & 0x7
        val sr = (instruction >> 0x9) & 0x7

        VM.writeMemory(
          VM.readRegister(baseRegister) + offset6.signExtend(6),
          VM.readRegister(sr)
        )
      }
      case _ if op == OP_TRAP.id => {
        val trapvect8 = instruction & 0xff

        trapvect8 match {
          case 0x20 => {
            val input = StdIn.readChar
            VM.writeRegister(0, (input.toInt & 0xff)) // clear high 8 bits
          }
          case 0x21 => {
            out(VM.readRegister(0).toChar)
          }
          case 0x22 => {
            VM.trapPuts(VM.readRegister(0))
          }
          case 0x23 => {
            outLn("Enter a character:")
            val input = StdIn.readChar
            out(input) // echo
            VM.writeRegister(0, (input.toInt & 0xff)) // clear high 8 bits
          }
          case 0x24 => {
            VM.trapPutsSp(VM.readRegister(0))
          }
          case 0x25 => {
            outLn("HALTED")
            running = false
          }
          case _ => {
            outLn(s"Unrecognized trap routine")
            System.exit(1)
          }
        }
      }
      case op => {
        outLn(s"Unrecognized opcode: $op")
        System.exit(1)
      }
    }

  }

}
