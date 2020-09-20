object NumberManipulation {

  implicit def toWord16(byteTup2: Tuple2[Byte, Byte]): Int = {
    // make sure only get lowest 8 bits, and not the padded / signed 1's
    val firstByte = byteTup2._1 & 0xff
    val secondByte = byteTup2._2 & 0xff
    (firstByte << 8) + secondByte
  }

  implicit class IntExtraction(i: Int) {
    def getDestinationRegister() = (i >> 0x9) & 0x7
  }

  implicit class IntConversion(i: Int) {
    def signExtend(bitCount: Int): Int = {
      if (((i >> (bitCount - 1)) & 0x1) == 0x1) {
        ((0xffff << bitCount) | i) & 0xffff
      } else {
        i
      }
    }
  }

}
