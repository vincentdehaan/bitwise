package nl.vindh.bitwise

import org.scalacheck.{Arbitrary, Gen}

trait ArbitraryBits extends BitVarXs {
  implicit def arbitraryBit: Arbitrary[Bit] = Arbitrary {
    val primitiveGen = Gen.oneOf(ZERO, ONE)
    val varGen = Gen.oneOf(x1, x2, x3, x4, x5, x6, x7, x8)
    def notGen(level: Int): Gen[Bit] = for{
      bit <- bitGen(level - 1)
    } yield (! bit)

    def binGen(level: Int): Gen[Bit] = for {
      left <- bitGen(level - 1)
      right <- bitGen(level - 1)
      op <- Gen.oneOf(
        (_:Bit) & (_:Bit),
        (_:Bit) | (_:Bit),
        (_:Bit) <-> (_:Bit),
        (_:Bit) ^ (_:Bit)
      )
    } yield op(left, right)

    def bitGen(level: Int): Gen[Bit] = for {
      bit <- Gen.lzy(
        Gen.frequency(
          ((5 - level).max(0), primitiveGen),
          ((10 - level).max(0), varGen),
          (level, notGen(level)),
          (level, binGen(level))
        )
      )
    } yield bit

    bitGen(10)
  }

}
