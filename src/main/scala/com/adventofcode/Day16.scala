package com.adventofcode

import com.adventofcode.utils.FileUtils.getStrings

object Day16 extends App {
  object Packet {
    def apply(str: String): Packet = {
      val version = str.take(3)
      val typeID = str.slice(3, 6)
      val payload = str.drop(6)
      val res = typeID match {
        case "100" =>
          LiteralPacket(version, typeID, payload)
        case _ =>
          OperatorPacket(version, typeID, payload)
      }
      res
    }
  }

  trait Packet {
    val version: String
    val typeID: String
    val length: Int
    val value: BigInt

    def sumOfVersions: BigInt
  }

  object LiteralPacket {
    def apply(version: String, typeID: String, payload: String): LiteralPacket = {
      def decodeLiteralValue(str: String): List[String] = {
        val group = str.take(5)
        val value = group.tail
        group.head match {
          case '1' => value :: decodeLiteralValue(str.drop(5))
          case '0' => List(value)
        }
      }

      val values = decodeLiteralValue(payload)
      LiteralPacket(
        version,
        typeID,
        3 + 3 + values.size * 5,
        bitsToDec(values.mkString)
      )
    }
  }

  case class LiteralPacket(version: String, typeID: String, length: Int, value: BigInt) extends Packet {
    override def sumOfVersions: BigInt = bitsToDec(version)
  }

  object OperatorPacket {
    def apply(version: String, typeID: String, payload: String): OperatorPacket = {
      val lengthTypeID = payload.head
      val packets = payload.tail

      def readZeroMode(length: BigInt, payload: String): List[Packet] = {
        val packet = Packet(payload)
        val packetLength = packet.length
        if (packetLength == length) List(packet)
        else packet :: readZeroMode(length - packetLength, payload.drop(packetLength))
      }

      def readOneMode(n: BigInt, payload: String): List[Packet] = {
        if (n == 0) List()
        else {
          val packet = Packet(payload)
          val packetLength = packet.length
          packet :: readOneMode(n - 1, payload.drop(packetLength))
        }
      }

      lengthTypeID match {
        case '0' =>
          val length = packets.take(15)
          val packetsPayload = packets.drop(15)
          val inner = readZeroMode(bitsToDec(length), packetsPayload)
          OperatorPacket(version, typeID, 3 + 3 + 1 + 15 + inner.map(_.length).sum, inner)
        case '1' =>
          val countStr = packets.take(11)
          val packetsPayload = packets.drop(11)
          val inner = readOneMode(bitsToDec(countStr), packetsPayload)
          OperatorPacket(version, typeID, 3 + 3 + 1 + 11 + inner.map(_.length).sum, inner)
      }
    }
  }

  case class OperatorPacket(version: String, typeID: String, length: Int, packets: List[Packet]) extends Packet {
    override def sumOfVersions: BigInt = bitsToDec(version) + packets.map(_.sumOfVersions).sum

    private def packetValues = packets.map(_.value)

    private def compare(op: (BigInt, BigInt) => Boolean): BigInt =
      if (op(packets.head.value, packets(1).value)) 1 else 0

    override lazy val value: BigInt = typeID match {
      case "000" => packetValues.sum
      case "001" => packetValues.product
      case "010" => packetValues.min
      case "011" => packetValues.max
      case "101" => compare(Ordering[BigInt].gt)
      case "110" => compare(Ordering[BigInt].lt)
      case "111" => compare(Ordering[BigInt].equiv)
    }
  }

  val lines = getStrings("input16.txt")

  private def hexToBits(str: String): String =
    BigInt(str, 16).toString(2)

  private def bitsToDec(str: String) =
    BigInt(str, 2)

  private def hexToBitsWithLeadingZero(str: String): String = {
    val res = hexToBits(str)
    Option(res.length % 4)
      .filter(_ != 0)
      .map(4 - _)
      .map("0" * _)
      .getOrElse("") + res
  }

  lines
    .map(hexToBitsWithLeadingZero)
    .map(Packet.apply)
    .tapEach(p => println(p.sumOfVersions))
    .tapEach(p => println(p.value))
}
