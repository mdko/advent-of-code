import scala.io.Source

object Puzzle {
    def toNumber(s: String): Integer = {
        val n = s.map(c => c match {
            case 'F' | 'L' => '0'
            case 'B' | 'R' => '1'
        })
        Integer.parseInt(n, 2)
    }

    def part1(seatIDs: List[Integer]): Int = {
        seatIDs.max
    }

    def part2(seatIDs: List[Integer]): Int = {
        val s = seatIDs.sorted
        s.zipWithIndex.filter({case (seatId, ix) =>
          ix match {
              case 0 => false
              case _ => {
                  val pre = s(ix - 1)
                  pre == seatId - 2
              }
          }
        }).head._1 - 1
    }

    def main(args: Array[String]): Unit = {
        val source = Source.fromFile("input")
        val lineIterator = source.getLines()
        val seatIDs = lineIterator.map(toNumber).toList
        println(part1(seatIDs))
        println(part2(seatIDs))
        source.close
    }
}