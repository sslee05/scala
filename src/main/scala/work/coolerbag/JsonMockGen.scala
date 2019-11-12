package work.coolerbag

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.io.Source
import spray.json._
import DefaultJsonProtocol._
import work.coolerbag.data.{BagCode, CoolerBagCollected, CoolerBagCollectedCommand, Coolerbag, CoolerbagData, CustomerAddr}

object JsonMockGen {

  val bagCodeFormat = "%05d"

  def main(args: Array[String]): Unit = {

    println(args)
    var fileName = "/Users/sslee05/work/coolerbag/source.txt"
    if(!args.isEmpty)
     fileName = args(0)

    //(Source fromFile fileName).getLines().foldLeft(StringBuilder.newBuilder)((b,s) => b append s)

    implicit val customerFormat = jsonFormat3(CustomerAddr)

    implicit val bagCodeFormat = jsonFormat2(BagCode)
    implicit val coolerbagFormat = jsonFormat4(Coolerbag)
    implicit val coolerbagDataFormat = jsonFormat1(CoolerbagData)
    implicit val coolerBagCollectedFormat = jsonFormat4(CoolerBagCollected)
    implicit val coolerBagCollectedCommand = jsonFormat8(CoolerBagCollectedCommand)

    val rs = (Source fromFile fileName).getLines().foldLeft(List.empty[CustomerAddr])((xs,s) => s.parseJson.convertTo[CustomerAddr] :: xs )

    val coolerBags = rs.zipWithIndex.map { it =>
      Coolerbag(it._1.memberSrl, it._1.addr1, it._1.addr2, genBagCode(it._2 * 3))
    }

    coolerBags.foreach(println)

    val outputJson = CoolerbagData(coolerBags).toJson.toString()
    println(outputJson)

    Files.write(Paths.get("/Users/sslee05/work/coolerbag/gen-mockdata.json"),outputJson.getBytes(StandardCharsets.UTF_8))


    val collectedDatas = coolerBags.map{it =>
      CoolerBagCollectedCommand(
        it.memberSrl.toLong,
        -1L,
        it.addr1,
        it.addr2,
        it.bagCodes.size,
        0,
        it.bagCodes.map(b => CoolerBagCollected(b.code, "COLLECTED", "SCAN",false)),
        "1573527420"
      )
    }

    val collectedJson = collectedDatas.toJson.toString()

    println("###################")
    println(collectedDatas)
    println(collectedJson)

    Files.write(Paths.get("/Users/sslee05/work/coolerbag/collected.json"),collectedJson.getBytes(StandardCharsets.UTF_8))

  }

  def genBagCode(start: Int): List[BagCode] =
    Range(start,start+3).map(it => BagCode(s"C1-${bagCodeFormat.format(it)}", "ORDERED")).toList

}
