package work.coolerbag

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.io.Source
import spray.json._
import DefaultJsonProtocol._
import work.coolerbag.data.{BagCode, Coolerbag, CoolerbagData, CustomerAddr}

object JsonMockGen {

  def main(args: Array[String]): Unit = {

    println(args)
    var fileName = "/Users/sslee05/work/coolerbag/test.txt"
    if(!args.isEmpty)
     fileName = args(0)

    //(Source fromFile fileName).getLines().foldLeft(StringBuilder.newBuilder)((b,s) => b append s)

    implicit val customerFormat = jsonFormat3(CustomerAddr)

    implicit val bagCodeFormat = jsonFormat2(BagCode)
    implicit val coolerbagFormat = jsonFormat4(Coolerbag)
    implicit val coolerbagDataFormat = jsonFormat1(CoolerbagData)

    val rs = (Source fromFile fileName).getLines().foldLeft(List.empty[CustomerAddr])((xs,s) => s.parseJson.convertTo[CustomerAddr] :: xs )
    val coolerBags = rs.zipWithIndex.map { it =>
      Coolerbag(it._1.memberSrl, it._1.addr1, it._1.addr2, genBagCode(it._2))
    }

    coolerBags.foreach(println)

    val outputJson = CoolerbagData(coolerBags).toJson.toString()
    println(outputJson)

    Files.write(Paths.get("/Users/sslee05/work/coolerbag/gen-mockdata.json"),outputJson.getBytes(StandardCharsets.UTF_8))

  }

  def genBagCode(preIdx: Int): List[BagCode] =
    Range(0,3).map(it => BagCode(s"C${preIdx}000${it}", "ORDERED")).toList

}
