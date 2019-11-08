import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}

import scala.io.Source

object InvoiceAdjust {

  def main(args: Array[String]): Unit = {

    val allInvoices = Source.fromFile(s"/Users/sting/work/data-file/${args(0)}")
      .getLines().foldRight[Map[String,String]](Map.empty[String,String])((a, xs) => xs + (a -> a))

    val flexInvoices = Source.fromFile(s"/Users/sting/work/data-file/${args(1)}")
      .getLines().foldRight[Map[String,String]](Map.empty[String,String])((a, xs) => xs + (a -> a))

    val resultXs = allInvoices.filterKeys(key => !flexInvoices.contains(key)).keySet.toList

    resultXs.foreach(println)

    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(s"/Users/sting/work/data-file/${args(2)}",true)))
    resultXs.foreach(a => {writer.newLine(); writer.write(a)})

    writer.close()


  }

}
