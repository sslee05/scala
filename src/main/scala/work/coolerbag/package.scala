package work.coolerbag

package object data {

  case class BagCode(code: String, status: String)
  case class Coolerbag(memberSrl: String, addr1: String, addr2: String, bagCodes: List[BagCode])
  case class CustomerAddr(memberSrl: String, addr1: String, addr2: String)
  case class CoolerbagData(coolerbags: List[Coolerbag])

  case class CoolerBagCollected(bagCode: String,status:String, scanMethod: String, isExtra: Boolean)
  case class CoolerBagCollectedCommand(
       memberSrl: Long,
       householdHash: Long,
       addr1: String,
       addr2: String,
       totalCount: Int = 0,
       damagedCount: Int = 0,
       coolerBags: List[CoolerBagCollected],
       requestAt: String)
}
