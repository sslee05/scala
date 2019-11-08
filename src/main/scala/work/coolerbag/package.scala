package work.coolerbag

package object data {

  case class BagCode(code: String, status: String)
  case class Coolerbag(memberSrl: String, addr1: String, addr2: String, bagCodes: List[BagCode])
  case class CustomerAddr(memberSrl: String, addr1: String, addr2: String)
  case class CoolerbagData(coolerbags: List[Coolerbag])
}
