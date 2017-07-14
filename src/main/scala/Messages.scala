sealed trait AkkaMessage

case class RemoveOld() extends AkkaMessage

case class Get(appId: String) extends AkkaMessage

