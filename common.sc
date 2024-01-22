import mill._
import scalalib._

trait MOESIModule extends ScalaModule {

  def rocketModule: ScalaModule

  override def moduleDeps = super.moduleDeps ++ Seq(rocketModule)
}
