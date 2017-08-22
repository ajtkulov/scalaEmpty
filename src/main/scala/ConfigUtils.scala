package infrastructure

import com.typesafe.config.{Config, ConfigFactory}

/**
  * Application configuration utilities
  */
object ConfigUtils {
  private lazy val conf: Config = ConfigFactory.load

  def config(): Config = conf
}
