package main

import info.folone.ddl.DDLParser

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val res = DDLParser.parse(
      """
        |CREATE TABLE `account` (
        |  `account_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
        |  `type` tinyint(3) unsigned NOT NULL,
        |  `balance` decimal(12,4) DEFAULT NULL,
        |  `avg_fee` float DEFAULT NULL,
        |  `currency` varchar(3) NOT NULL DEFAULT 'USD',
        |  `app_id` int(10) unsigned DEFAULT NULL,
        |  `user_id` int(10) unsigned DEFAULT NULL,
        |  `payment_provider_configuration_id` int(10) unsigned DEFAULT NULL,
        |  PRIMARY KEY (`account_id`),
        |  KEY `IDX_APP_ID` (`app_id`),
        |  KEY `IDX_USER_ID` (`user_id`),
        |  KEY `IDX_ACCOUNT_PPC` (`payment_provider_configuration_id`),
        |  KEY `IDX_ACCOUNT_TYPE_CURRENCY` (`type`,`currency`) USING HASH,
        |  KEY `IDX_ACCOUNT_TYPE_APP` (`type`,`app_id`) USING HASH
        |) ENGINE=InnoDB AUTO_INCREMENT=149891 DEFAULT CHARSET=latin1;
      """.stripMargin)

    println(res)
  }
}