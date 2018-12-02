package main

import info.folone.ddl.DDLParser

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val res = DDLParser.parse(
      """
        |CREATE TABLE `external_tx` (
        |  `ext_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
        |  `tracking_id` varchar(40) CHARACTER SET latin1 COLLATE latin1_general_cs NOT NULL,
        |  `source` tinyint(4) NOT NULL,
        |  `tx_type` tinyint(3) NOT NULL,
        |  `state` tinyint(3) NOT NULL,
        |  `amount` decimal(10,4) DEFAULT NULL,
        |  `currency` varchar(3) NOT NULL DEFAULT 'USD',
        |  `external_tx_id` varchar(60) CHARACTER SET latin1 COLLATE latin1_general_cs DEFAULT NULL,
        |  `sender_email` varchar(255) DEFAULT NULL,
        |  `fee` decimal(8,4) DEFAULT NULL,
        |  `stamp` datetime NOT NULL,
        |  `message` varchar(255) DEFAULT NULL,
        |  `refund_external_tx_id` varchar(60) DEFAULT NULL,
        |  `refund_state` smallint(6) NOT NULL DEFAULT '0',
        |  `payment_provider_configuration_id` int(10) unsigned NOT NULL,
        |  `tax_amount` decimal(10,4) DEFAULT NULL,
        |  `sales_tax_rate_id` int(10) unsigned DEFAULT NULL,
        |  `pre_tax_amount` decimal(10,4) NOT NULL,
        |  `refunded_amount` decimal(10,4) DEFAULT NULL,
        |  `error_context` mediumtext,
        |  `rate` decimal(10,4) DEFAULT NULL,
        |  `tax_provider_configuration_id` int(10) unsigned DEFAULT NULL,
        |  `is_manually_refunded` tinyint(4) NOT NULL DEFAULT '0',
        |  `refunded_tax_amount` decimal(10,4) DEFAULT NULL,
        |  `downstream_external_tx_id` varchar(60) CHARACTER SET latin1 DEFAULT NULL COMMENT 'Tx id in terminal processing system (e.g. for Paypal via Braintree - Paypal tx id). Regular external tx id could be just an intermediate tx id on high-level provider',
        |  `refund_downstream_external_tx_id` varchar(60) CHARACTER SET latin1 DEFAULT NULL COMMENT 'Refund Tx id in terminal processing system (see also downstream_external_tx_id)',
        |  PRIMARY KEY (`ext_id`),
        |  UNIQUE KEY `IX_TRACKING_ID` (`tracking_id`),
        |  UNIQUE KEY `IX_EXTERNAL_TX_ID` (`external_tx_id`),
        |  KEY `IDX_STATE` (`state`),
        |  KEY `IDX_SOURCE` (`source`),
        |  KEY `IDX_SENDER_EMAIL` (`sender_email`) USING BTREE,
        |  KEY `IDX_UPI_PPC` (`payment_provider_configuration_id`)
        |) ENGINE=InnoDB AUTO_INCREMENT=118915 DEFAULT CHARSET=utf8;
      """.stripMargin)
    println(res)
  }
}