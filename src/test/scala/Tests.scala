package test

import info.folone.ddl.DDLParser
import org.scalatest.FunSuite

class Tests extends FunSuite {
  test("test1") {
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

    assert(res.successful)
  }

  test("test2") {
    val res = DDLParser.parse(
      """CREATE TABLE `action_step` (
        |  `action_step_id` bigint(20) NOT NULL AUTO_INCREMENT,
        |  `type` int(11) NOT NULL DEFAULT '0',
        |  `optional` tinyint(1) unsigned NOT NULL DEFAULT '0',
        |  `enabled` tinyint(1) unsigned NOT NULL DEFAULT '0',
        |  `sort_order` int(10) unsigned NOT NULL DEFAULT '0',
        |  `action_id` bigint(20) NOT NULL,
        |  `from_term_id` int(10) unsigned DEFAULT NULL,
        |  `to_term_id` int(10) unsigned DEFAULT NULL,
        |  `send_email` tinyint(1) unsigned DEFAULT '0',
        |  `email_subject` varchar(1000) CHARACTER SET utf8mb4 DEFAULT NULL,
        |  `email_body` varchar(5000) CHARACTER SET utf8mb4 DEFAULT NULL,
        |  PRIMARY KEY (`action_step_id`),
        |  KEY `FK_ACTION_STEP_ACTION` (`action_id`),
        |  KEY `FK_ACTION_STEP_FROM_TERM` (`from_term_id`),
        |  KEY `FK_ACTION_STEP_TO_TERM` (`to_term_id`),
        |  CONSTRAINT `FK_ACTION_STEP_ACTION` FOREIGN KEY (`action_id`) REFERENCES `action` (`action_id`),
        |  CONSTRAINT `FK_ACTION_STEP_FROM_TERM` FOREIGN KEY (`from_term_id`) REFERENCES `term` (`term_id`),
        |  CONSTRAINT `FK_ACTION_STEP_TO_TERM` FOREIGN KEY (`to_term_id`) REFERENCES `term` (`term_id`)
        |) ENGINE=InnoDB AUTO_INCREMENT=17 DEFAULT CHARSET=latin1 COMMENT='Steps of mass operation';
      """.stripMargin)

    assert(res.successful)
  }

  test("test3") {
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

    assert(res.successful)
  }
}