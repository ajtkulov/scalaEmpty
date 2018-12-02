package main

import info.folone.ddl.DDLParser

object Main extends App {
  override def main(args: Array[String]): Unit = {
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
    println(res)
  }
}