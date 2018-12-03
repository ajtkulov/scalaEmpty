package main

import info.folone.ddl.DDLParser

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val res = DDLParser.parse(
      """
        |DROP TABLE IF EXISTS `external_api_configuration`;
        |/*!40101 SET @saved_cs_client     = @@character_set_client */;
        |/*!40101 SET character_set_client = utf8 */;
        |CREATE TABLE `external_api_configuration` (
        |  `external_api_configuration_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
        |  `pub_id` varchar(12) NOT NULL,
        |  `app_id` int(10) unsigned NOT NULL,
        |  `name` varchar(70) CHARACTER SET utf8mb4 NOT NULL,
        |  `description` mediumtext CHARACTER SET utf8mb4,
        |  `fields` mediumtext CHARACTER SET utf8 NOT NULL,
        |  `properties` mediumtext CHARACTER SET utf8 NOT NULL,
        |  `create_date` datetime NOT NULL,
        |  `create_by` int(10) unsigned NOT NULL,
        |  `update_date` datetime DEFAULT NULL,
        |  `update_by` int(10) unsigned NOT NULL,
        |  `external_api_source` int(11) NOT NULL,
        |  `deleted` smallint(6) DEFAULT '0',
        |  `enforce_uniqueness` tinyint(1) NOT NULL DEFAULT '0',
        |  PRIMARY KEY (`external_api_configuration_id`),
        |  KEY `IDX_EXT_CONF_APP` (`app_id`),
        |  KEY `IDX_EXT_ACC_PUB_INDEX` (`pub_id`)
        |) ENGINE=InnoDB AUTO_INCREMENT=172 DEFAULT CHARSET=latin1;
        |/*!40101 SET character_set_client = @saved_cs_client */;
        |
        |--
        |-- Dumping data for table `external_api_configuration`
        |--
        |
        |LOCK TABLES `external_api_configuration` WRITE;
        |
        |
        |LOCK TABLES `external_api_configuration` WRITE;
        |/*!40000 ALTER TABLE `external_api_configuration` DISABLE KEYS */;
        |INSERT INTO `external_api_configuration` VALUES (1,'EAYQD40JCG1X',4334,'asdf','asdf','[{\"fieldName\":\"receiptData\",\"fieldTitle\":\"Receipt data\",\"description\":\"App store receipt\",\"mandatory\":true,\"hidden\":false,\"defaultValue\":null,\"order\":0}]','{\"password\":\"asdf\",\"appStoreReceiptUrl\":\"asdfasdf\"}','2015-11-05 01:59:57',18944,'2015-11-05 01:59:57',18944,3,0,0),(170,'EAHKYC5SY7DC',4298,'TEST 1234',NULL,'[{\"fieldName\":\"qwsa\",\"fieldTitle\":\"qwerty\",\"description\":\"sdff\",\"mandatory\":true,\"hidden\":false,\"defaultValue\":\"1234\",\"order\":0,\"type\":null}]','{\"prodId\":\"122334545\"}','2016-11-08 03:59:44',15451,'2016-11-08 04:00:25',15451,1,0,0);
        |/*!40000 ALTER TABLE `external_api_configuration` ENABLE KEYS */;
        |UNLOCK TABLES;
      """.stripMargin)
    println(res)
  }
}