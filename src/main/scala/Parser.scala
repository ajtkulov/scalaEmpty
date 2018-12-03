package info.folone.ddl

import scala.util.parsing.combinator._

object DDLParser extends JavaTokenParsers {
  val tableName = """(?!(?i)KEY)(?!(?i)PRIMARY)(?!(?i)UNIQUE)(`)?[a-zA-Z_0-9]+(`)?""".r
  val columnName = tableName
  val indexName = tableName
  val default = """[_a-zA-Z'\(\)[0-9]]+""".r
  val keyName = tableName
  val engine = tableName
  val charset = tableName
  val dataType = """[a-zA-Z]+(\([0-9,]+\))?""".r
  val statementTermination = ";"
  val columnDelimiter = """,*""".r

  val quotedStr = """\'(\\.|[^\'])*\'""".r

  final case class Table(name: String, columns: Set[Column], constraints: Set[Constraint])

  final case class Column(name: String, datatype: String, notNull: Boolean,
                          autoInc: Boolean, defaultVal: Option[String])

  sealed trait Constraint

  final case class UniqueKey(name: Option[String], column: String) extends Constraint

  final case class PrimaryKey(column: String) extends Constraint

  final case class ForeignKey(name: String, column: String, foreignTable: String, foreignColumn: String) extends Constraint

  final case class Key(name: Option[String], column: String) extends Constraint

  def cleanString(str: String) = str.replaceAll("`", "")

  sealed trait Value

  final case class IntValue(value: Int) extends Value
  final case class DoubleValue(value: Double) extends Value
  final case class NullValue() extends Value
  final case class StringValue(value: String) extends Value

  case class Values(values: List[Value])
  case class InsertValues(table: String, values: List[Values])

  // Handle comments
  protected override val whiteSpace =
    """(\s|#.*|(?m)/\*(\*(?!/)|[^*])*\*/;*)+""".r

  def column = columnName ~ dataType ~
    ((("""CHARACTER SET""".r) ~ default ~ (("COLLATE".r ~ default)?)) ?) ~
    ("""unsigned""".r ?) ~
    ("""(?i)NOT NULL""".r ?) ~
    ("""(?i)AUTO_INCREMENT""".r ?) ~
    ((("""(?i)DEFAULT""".r) ~ default) ?) ~
    (("COMMENT".r ~ quotedStr)?) ~
    columnDelimiter

  def uniqueOrPk = ("""(?i)(PRIMARY|UNIQUE)""".r ?) ~ ("""(?i)KEY""".r) ~
    (keyName ?) ~ "(" ~ columnName ~ ((columnDelimiter ~ columnName) *) ~ ")" ~ (("USING".r ~ default) ?) ~ columnDelimiter ^^ {
    case kind ~ _ ~ name ~ "(" ~ column ~ others ~ ")" ~ _ ~ _ =>
      kind match {
        case Some(x) if x.equalsIgnoreCase("primary") => PrimaryKey(column)
        case Some(x) if x.equalsIgnoreCase("unique") => UniqueKey(name, column)
        case None => Key(name, column)
      }
  }

  def fk =
    """(?i)CONSTRAINT""".r ~ keyName ~ """FOREIGN KEY""".r ~
      "(" ~ columnName ~ ")" ~
      """(?i)REFERENCES""".r ~
      tableName ~ "(" ~ columnName ~ ")" ~ columnDelimiter ^^ {
      case _ ~ keyName ~ _ ~ "(" ~ columnName ~ ")" ~ _ ~
        tableName ~ "(" ~ extColumn ~ ")" ~ _ =>
        ForeignKey(keyName, columnName, tableName, extColumn)
    }

  def constraint = (uniqueOrPk | fk)

  def tableMetaInfo =
    """(?i)ENGINE=""".r ~ engine ~
      ("AUTO_INCREMENT=[0-9]+".r ?) ~
      """(?i)DEFAULT CHARSET=""".r ~ charset ~
      (("""COMMENT=""".r ~ quotedStr)?)

  def createTable = ("""(?i)CREATE TABLE""".r) ~ ("""(?i)IF NOT EXISTS""".r ?) ~ tableName ~
    "(" ~ (column *) ~ (constraint *) ~ ")" ~ (tableMetaInfo ?) ^^ {
    case _ ~ _ ~ name ~ "(" ~ columns ~ constraints ~ ")" ~ meta => {
      val columnsData = columns map {
        case colName ~ colType ~ charSet ~ unsigned ~ notNull ~ autoInc ~ isDefault ~ _ ~ _ =>
          Column(cleanString(colName), colType, notNull.isDefined,
            autoInc.isDefined, isDefault.map(_._2))
      }
      Table(cleanString(name), columnsData.toSet, constraints.toSet)
    }
  }

  def dropTable = "(?i)DROP TABLE".r ~ ("IF EXISTS".r ?) ~ tableName

  def lockTable = "(?i)LOCK TABLES".r ~ tableName ~ "WRITE"

  def nullValue: DDLParser.Parser[Value] = "NULL".r ^^ (_ => NullValue())
  def decimalValue: DDLParser.Parser[Value] = decimalNumber ^^ (value => if (value.contains(".")) DoubleValue(value.toDouble) else IntValue(value.toInt) )
  def stringValue: DDLParser.Parser[Value] = quotedStr ^^ (value =>  StringValue(value))

  def value: DDLParser.Parser[Value] = nullValue | decimalValue | stringValue ^^ identity

  def argumentValues = "(" ~ value ~ (("," ~ value)*) ~ ")" ^^ {
    case _ ~ value ~ values ~ _ => Values(List(value) ++ values.map(_._2))
  }

  def insertValues = "INSERT INTO".r ~ tableName ~ "VALUES".r ~ argumentValues ~ (("," ~ argumentValues)*) ^^ {
    case _ ~ tableName ~ _ ~ arg ~ args => InsertValues(tableName, List(arg) ++ args.map(_._2))
  }

  def unlock = "UNLOCK TABLES".r

  def statement = (createTable | dropTable | lockTable | insertValues | unlock) ~ statementTermination ^^ {
    case res ~ _ => res
  }

  def program = statement *

  def parse(sql: String) = parseAll(program, sql.split("\n").filterNot(_.startsWith("--")).mkString("\n")) map (_.toSet)
}
