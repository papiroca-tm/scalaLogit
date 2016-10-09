/*todo: scaladoc*/

package scalaLogit

import org.joda.time.DateTime
import scalaLogit.Log.LogMode.LoggingMode

/**
  * Created by papiroca on 10/8/16.
  */
object Log {

  private val STD_OUT: Boolean = true // todo: LogOptions.isStdOut
  private val FILE_OUT: Boolean = false // todo: LogOptions.isFileOut
  private val MQ_OUT: Boolean = false // todo: LogOptions.isMqOut
  private val DB_OUT: Boolean = false // todo: LogOptions.isDbOut
  private val DATE_TIME_FORMAT_STRING: String = "dd.MM.Y HH:mm:ss:SSS:Z"


  object LogMode extends Enumeration() {
    type LoggingMode = Value
    val FULL = Value("FULL")
    val SIMPLE = Value("SIMPLE")
  }

  def info(text: String, mode: LoggingMode = LogMode.SIMPLE) = {
    val systemInfo: Map[String, String] = getSystemInfo
    logIt("info", text, systemInfo, mode)
  }

  def debug(text: String, mode: LoggingMode = LogMode.SIMPLE) = {
    val systemInfo: Map[String, String] = getSystemInfo
    logIt("debug", text, systemInfo, mode)
  }

  def success(text: String, mode: LoggingMode = LogMode.SIMPLE) = {
    val systemInfo: Map[String, String] = getSystemInfo
    logIt("success", text, systemInfo, mode)
  }

  def error(text: String, mode: LoggingMode = LogMode.SIMPLE) = {
    val systemInfo: Map[String, String] = getSystemInfo
    logIt("error", text, systemInfo, mode)
  }

  def warn(text: String, mode: LoggingMode = LogMode.SIMPLE) = {
    val systemInfo: Map[String, String] = getSystemInfo
    logIt("warn", text, systemInfo, mode)
  }

  private def getSystemInfo: Map[String, String] = {
    val now = new DateTime().toString(DATE_TIME_FORMAT_STRING)
    val fileName = Thread.currentThread().getStackTrace()(3).getFileName
    val lineNumber = Thread.currentThread().getStackTrace()(3).getLineNumber.toString
    val cnArr = Thread.currentThread().getStackTrace()(3).getClassName.split("[$]")
    val pkgName = cnArr(0).split("[.]")(0)
    val name = cnArr(0).split("[.]")(1)
    val defName = cnArr(3)
    Map("dateTime" -> now, "pkgName" -> pkgName, "fileName" -> fileName, "name" -> name, "defName" -> defName, "lineNumber" -> lineNumber)
  }

  private def logIt(logType: String, text: String, systemInfo: Map[String, String], mode: LoggingMode) = {
    val dateTime = systemInfo("dateTime")
    val pkgName = systemInfo("pkgName")
    val fileName = systemInfo("fileName")
    val name = systemInfo("name")
    val defName = systemInfo("defName")
    val lineNumber = systemInfo("lineNumber")
    if (STD_OUT) {
      if (mode == LogMode.FULL) { stdOut(logType, s"$dateTime : $pkgName : $fileName : $name : $defName : line $lineNumber - ", text) }
      else { stdOut(logType, s"$fileName : line $lineNumber - ", text) }
    }
    if (FILE_OUT) { /*todo: if (FILE_OUT)*/ }
    if (MQ_OUT) { /*todo: if (MQ_OUT)*/ }
    if (DB_OUT) { /*todo: if (DB_OUT)*/ }
  }

  private def stdOut(logType: String, systemStr: String, text: String): Unit = {
    logType match {
      case "info" =>  Console.out.println(Console.BLUE + "[INFO] " + systemStr + Console.RESET + text)
      case "debug" => Console.out.println(Console.MAGENTA + "[DEBUG] " + systemStr + Console.RESET + text)
      case "success" => Console.out.println(Console.GREEN + "[SUCCESS] " + systemStr + Console.RESET + text)
      case "error" => Console.out.println(Console.RED + "[ERROR] " + systemStr + Console.RESET + text)
      case "warn" => Console.out.println(Console.YELLOW + "[WARN] " + systemStr + Console.RESET + text)
    }
  }

}