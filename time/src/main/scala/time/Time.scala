package time

import java.text.SimpleDateFormat
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.slf4j.Logger
import org.slf4j.LoggerFactory

object Time {

  val log = LoggerFactory.getLogger(classOf[Time])

  case class Options(source: Option[String] = None,
                     target: Option[String] = None,
                     sourceTime: Option[String] = None)

  def main(args: Array[String]) {
    val parser = new scopt.OptionParser[Options]("time") {
       head("time", "0.1")
       opt[String]('s', "source").required().valueName("<source>"). 
         action { (s, opts) => opts.copy(source = Option(s)) }
       opt[String]('t', "target").required().valueName("<target>").
         action { (t, opts) => opts.copy(target = Option(t)) }
       opt[String]('x', "source-time") action { (x, opts) => 
         opts.copy(sourceTime = Option(x)) 
       }
    }

    parser.parse(args, Options()) match {
      case Some(opts) => opts.sourceTime match { 
        case Some(time) => opts.source.map { source => opts.target.map {
          target => log.info(new Time().convert(source, target, Option(time)))
        }}
        case None => opts.source.map { source => opts.target.map { target => {
          log.info(new Time().convert(source, target)) 
        }}}
      }
      case None => log.error("Options not configured correctly!")
    }
  }
}

protected[time] class Time {

  import Time._

  def convert(source: String, target: String, 
              sourceTime: Option[String] = None): String = {
    val sourcetimezone = DateTimeZone.forID(source)
    val targettimezone = DateTimeZone.forID(target)
    val datetime = sourceTime match {
      case Some(t) => new DateTime(t, sourcetimezone)
      case None => new DateTime(sourcetimezone)
    }
    val sourcedate = datetime.toLocalDateTime.toDate
    val targetdate = datetime.withZone(targettimezone).toLocalDateTime.toDate
    val formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
    ("Time at "+ sourcetimezone.getID + " is " + formatter.format(sourcedate) + 
     ", that is " + formatter.format(targetdate) + " at "+ target + ".")
  }
}
