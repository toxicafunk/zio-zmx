package zio.zmx.metrics

import zio.zmx._
import zio.zmx.Metric._
import java.text.DecimalFormat
import java.time.Instant

object Encoder {

  val format = new DecimalFormat("0.################")

  private def encode(
    name: String,
    value: String,
    sampleRate: Double,
    metricType: String,
    tags: Tag*
  ): Option[String] = {
    val tagString = if (tags.isEmpty) "" else "|#" + tags.mkString(",")
    val rate      = if (sampleRate < 1.0) s"|@${format.format(sampleRate)}" else ""
    Some(s"${name}:${value}|${metricType}${rate}${tagString}")
  }

  private def encodeEvent(event: Event): String = {
    val timestamp   = event.timestamp.fold(s"|d:${Instant.now().toEpochMilli()}")(l => s"|d:$l")
    val hostname    = event.hostname.fold("")(h => s"|h:$h")
    val aggKey      = event.aggregationKey.fold("")(k => s"|k:$k")
    val priority    = event.priority.fold("|p:normal")(s => s"|p:$s")
    val sourceType  = event.sourceTypeName.fold("")(s => s"|s:$s")
    val alertType   = event.alertType.fold("|t:info")(s => s"|t:$s")
    val tagString   = if (event.tags.isEmpty) "" else "|#" + event.tags.mkString(",")
    val encodedText = event.text.replace("\n", "\\\\n")
    s"_e{${event.name.size},${event.text.size}}:${event.name}|${encodedText}$timestamp$hostname$aggKey$priority$sourceType$alertType" + tagString
  }

  private def encodeSeviceCheck(serviceCheck: ServiceCheck): String = {
    val name      = serviceCheck.name
    val status    = serviceCheck.status
    val timestamp = serviceCheck.timestamp.fold(s"|d:${Instant.now().toEpochMilli()}")(l => s"|d:$l")
    val hostname  = serviceCheck.hostname.fold("")(h => s"|h:$h")
    val tagString = if (serviceCheck.tags.isEmpty) "" else "|#" + serviceCheck.tags.mkString(",")
    val message   = serviceCheck.message.fold("")(m => s"|m:${m.replace("\n", "\\\\n")}")
    s"_sc|$name|$status$timestamp$hostname$tagString$message"
  }

  def encode[A](metric: Metric[A]): Option[String] = metric match {
    case Counter(name, value, sampleRate, tags)   => encode(name, format.format(value), sampleRate, "c")
    case Gauge(name, value, tags)                 => encode(name, format.format(value), 1.0, "g")
    case Histogram(name, value, sampleRate, tags) => encode(name, format.format(value), sampleRate, "h")
    case Meter(name, value, tags)                 => encode(name, format.format(value), 1.0, "m")
    case Set(name, value, tags)                   => encode(name, format.format(value), 1.0, "s")
    case Timer(name, value, sampleRate, tags)     => encode(name, format.format(value), sampleRate, "ms")
    case evt: Event                               => Some(encodeEvent(evt))
    case chk: ServiceCheck                        => Some(encodeSeviceCheck(chk))
    case _                                        => None
  }
}
