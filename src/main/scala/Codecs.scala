package pigment

import cats.syntax.either._

import scala.scalajs.js
import org.scalajs.dom._
import js.Dynamic.{global => g}
import window._

import java.nio.ByteBuffer

trait ModelCodec[T] {
  def encode(model: RootModel): T = ???
  def decode(data: T): Option[RootModel] = ???
}

object BooPickleCodec extends ModelCodec[ByteBuffer] {
  import boopickle.Default._

  // explicitly store colors in LCH (polar coordinates in LAB)
  // (to not lose hue value) and store them as floats to save space
  implicit val colorPickler: Pickler[Color] = transformPickler { (a: Array[Float]) =>
    def dec(x: Float) = x.toDouble
    LCH(dec(a(0)), dec(a(1)), dec(a(2))): Color
  } { (c: Color) =>
    val lch = c.lch
    def enc(x: Double) = x.toFloat
    Array(enc(lch.l), enc(lch.c), enc(lch.h))
  }

  override def encode(model: RootModel) = Pickle.intoBytes(model)
  override def decode(data: ByteBuffer) = Some(Unpickle[RootModel].fromBytes(data))
}

object Base64Codec extends ModelCodec[String] {
  override def encode(model: RootModel) = {
    val buffer = BooPickleCodec.encode(model)
    val s = new StringBuilder(buffer.limit)
    for (i <- 0 until buffer.limit) {
      val c = buffer.get
      s ++= g.String.fromCharCode(c & 0xFF).asInstanceOf[String]
    }

    btoa(s.result)
  }
  override def decode(data: String): Option[RootModel] = {
    val byteString = atob(data)
    val buffer = ByteBuffer.allocateDirect(byteString.size)
    byteString.foreach(c => buffer.put(c.toByte))
    buffer.flip()
    BooPickleCodec.decode(buffer)
  }
}

object JsonCodec extends ModelCodec[String] {
  import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

  implicit val encodeColor: Encoder[Color] = Encoder.encodeList[Double].contramap[Color] { c =>
    val lch = c.lch
    List(lch.l, lch.c, lch.h).map(x => math.round(100 * x) / 100.0)
  }
  implicit val decodeColor: Decoder[Color] = Decoder.decodeList[Double].emap { list =>
    Either.catchNonFatal(LCH(list(0), list(1), list(2))).leftMap(t => "LCH")
  }

  implicit val encodeColorScheme: Encoder[ColorScheme] = Encoder.encodeMapLike[Map, Int, IndexedSeq[Color]].contramap[ColorScheme](_.groups)
  implicit val decodeColorScheme: Decoder[ColorScheme] = Decoder.decodeMapLike[Map, Int, IndexedSeq[Color]].emap { groups =>
    Either.catchNonFatal(ColorScheme(groups)).leftMap(t => "ColorScheme")
  }

  override def encode(model: RootModel): String = {
    g.encodeURIComponent(model.asJson.noSpaces).asInstanceOf[String]
  }

  override def decode(json: String) = {
    io.circe.parser.decode[RootModel](g.decodeURIComponent(json).asInstanceOf[String]).toOption
  }
}
