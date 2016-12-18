package pigment

// import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import cats.syntax.either._

import scala.scalajs.js
import js.Dynamic.{global => g}

import scala.scalajs.js
import org.scalajs.dom._
import js.typedarray._
import js.typedarray.TypedArrayBufferOps._
import js.Dynamic.global
import java.nio.ByteBuffer
import boopickle.Default._
import window._

trait ModelCodec[T] {
  def decode(data: T): Option[RootModel] = ???
  def encode(model: RootModel): T = ???
}

package object export {
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

  def toBase64(model: RootModel): String = {
    val buffer = Pickle.intoBytes(model)
    val s = new StringBuilder(buffer.limit)
    for (i <- 0 until buffer.limit) {
      val c = buffer.get
      s ++= global.String.fromCharCode(c & 0xFF).asInstanceOf[String]
    }

    btoa(s.result)
  }

  def fromBase64(base64: String): Option[RootModel] = {
    val byteString = atob(base64)
    val buffer = ByteBuffer.allocateDirect(byteString.size)
    byteString.foreach(c => buffer.put(c.toByte))
    buffer.flip()
    Some(Unpickle[RootModel].fromBytes(buffer))
  }

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

  def toJson(model: RootModel): String = {
    g.encodeURIComponent(model.asJson.noSpaces).asInstanceOf[String]
  }

  def fromJson(json: String): Either[io.circe.Error, RootModel] = {
    decode[RootModel](g.decodeURIComponent(json).asInstanceOf[String])
  }

  implicit class ByteBufferOpt(data: ByteBuffer) {
    def toArrayBuffer: ArrayBuffer = {
      if (data.hasTypedArray()) {
        // get relevant part of the underlying typed array
        data.typedArray().subarray(data.position, data.limit).buffer
      } else {
        // fall back to copying the data
        val tempBuffer = ByteBuffer.allocateDirect(data.remaining)
        val origPosition = data.position
        tempBuffer.put(data)
        data.position(origPosition)
        tempBuffer.typedArray().buffer
      }
    }
  }

}
