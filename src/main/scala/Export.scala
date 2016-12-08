package pigment

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import cats.syntax.either._

package object export {
  // def toBase64(model: RootModel): String = {
  //   import scala.scalajs.js
  //   import org.scalajs.dom._
  //   import js.typedarray._
  //   import js.typedarray.TypedArrayBufferOps._
  //   import js.Dynamic.global
  //   import java.nio.ByteBuffer
  //   import boopickle.Default._

  //   implicit class ByteBufferOpt(data: ByteBuffer) {
  //     def toArrayBuffer: ArrayBuffer = {
  //       if (data.hasTypedArray()) {
  //         // get relevant part of the underlying typed array
  //         data.typedArray().subarray(data.position, data.limit).buffer
  //       } else {
  //         // fall back to copying the data
  //         val tempBuffer = ByteBuffer.allocateDirect(data.remaining)
  //         val origPosition = data.position
  //         tempBuffer.put(data)
  //         data.position(origPosition)
  //         tempBuffer.typedArray().buffer
  //       }
  //     }
  //   }

  //   val byteBuffer = Pickle.intoBytes(model)
  //   val buffer = new Uint8Array(byteBuffer.toArrayBuffer, 0, byteBuffer.remaining)
  //   val s = new StringBuilder(buffer.length)
  //   for (i <- 0 until buffer.length) {
  //     s ++= global.String.fromCharCode(buffer(i)).asInstanceOf[String]
  //   }
  //   window.btoa(s.result)
  // }

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

    model.asJson.noSpaces
  }

  def fromJson(json: String): Either[io.circe.Error, RootModel] = {
    decode[RootModel](json)
  }
}
