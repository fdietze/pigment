package pigment

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

  def toJson(model: RootModel): String = {
    import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

    model.asJson.noSpaces
  }

  def fromJson(json: String): Either[io.circe.Error, RootModel] = {
    import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

    decode[RootModel](json)
  }
}
