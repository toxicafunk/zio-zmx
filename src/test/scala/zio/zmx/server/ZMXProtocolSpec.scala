/*
 * Copyright 2017-2019 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.zmx.server

import zio.nio.core.ByteBuffer

import zio.test.Assertion._
import zio.test._
import zio.UIO

object ZMXProtocolSpec extends DefaultRunnableSpec {
  def spec =
    suite("ZMXProtocolSpec")(
      suite("Using the RESP protocol")(
        test("zmx test generating a successful command") {
          val p: String = ZMXProtocol.generateRespCommand(args = List("foobar"))
          assert(p)(equalTo("*1\r\n$6\r\nfoobar\r\n"))
        },
        test("zmx test generating a successful multiple command") {
          val p: String = ZMXProtocol.generateRespCommand(args = List("foo", "bar"))
          assert(p)(equalTo("*2\r\n$3\r\nfoo\r\n$3\r\nbar\r\n"))
        },
        test("zmx test generating a successful empty command") {
          val p: String = ZMXProtocol.generateRespCommand(args = List())
          assert(p)(equalTo("*0\r\n"))
        },
        testM("zmx test generating a success reply") {
          val p: UIO[ByteBuffer] = ZMXProtocol.generateReply(ZMXMessage("foobar"), Success)
          for {
            content <- p
            value   <- ZMXProtocol.ByteBufferToString(content)
          } yield assert(value)(equalTo("+foobar"))
        },
        testM("zmx test generating a fail reply") {
          val p: UIO[ByteBuffer] = ZMXProtocol.generateReply(ZMXMessage("foobar"), Fail)
          for {
            content <- p
            value   <- ZMXProtocol.ByteBufferToString(content)
          } yield assert(value)(equalTo("-foobar"))
        },
        test("zmx get size of bulk string") {
          assert(ZMXProtocol.sizeOfBulkString("$6"))(equalTo(6))
        },
        test("zmx get bulk string successfully") {
          assert(ZMXProtocol.getBulkString((List("$6", "foobar"), 6)))(equalTo("foobar"))
        },
        test("zmx get the number of bulk strings") {
          assert(ZMXProtocol.numberOfBulkStrings("*6"))(equalTo(6))
        },
        test("zmx get successful reponse") {
          assert(ZMXProtocol.getSuccessfulResponse("+foobar"))(equalTo("foobar"))
        },
        test("zmx get error response") {
          assert(ZMXProtocol.getErrorResponse("-foobar"))(equalTo("foobar"))
        },
        test("zmx get args from a list") {
          assert(ZMXProtocol.getArgs(List("$3", "foo", "$3", "bar")))(equalTo(List("foo", "bar")))
        },
        test("zmx server received a command with no args") {
          val expected = ZMXServerRequest("foobar", None)
          assert(ZMXProtocol.serverReceived("*1\r\n$6\r\nfoobar\r\n"))(equalTo(Some(expected)))
        },
        test("zmx server received nothing") {
          assert(ZMXProtocol.serverReceived("*0\r\n"))(equalTo(None))
        },
        test("zmx server received a command with one argument") {
          val expected = ZMXServerRequest("foobar", Some(List("argy")))
          assert(ZMXProtocol.serverReceived("*1\r\n$6\r\nfoobar\r\n$4\r\nargy\r\n"))(equalTo(Some(expected)))
        }
      )
    )
}
