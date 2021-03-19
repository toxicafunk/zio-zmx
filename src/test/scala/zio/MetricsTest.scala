package zio

import zio.test.DefaultRunnableSpec
import zio.test._
import zio.console._

import zio.clock.Clock
import zio.internal.Platform

object MetricsTest extends DefaultRunnableSpec {

  object Test2 {
    val rts: zio.Runtime[Console with Clock] =
      zio.Runtime.unsafeFromLayer(Clock.live >+> Console.live, Platform.default)
  }

  def spec =
    suite("Metrics Layer Test")(
      zio.test.test("unsafeFromLayer") {
        type Metrics = Has[Metrics.Service]

        object Metrics {
          trait Service {
            def log(): Task[Unit]
          }

          val receiverHas: ZLayer[Has[String], Nothing, Metrics] =
            ZLayer.fromFunction[Has[String], Metrics.Service](text =>
              new Service {
                override def log(): Task[Unit] = Task(println(text))
              }
            )
        }

        val chHas                                                     = ZLayer.succeed[String]("My cool text")
        val rLayerHas: ZLayer[Any, Nothing, Metrics]                  = chHas >>> Metrics.receiverHas
        println(s"defining ReceiverHas RT: $rLayerHas")
        val combinedLayer: ZLayer[Any, Nothing, Metrics with Console] = rLayerHas ++ Console.live
        println(s"combined: $combinedLayer")
        val rtReceiverHas                                             = Runtime.unsafeFromLayer(combinedLayer)

        println("defining Test program")
        val program =
          for {
            m <- RIO.environment[Metrics]
            _ <- m.get.log()
          } yield ()

        rtReceiverHas.unsafeRun(program)
        assertCompletes
      },
      zio.test.test("composeTest") {
        println(Test2.rts)
        assertCompletes
      }
    )
}
