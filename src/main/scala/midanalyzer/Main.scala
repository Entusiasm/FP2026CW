package midanalyzer

import midanalyzer.ui.MidiFrame
import midanalyzer.pipeline.AnalysisPipeline
import midanalyzer.report.Report
import midanalyzer.model.Config
import zio.{Runtime, Unsafe, Console, ZIO}
import javax.swing.{JFrame, SwingUtilities, WindowConstants}
import java.awt.event.{WindowAdapter, WindowEvent}

object Main {
  def main(args: Array[String]): Unit = {
    if (args.nonEmpty) {
      // Консольный режим
      val path = args(0)
      val config = Config()
      val effect: ZIO[Any, Throwable, Unit] = 
        AnalysisPipeline.analyze(path, config).flatMap { case (result, logs) =>
          Console.printLine(logs.mkString("\n")) *>
          Report.saveJson(result, path + ".json") *>
          Report.saveText(result, path + ".txt")
        }.mapError(e => new Exception(e.toString))
      
      Unsafe.unsafe { implicit u =>
        Runtime.default.unsafe.run(effect).getOrThrow()
      }
    } else {
      // GUI режим – блокируем поток до закрытия окна
      val runtime = Runtime.default
      val frame = new MidiFrame(runtime)
      // Используем WindowConstants.DISPOSE_ON_CLOSE
      frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
      SwingUtilities.invokeLater(() => frame.setVisible(true))
      
      // Блокировка main потока до закрытия окна
      val lock = new Object()
      frame.addWindowListener(new WindowAdapter {
        override def windowClosed(e: WindowEvent): Unit = lock.synchronized(lock.notify())
        override def windowClosing(e: WindowEvent): Unit = lock.synchronized(lock.notify())
      })
      lock.synchronized(lock.wait())
    }
  }
}