package midanalyzer.ui

import midanalyzer.model.{Config, AnalysisResult}
import midanalyzer.pipeline.AnalysisPipeline
import midanalyzer.report.Report
import zio.{Runtime, Unsafe, ZIO}
import javax.swing._
import java.awt.{BorderLayout, FlowLayout}
import scala.concurrent.{ExecutionContext, Future}

class MidiFrame(runtime: Runtime[Any]) extends JFrame("MIDI Analyzer") {
  private var resultOpt: Option[AnalysisResult] = None
  private val textArea = new JTextArea(20, 60)
  private val openButton = new JButton("Open MIDI file")
  private val analyzeButton = new JButton("Analyze")
  private val saveJsonButton = new JButton("Save JSON")
  private val saveTxtButton = new JButton("Save text")
  private val statusLabel = new JLabel("Ready")
  private var currentPath: String = _
  private var analyzing = false

  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  setSize(900, 700)
  setLayout(new BorderLayout())

  val topPanel = new JPanel(new FlowLayout())
  topPanel.add(openButton)
  topPanel.add(analyzeButton)
  topPanel.add(saveJsonButton)
  topPanel.add(saveTxtButton)
  add(topPanel, BorderLayout.NORTH)

  val scrollPane = new JScrollPane(textArea)
  add(scrollPane, BorderLayout.CENTER)

  add(statusLabel, BorderLayout.SOUTH)

  analyzeButton.setEnabled(false)
  saveJsonButton.setEnabled(false)
  saveTxtButton.setEnabled(false)

  openButton.addActionListener(_ => openFile())
  analyzeButton.addActionListener(_ => analyzeFile())
  saveJsonButton.addActionListener(_ => saveJson())
  saveTxtButton.addActionListener(_ => saveText())

  private def openFile(): Unit = {
    val chooser = new JFileChooser()
    if (chooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
      currentPath = chooser.getSelectedFile.getAbsolutePath
      analyzeButton.setEnabled(true)
      statusLabel.setText(s"Selected: $currentPath")
    }
  }

  private def analyzeFile(): Unit = {
    if (analyzing) return
    analyzing = true
    analyzeButton.setEnabled(false)
    openButton.setEnabled(false)
    statusLabel.setText("Analyzing...")
    textArea.setText("")

    val config = Config()
    // Преобразуем String ошибку в Throwable для Future
    val effect: ZIO[Any, Throwable, (AnalysisResult, List[String])] = 
      AnalysisPipeline.analyze(currentPath, config).mapError(e => new Exception(e))

    Unsafe.unsafe { implicit u =>
      val future: Future[(AnalysisResult, List[String])] = runtime.unsafe.runToFuture(effect)
      future.onComplete {
        case scala.util.Success((result, logs)) =>
          SwingUtilities.invokeLater(() => {
            resultOpt = Some(result)
            textArea.setText(Report.toText(result))
            saveJsonButton.setEnabled(true)
            saveTxtButton.setEnabled(true)
            statusLabel.setText("Analysis complete")
            analyzing = false
            openButton.setEnabled(true)
            analyzeButton.setEnabled(true)
            logs.foreach(println)
          })
        case scala.util.Failure(ex) =>
          SwingUtilities.invokeLater(() => {
            textArea.setText(s"Error: ${ex.getMessage}")
            statusLabel.setText("Error during analysis")
            analyzing = false
            openButton.setEnabled(true)
            analyzeButton.setEnabled(true)
          })
      }(ExecutionContext.global)
    }
  }

  private def saveJson(): Unit = {
    resultOpt.foreach { res =>
      val chooser = new JFileChooser()
      chooser.setSelectedFile(new java.io.File(res.filePath + ".json"))
      if (chooser.showSaveDialog(this) == JFileChooser.APPROVE_OPTION) {
        val path = chooser.getSelectedFile.getAbsolutePath
        Unsafe.unsafe { implicit u =>
          runtime.unsafe.run(Report.saveJson(res, path))
        }
        statusLabel.setText(s"JSON saved to $path")
      }
    }
  }

  private def saveText(): Unit = {
    resultOpt.foreach { res =>
      val chooser = new JFileChooser()
      chooser.setSelectedFile(new java.io.File(res.filePath + ".txt"))
      if (chooser.showSaveDialog(this) == JFileChooser.APPROVE_OPTION) {
        val path = chooser.getSelectedFile.getAbsolutePath
        Unsafe.unsafe { implicit u =>
          runtime.unsafe.run(Report.saveText(res, path))
        }
        statusLabel.setText(s"Text saved to $path")
      }
    }
  }
}