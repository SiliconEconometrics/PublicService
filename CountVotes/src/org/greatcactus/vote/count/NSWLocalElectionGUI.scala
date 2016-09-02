/*
    Copyright 2015-2016 Silicon Econometrics Pty. Ltd.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

 */

package org.greatcactus.vote.count

import javax.swing.SwingUtilities
import javax.swing.JFrame
import javax.swing.JPanel
import java.awt.BorderLayout
import javax.swing.JButton
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import javax.swing.JFileChooser
import java.io.File
import javax.swing.JEditorPane
import javax.swing.JFormattedTextField
import java.awt.GridLayout
import javax.swing.JLabel
import javax.swing.UIManager

object NSWLocalElectionGUI extends App {
   SwingUtil.inSwingThread(new NSWElectionGUIInfo)
}

object SwingUtil {
  def inSwingThread(code : =>Unit) {
    SwingUtilities.invokeLater(new Runnable() { override def run() { code } })
  }
  def inOTherThread(code : =>Unit) {
    val thread = new Thread(new Runnable() { override def run() { code } })
    thread.start();
  }

}


class NSWElectionGUIInfo {
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
  val textDisclaimer = "<p><em>This program is provided as is. No warranties are provided. Only minimal testing has been done. Don't trust it too far.<em></p><p>Copyright 2016 Silicon Econometrics Pty. Ltd. Source code released under the GPLv3.</p>"
  val textChooseFile = "<p>First you need to get the election data file you want to process. Go to http://www.pastvtr.elections.nsw.gov.au/LGE2012/lge-index.htm and choose your area. Note the number of candidates to be elected and enter it above.</p>"+
                       "<p>Choose then your ward (a link under the heading Election Results). In the page that then comes up, click on the 'Final Results' tab.</p>"+
                       "<p>There are some listed documents. One of these, usually the last, will be called <em>Details Preference for Count.zip</em>. Click on it. You will download a zip file.</p>"+
                       "<p>Now click on the button on this window marked 'Choose a .zip file containing election data' and select the file you downloaded.</p>"
  val textFileFail = "<p><b>Error! I am sorry, but I could not understand that file.</b></p>"
  val textHowToGo = "<p>Now set the number of candidates, the number of times you want to run the election, and press the 'Run count' button</p>"
  
  val fileChooserAction = new ActionListener {
    override def actionPerformed(e:ActionEvent) {
      val fc = new JFileChooser
      if (JFileChooser.APPROVE_OPTION == fc.showOpenDialog(frame)) {
        SwingUtil.inOTherThread{choseFile(fc.getSelectedFile())}
      }
    }
  }

  val frame = new JFrame("NSW Local government 2012")
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
  val pane = frame.getContentPane()
  val optionsPane = new JPanel(new GridLayout(0,2))
  val fileChooser = new JButton("Choose a .zip file containing election data")
  optionsPane.add(fileChooser)
  val fileName = new JLabel("no file chosen")
  optionsPane.add(fileName)
  fileChooser.addActionListener(fileChooserAction)
  optionsPane.add(new JLabel("Number of candidates to elect"))
  val numToElectField = new JFormattedTextField
  numToElectField.setValue(new Integer(3))
  numToElectField.setColumns(3)
  optionsPane.add(numToElectField)
  val numTimesToRun = new JFormattedTextField
  numTimesToRun.setValue(new Integer(100))
  numTimesToRun.setColumns(5)
  optionsPane.add(new JLabel("Number of times to run the election"))
  optionsPane.add(numTimesToRun)
  val goButton = new JButton("Run count")
  goButton.setEnabled(false)
  goButton.addActionListener(new ActionListener() {override def actionPerformed(e:ActionEvent) {runElection(numToElectField.getValue.asInstanceOf[Number].intValue(),numTimesToRun.getValue.asInstanceOf[Number].intValue())}})
  optionsPane.add(new JLabel("When the above entries are set..."))
  optionsPane.add(goButton)
  pane.add(optionsPane,BorderLayout.NORTH);
  val resultsPane = new JEditorPane
  resultsPane.setEditable(false)
  resultsPane.setContentType("text/html")
  resultsPane.setText(textChooseFile+textDisclaimer)
  pane.add(resultsPane,BorderLayout.CENTER);
  frame.pack()
  frame.setVisible(true)

  val output = new StatusOutput {
    def status(isFinished:Boolean,heading:String,margin:String,candidates:IndexedSeq[CandidateStat]) {
       if (isFinished) SwingUtil.inSwingThread{goButton.setEnabled(true);fileChooser.setEnabled(true)}
       val running = if (isFinished) "<p><b>Finished</b></p>" else "<p><b>Running</b></p>"
       val table = "<table><tr><th>Candidate</th><th>Proportion Elected</th><th>Mean position</th></tr>"+candidates.map { _.toTableRow }.mkString("")+"</table>"
       setText(running+voteDataInfo.getOrElse("")+"<p>"+heading+"</p><p>"+margin+"</p>"+table+textDisclaimer);
    }
  }
  
  def runElection(numCandidatesToElect:Int,numToRun:Int) {
    val data = voteData.get
    setText(s"Runnning election of numCandidatesToElect people $numToRun times"+voteDataInfo.get+textDisclaimer)
    val numThreads = 4
    SwingUtil.inSwingThread{goButton.setEnabled(false);fileChooser.setEnabled(false)}
    SwingUtil.inOTherThread{ProbabilisticWork.runProbabilisticly(numToRun, numThreads, data,true,numCandidatesToElect,NSWLocalGovernmentElectionRules,Some(output),Set.empty,None)}  // TODO add support for excluding mayor and choosing redistributable vote rules
  }
  def choseFile(file:File) {
    SwingUtil.inSwingThread(fileName.setText(file.getName))
    try {
      val data = NSWLocal2012IO.loadRaw(file)
      voteData = Some(data)
      val fileSummary = 
      setText(voteDataInfo.get+textHowToGo+textDisclaimer)
      SwingUtil.inSwingThread(goButton.setEnabled(true))
    } catch { case e:Exception =>
      voteData = None
      setText(textFileFail+textChooseFile+textDisclaimer)
      SwingUtil.inSwingThread(goButton.setEnabled(false))
    }
  }

  @volatile var voteData : Option[ElectionData] = None
  def voteDataInfo : Option[String] = for (data<-voteData) yield "<p><b>Successfully loaded data for "+data.name+"</b>. "+data.candidates.length+" candidates and "+data.totalFormalVotes+" formal votes.</p>" 
  
  def setText(text:String) { SwingUtil.inSwingThread(resultsPane.setText(text)) }
  
}