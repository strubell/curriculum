/* Copyright 2019, Emmanouil Antonios Platanios. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

package org.platanios.curriculum

import org.platanios.curriculum.utilities.{newReader, newWriter, Histogram}

import better.files._

import scala.collection.mutable

/**
  * @author Emmanouil Antonios Platanios
  */
class SentenceScoreHistogram(val score: SentenceScore, val maxNumBins: Int) extends SummaryScore {
  protected var histogram: Histogram = Histogram(maxNumBins)

  protected var skippedSentences = 0

  override def name: String = {
    s"$score.$maxNumBins.bins.histogram"
  }

  override def requiredSentenceScores: Seq[SentenceScore] = {
    Seq(score)
  }

  override def processSentence(
      sentence: String,
      requiredValues: Seq[Float],
      requiredSummaries: Seq[SummaryScore]
  ): Unit = {
    val sentenceScore = score.processSentence(sentence, requiredValues, requiredSummaries)
    if (sentenceScore == -1.0){
      skippedSentences += 1
      return
    }
    histogram.insert(sentenceScore)
  }

  override def report(): String = s"$name report: skipped sentences: $skippedSentences"

  def cdfScore: SentenceScore = {
    val histogramScore = this
    new SentenceScore {
      override def name: String = {
        s"$histogramScore.cdf"
      }

      override def requiredSentenceScores: Seq[SentenceScore] = {
        Seq(score)
      }

      override def requiredSummaryScores: Seq[SummaryScore] = {
        Seq(histogramScore)
      }

      override def processSentence(
          sentence: String,
          requiredValues: Seq[Float],
          requiredSummaries: Seq[SummaryScore]
      ): Float = {
        val sentenceScore = requiredValues.head
        if (sentenceScore == -1.0)
          return sentenceScore
        histogramScore.histogram.cdf(sentenceScore).toFloat
      }
    }
  }

  override def resetState(): Unit = {
    histogram = Histogram(maxNumBins)
  }

  override def saveStateToFile(file: File): Unit = {
    val writer = newWriter(file)
    histogram.bins.foreach(bin => {
      writer.write(s"${bin.mean}\t${bin.numSamples}\n")
    })
    writer.flush()
    writer.close()
  }

  override def loadStateFromFile(file: File): Unit = {
    reset()
    val histogram = Histogram(maxNumBins)
    newReader(file).lines().toAutoClosedIterator.foreach(line => {
      val lineParts = line.split('\t')
      histogram.insertBin(Histogram.Bin(
        mean = lineParts(0).toDouble,
        numSamples = lineParts(1).toLong))
    })
    this.histogram = histogram
  }
}

object SentenceScoreHistogram {
  def apply(score: SentenceScore, maxNumBins: Int): SentenceScoreHistogram = {
    new SentenceScoreHistogram(score, maxNumBins)
  }
}
