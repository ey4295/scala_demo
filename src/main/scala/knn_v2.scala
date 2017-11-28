import scala.io.Source
import scala.util.Random
import math._

/**
  * knn_v2
  * Created by xuqh
  * 2017/11/27
  * 下午6:05
  * description: implementation of knn
  */
object knn_v2 {

  def splitTrainTest(feats: Array[Array[Double]], labels: Array[String], testSize: Int) = {
    /** *
      * split train and test samples
      * param feats: features
      * param labels: feature labels
      * param testSize: number of test samples
      * return: 4 element tuple of (trainFeats,trainLabel,testFeats,testLabels)
      */
    val indices = Seq.fill(testSize)(Random.nextInt(labels.length - 1))
    val testFeats = (indices map { index => feats(index) }).toArray
    val testLabels = (indices map { index => labels(index) }).toArray
    val trainFeats = feats diff testFeats
    val trainLabel = labels diff testLabels
    (trainFeats, trainLabel, testFeats, testLabels)
  }

  def distance(x: Array[Double], y: Array[Double]): Double = sqrt(((x zip y) map { case (xi, yi) => pow(yi - xi, 2) }).sum)


  def quickSort(xs: Array[(Double, String)]): Array[(Double, String)] = {
    if (xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)._1
      Array.concat(quickSort(xs.filter(t => t._1 < pivot)), xs.filter(t => t._1 == pivot), quickSort(xs.filter(t => t._1 > pivot))
      )
    }
  }

  def classify(trainFeats: Array[Array[Double]], trainLabels: Array[String], numOfFeats: Int, x: Array[Double], N: Int) = {
    /**
      * classify a single sample by voting
      * param trainFeats:training samples features
      * param trainLabels:training samples labels
      * param numOfFeats: number of features
      * param x:sample to be classified
      * param N:number of neighbours which have voting right
      * return:category name (label)
      */

    // calculate distances
    val dists = trainFeats map { feat => distance(feat, x) }
    // get neighbours
    val neighboursLabels = quickSort(dists zip trainLabels).slice(0, N - 1).map(x => x._2)
    // vote
    val labelNames = neighboursLabels.distinct
    val labelCounts = labelNames.map(labelName => (labelName, neighboursLabels.count(label => label == labelName)))
    labelCounts.maxBy({ case (_, count) => count })._1
  }

  def eval(predictedLabels: Array[String], acutalLabels: Array[String]) = (predictedLabels zip acutalLabels).count(t => t._1 == t._2) / acutalLabels.length

  def main(args: Array[String]): Unit = {
    //fetch data
    //clean data
    val numOfFeats = 4
    val testSize = 15
    val N = 40
    // read data
    val input = Source.fromFile("dataset/iris.data").getLines()
      .map(line => {
        val arr = line.split(",")
        (arr.slice(0, numOfFeats), arr(numOfFeats))

      }).toArray
    val featsRaw = input map { p => p._1 }
    val labels = input map { p => p._2 }
    // normalization
    val featsRawT = featsRaw.transpose
    val feats = ((0 until numOfFeats) map {
      row => {
        val featT = featsRawT(row).map(x => x.toDouble)
        featT map { x => (x - featT.min) / (featT.max - featT.min) }
      }
    }).toArray.transpose
    val (trainFeats, trainLabel, testFeats, testLabels) = splitTrainTest(feats, labels, testSize)
    // classification
    val predictedLabels = testFeats map { feat => classify(trainFeats, trainLabel, numOfFeats, feat, N) }
    // evaluation
    val accuracy = eval(predictedLabels, testLabels)
    for ((pred, actual) <- predictedLabels.zip(testLabels)) println(s"predicted=$pred,   actual=$actual")
    println(s"Accuracy=$accuracy")
  }
}
