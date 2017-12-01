package classifier

import scala.math.{pow, sqrt}

class KNNClassifier(N: Int) {
  private[this] var _trainFeats: Array[Array[Double]] = Array[Array[Double]]()
  private[this] var _trainLabels: Array[String] = Array[String]()

  private def trainFeats: Array[Array[Double]] = _trainFeats

  private def trainFeats_=(value: Array[Array[Double]]): Unit = {
    _trainFeats = value
  }

  private def trainLabels: Array[String] = _trainLabels

  private def trainLabels_=(value: Array[String]): Unit = {
    _trainLabels = value
  }

  def fit(X: Array[Array[Double]], y: Array[String]) = {
    trainLabels_=(y)
    trainFeats_=(X)
  }


  // classify a single point
  def classify(x: Array[Double]): String = {
    /**
      * classify a single sample by voting
      * param trainFeats:training samples features
      * param trainLabels:training samples labels
      * param numOfFeats: number of features
      * param x:sample to be classified
      * param N:number of neighbours which have voting right
      * return:category name (label)
      */
    def distance(x: Array[Double], y: Array[Double]): Double = sqrt(((x zip y) map { case (xi, yi) => pow(yi - xi, 2) }).sum)

    def quickSort(xs: Array[(Double, String)]): Array[(Double, String)] = {
      if (xs.length <= 1) xs
      else {
        val pivot = xs(xs.length / 2)._1
        Array.concat(quickSort(xs.filter(t => t._1 < pivot)), xs.filter(t => t._1 == pivot), quickSort(xs.filter(t => t._1 > pivot))
        )
      }
    }

    // calculate distances
    val dists = trainFeats map { feat => distance(feat, x) }
    // get neighbours
    val neighboursLabels = quickSort(dists zip trainLabels).slice(0, N - 1).map(x => x._2)
    // vote
    val labelNames = neighboursLabels.distinct
    val labelCounts = labelNames.map(labelName => (labelName, neighboursLabels.count(label => label == labelName)))
    labelCounts.maxBy({ case (_, count) => count })._1
  }

  // classify whole test set
  def predict(X: Array[Array[Double]]): Array[String] = X map classify


  def score(predictedLabels: Array[String], acutalLabels: Array[String]): Double = {
    (predictedLabels zip acutalLabels).count(t => t._1 == t._2) / (acutalLabels.length).toDouble
  }

}
