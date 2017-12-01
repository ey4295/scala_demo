import scala.io.Source
import scala.util.Random
import classifier.KNNClassifier
/**
 * iris_analysis
 * Created by xuqh
 * 2017/12/01
 * 下午2:46
 * description: classify iris data using knn
*/
object iris_analysis {
  def splitTrainTest(feats: Array[Array[Double]], labels: Array[String], testSize: Int): (Array[Array[Double]], Array[String], Array[Array[Double]], Array[String]) = {
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
  def main(args: Array[String]): Unit = {
    //fetch data
    //clean data
    val numOfFeats = 4
    val testSize = 15
    val N = 18
    // read data
    val input = Source.fromFile("dataset/iris.data").getLines()
      .map(
        line => {
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
    val (trainFeats, trainLabels, testFeats, testLabels) = splitTrainTest(feats, labels, testSize)

    // classification
    val knn=new KNNClassifier(N)
    knn.fit(trainFeats,trainLabels)
    val predictedLabels =knn.predict(testFeats)

    // evaluation
    val accuracy = knn.score(predictedLabels, testLabels)
    for ((pred, actual) <- predictedLabels.zip(testLabels)) println(s"predicted=$pred,   actual=$actual")
    println(s"Accuracy=$accuracy")
  }

}