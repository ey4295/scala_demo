import scala.io.Source
import scala.util.Random

/**
  * knn
  * Created by xuqh
  * 2017/11/26
  * 下午12:30
  * description: implementation of k-nearest neighbour algorithm
  */
object knn {


  def classify(data: List[Array[Float]], numOfFeats: Int, x: Array[Float], N: Int, CATEGORY: Map[String, String]): Int = {
    /**
      * param data: training data
      * param numOfFeates: number of features
      * param x: data to be classified
      * param N: number of neighbours
      * param CATEGORY: category reference table
      * classify given x into category elected by its neighbours
      */
    def dot(x: Array[Float], y: Array[Float]) = (0 to x.length - 1).map(i => x(i) * y(i)).sum

    def vote(cate: Array[Float], cateRef: Map[String, String]): Int = {
      /**
        * vote for categories occurs most
        * param cate: category list occurred
        * param cateRef: category mapping table;used to get all possible catagories
        */
      var tmp1 = cateRef.values.map(labelRef => (labelRef.toInt, cate.count(label => label.toInt == labelRef.toInt)))
      var tmp2 = tmp1.maxBy { case (label: Int, count: Int) => count }
      var tmp3 = tmp2._1
      tmp3

    }

    def quickSort(xs: List[(Float, Int)]): List[(Float, Int)] = {
      /**
        * list to be sorted ;[(value,index)]
        */
      if (xs.length <= 1) xs
      else {
        var pivot = xs(xs.length / 2)._1
        quickSort(xs filter { t => t._1 < pivot }) ::: (xs filter { t => t._1 == pivot }) ::: quickSort(xs filter { t => t._1 > pivot })
      }

    }

    // get neighbours
    val dist = data.map(p => dot(p.slice(0, 3), x)).zipWithIndex
    val sorted_dist = quickSort(dist)
    val neighbours_index = sorted_dist.slice(0, N).map(p => p._2)
    // vote
    val neighboursCatagories = neighbours_index.map(index => data(index)(numOfFeats)).toArray
    vote(neighboursCatagories, CATEGORY)


  }

  def main(args: Array[String]): Unit = {
    //fetch data
    //clean data
    val numOfFeats = 4
    val testSize = 15
    val CATEGORY = Map("Iris-virginica" -> "0", "Iris-versicolor" -> "1", "Iris-setosa" -> "2")
    val input = Source.fromFile("dataset/iris.data").getLines()
      .map(line => {
        val array = line.split(",")
        array(numOfFeats) = CATEGORY(array(numOfFeats))
        array.map(p => p.toFloat)
      })
      .toArray
    //    var minAndMax=((0 to numOfFeats-1) map {col=>(rawInput.map(p=>p(col)).min,rawInput.map(p=>p(col)).max)}).toArray
    //todo normalization
//    val input = Array.concat(
//      (0 to numOfFeats - 1).map({
//        row =>
//          val rawInputT = rawInput.transpose
//          rawInputT(row).map(x => x / (rawInputT(row).max - rawInputT(row).min))
//      }).toArray
//      , {
//        val rawInputT = rawInput.transpose
//        rawInputT(numOfFeats)
//      }).transpose.toList
    //split data into test and training
    val testSampleIndex = Seq.fill(testSize)(Random.nextInt(input.length))
    val trainSampleIndex = (0 to input.length - 1) diff testSampleIndex
    val testSample = testSampleIndex.map(index => input(index)).toList
    val trainSample = trainSampleIndex.map(index => input(index)).toList

    //classify 1.calculate distance 2.quick sort 3 generate result
    for (sample <- testSample) {
      var myres = classify(trainSample, numOfFeats, sample.slice(0, 3), 3, CATEGORY)
      var trueRes = sample(4)
      println(s"my=$myres,true=$trueRes")

    }
    //todo calculate accuracy
  }
}


