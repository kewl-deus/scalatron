package routing

import framework.XY

object PathCompressor {
  def apply(path: Seq[XY]): Seq[XY] = compressPath(path)


  def findSkipIndex(path: Seq[XY]): Int = path.scanLeft(XY.Zero)(_+_).lastIndexOf(XY.Zero)
  
  private def compressPath(path: Seq[XY]) = {
    val skipIndex = findSkipIndex(path) // path.scanLeft(XY.Zero)(_+_).lastIndexOf(XY.Zero)
    path.takeRight(path.size - skipIndex)
  }

  private def compressPathImperative(path: Seq[XY]) = {
    var point = XY.Zero
    var compressed: List[XY] = List()
    var tmpPath = path
    while(! tmpPath.isEmpty){
      point += tmpPath.head
      if (point == XY.Zero){
        compressed = List()
      } else {
        compressed :+= tmpPath.head
      }
      tmpPath = tmpPath.tail
    }
    compressed
  }
}
