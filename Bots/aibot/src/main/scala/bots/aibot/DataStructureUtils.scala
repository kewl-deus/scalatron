package bots.aibot

import org.nd4j.linalg.factory.Nd4j

object DataStructureUtils {

  type State = Seq[Double]

  /**
    * Reshape state to 2-dimensional matrix: [[1,2,3]]
    * @param state
    * @return
    */
  def reshape(state: State) = Nd4j.create(state.toArray, Array(1, state.length)) //rows, columns
}
