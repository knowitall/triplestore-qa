package edu.knowitall.scoring.learning

class HiddenVariblePerceptron[Input, Hidden, Output] {

  
  type Model = HiddenVariableModel[Input, Hidden, Output]
  type Example = (Input, Output)
  
  private case class SummedModel(current: Model, summed: Model) {
    def this(model: Model) = this(model, model)
    def sum(next: Model): SummedModel = SummedModel(next, summed.sum(next))
  }
  
  private def trainIter(model: Model, input: Input, output: Output): Option[Model] =
    for (
    	 // Predict the hidden layer according to the given model
         hiddenPrediction <- model.predict(input);
         
         // Project the hidden layer down to make an output prediction
    	 outputPrediction = model.project(hiddenPrediction);
    	 
    	 // Check to see whether the predicted output is correct
    	 if !model.isCorrect(input, outputPrediction, output);
    	 
    	 // If the predicted output is incorrect, infer the best hidden layer
    	 // for the correct expected output.
    	 hiddenCorrect <- model.predictConstrained(input, output)
    	 )
      
      // Return an updated model, correcting for the difference between the
      // incorrect hidden layer and the (inferred) correct hidden layer.
      yield model.update(input, hiddenPrediction, hiddenCorrect)
      
  private def trainEpoch(model: SummedModel, data: List[Example]): SummedModel = data match {

      // Base case: if data is just an empty list, return the given model
      case Nil => model
      
      // Otherwise, run one training iteration, then train on the rest of the
      // data. 
      case (input, output) :: rest => {
        
        // Run on a single training example. This returns an optional new 
        // model, so default to the previous one if None is returned.
        val newModel = trainIter(model.current, input, output).getOrElse(model.current)
        trainEpoch(model.sum(newModel), rest)
        
      }
    }
  
  private def train(model: SummedModel, data: List[Example], numIters: Int): SummedModel = numIters match {
    case i if i > 0 => train(trainEpoch(model, data), data, numIters - 1)
    case _ => model
  }
  
  def trainAverage(model: Model, data: List[Example], numIters: Int): Model = (data.size * numIters) match {
    case 0 => model
    case _ => {
      val summedModel = new SummedModel(model)
      val result = train(summedModel, data, numIters)
      val denom = (data.size * numIters).toDouble
      result.summed.scale(1/denom)
    }
  }
  
  def train(model: Model, data: List[Example], numIters: Int): Model = train(new SummedModel(model), data, numIters).current
  
}