#the run function imports the data and will create an ANN.
#percentTrain is what fraction of data you want to use to train your network.
#HLNodes is the number of hidden layer nodes.


run = function(percentTrain= 0.5, roundOff =T ,outputClosenessThreshold = 0.3 , HLNodes = 4 ) {
  #import the data set
  dataSet = read.csv("creditset.csv");
  
  #make sure the header is used as labels
  head(dataSet);
  
  #Find how many observations will be used to train
  numTrain = percentTrain*nrow(dataSet);
  
  #extract the Training Set and the testing set
  trainSet = dataSet[1:numTrain,];
  testSet = dataSet[(numTrain+1):nrow(dataSet),];
  
  #error checking
  if(nrow(trainSet)+nrow(testSet) != nrow(dataSet)  ){
    print("There was an error in divinding data");
    stop();
  }
  
  #building the magnificent neural network creature
  #The first parameter is the model <what you are tyring to predict> ~ <All your features seperated by + signs>
  NNet =  neuralnet(default10yr~ LTI + age, trainSet, 
                    hidden = HLNodes ,linear.output=F,
                    lifesign="minimal",threshold = 0.1);
  
  #Plot the network(the best part)
  plot(NNet);
  
  #From the testing set we consider the features 
  #our model was based on
  testSet_Features = subset(testSet,select=c("LTI","age"));
  
  #Using the testData compute output based on Neural network prediction
  NNresults = compute(NNet , testSet_Features );
  if(roundOff){
    NNresults$net.result = round(NNresults$net.result);
  }
  
  #Compile the output in a presentable form
  finalOutput = data.frame(Actual = testSet$default10yr,
                          Prediction = NNresults$net.result,
                          Matches = doesPredictionMatch(testSet$default10yr,NNresults$net.result,outputClosenessThreshold));
  row.names(finalOutput) = NULL;
  
  #Print the output along with the success percentage of the ANN
  #print(finalOutput);
  print(cat("Prediction Success Rate : ",countSuccessPercent(finalOutput$Matches)));
  
  return(finalOutput);
}

doesPredictionMatch = function( expected =NULL , predicted = NULL , threshold = 0.3){
  if(is.null(expected) || is.null(predicted)){
    print("Necessary arguments missing or null");
    stop();
  }
  
  results = rep(FALSE,length(expected));
  
  for(i in 1:length(expected)){
    if((!is.na(expected[i]))&&(!is.na(predicted[i]))){  
      if(abs(expected[i]-predicted[i])<threshold ){
        results[i] = TRUE;
      }
    }
      
  }
  return (results);
}

countSuccessPercent = function(input = NULL){
  count= 0;
  for(i in 1:length(input)){
    tmp =as.logical(input[i]); 
    if(is.logical(tmp) && tmp ==TRUE ){
      count=count+1;
    }
  }
  return ((count/length(input))*100);  
}