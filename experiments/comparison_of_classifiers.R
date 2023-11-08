source("Points.R")
source("Points.Util.R")
library(doParallel)
library(foreach)

datasets = c("wine", "breast-cancer-wisconsin", "yeast", "glass", "ecoli",
             "vowel-context", "iris", "pima-indians-diabetes", "sonar.all",
             "image-segmentation", "ionosphere", "statlog-vehicle",
             "optdigits", "statlog-satimage", "pendigits")

repeats = 30
trainTestSplit = 0.7

classifiers = c("knn", "svmRadial", "rf", "rpart")

set.seed(1)

cores = detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

results <- foreach(run=1:repeats, .combine=rbind, .packages=c("caret", "readMLData", "dplyr", "fields")) %dopar% {
  source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")
  source("Points.R")
  
  run.results = data.frame(train=c(), test=c(), method=c(), run=c(), dataset=c(), classifier=c(), noPoints=c())
  for (dataset.name in datasets) {
    dataset = getDataset(dataset.name, "UCI")
    dataset = preprocessData(dataset)
    
    list[trainSet, testSet] <- splitDataset(dataset, trainTestSplit)
    
    M = as.matrix(dist(trainSet %>% select(-Class)))
    
    reference.points = GenerateReferencePoints(dataset=trainSet, distanceMatrix=M, method="degree", angle=60)

    trainSet.new = TransformDataset(reference.points, trainSet, "Class")
    testSet.new = TransformDataset(reference.points, testSet, "Class")   
    
    for (classifier in classifiers) {
      list[accuracy.train, accuracy.test] = trainTestEvaluate(trainSet.new, testSet.new, classifier, 2, 5)
      
      resultsRow = data.frame(train=accuracy.train, test=accuracy.test, method="peakDegree", run=run, dataset=dataset.name, classifier=classifier, noPoints=nrow(reference.points))
      
      write.table(resultsRow, paste0("results/comparison_of_classifiers_dataset_", dataset.name, ".csv"), append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
      
      run.results = rbind(run.results, resultsRow)
    }
  }
  
  run.results
}
