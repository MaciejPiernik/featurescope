source("Points.R")
source("Points.Util.R")
library(doParallel)
library(foreach)

datasets = c("wine", "breast-cancer-wisconsin", "yeast", "glass", "ecoli",
             "vowel-context", "iris", "pima-indians-diabetes", "sonar.all",
             "image-segmentation", "ionosphere", "spectrometer", "statlog-vehicle",
             "optdigits", "statlog-satimage", "pendigits")

repeats = 30
trainTestSplit = 0.7

set.seed(2)

centrality_results = read.csv("results/centrality_comparison.csv", sep=" ")

#cores = detectCores()
#cl <- makeCluster(cores[1]-1)
#registerDoParallel(cl)

pb <- txtProgressBar(min = 0, max = repeats*length(datasets), style = 3)

#results <- foreach(run=1:repeats, .combine=rbind, .packages=c("caret", "readMLData", "dplyr", "fields")) %dopar% {
for (run in 1:repeats) {
    #source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")
    
    run.results = data.frame(train=c(), test=c(), method=c(), run=c(), dataset=c(), classifier=c(), noPoints=c())
    for (i in 1:length(datasets)) {
        dataset.name = datasets[i]
                
        setTxtProgressBar(pb, (run-1)*length(datasets) + i)
        
        dataset = getDataset(dataset.name, "UCI")
        dataset = preprocessData(dataset)
        
        list[trainSet, testSet] <- splitDataset(dataset, trainTestSplit)
        
        noPoints = centrality_results[centrality_results$method == "peakDegree" & centrality_results$dataset == dataset.name & centrality_results$run == run, ]$noPoints
        
        reference.points = GenerateReferencePoints(dataset=trainSet, distanceMatrix=NULL, method="randomExamples", noPoints=noPoints)
        
        trainSet.new = TransformDataset(reference.points, trainSet, "Class")
        testSet.new = TransformDataset(reference.points, testSet, "Class")   
        
        list[accuracy.train, accuracy.test] = trainTestEvaluate(trainSet.new, testSet.new, "svmLinear", 2, 5)
        
        resultsRow = data.frame(train=accuracy.train, test=accuracy.test, method="random", run=run, dataset=dataset.name, classifier="svmLinear", noPoints=noPoints)
        
        write.table(resultsRow, "results/random.csv", append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        run.results = rbind(run.results, resultsRow)
    }
    
    run.results
}
