library(ggplot2)
library(dplyr)
library(progress)
library(plotly)
source("Network.Measures.R")
source("Points.R")
source("Points.Util.R")

c1 = data.frame(x1 = c(1, 2, 3, 4, 5.5, 7, 9, 10, 0.5, 2, 3.7, 4.25, 5, 5.5, 5.9, 6.9, 6.9, 7.25, 8.25),
                x2 = c(1, 0.5, 1, 0.5, 1, 0.5, 0.5, 0.7, 3, 2, 4.5, 3.5, 4.5, 3.5, 2.4, 4.4, 3.1, 2, 1.25))

c2 = data.frame(x1 = c(1, 1, 1, 2.5, 2.45, 2.5, 3.5, 3.4, 4.4, 8.2, 8, 8.5, 9.9, 9.9, 10, 9.5),
                x2 = c(4.5, 3.4, 2.5, 4.5, 3.5, 3, 3, 2.1, 2, 4.4, 3.4, 2.25, 4.5, 3.5, 2.5, 1.25))

dataset = rbind(c1, c2)
dataset$Class = as.factor(c(rep(1, nrow(c1)), rep(2, nrow(c2))))

ggplot(dataset, aes(x = x1, y = x2, shape = Class)) +
    geom_point(size = 3) +
    xlim(0, 10) + ylim(0, 5)

M = as.matrix(dist(dataset[, -3]))

W = 1/M
W[W == Inf] = 0

classes.as.numbers = as.numeric(dataset$Class)

W.signs = classes.as.numbers %*% t(classes.as.numbers)

W.signs[W.signs == 4] = 1
W.signs[W.signs == 2] = -1

W = W * W.signs

dataset$degree = c(rep(1, nrow(W)) %*% W)
dataset$clust.coef = clustering.coefficient.neg(M * W.signs)
g = graph.adjacency(W, "undirected", weighted = TRUE)
dataset$page.rank = c(page.rank(g, directed = FALSE)$vector)

dataset$id = 1:nrow(dataset)

plot_ly(x=dataset$x1, y=dataset$x2, z=dataset$degree, type="scatter3d", mode="markers",
        color=dataset$degree, symbol=dataset$Class, symbols=c('circle', 'square'),
        text=paste("ID:",dataset$id))

ggplot(dataset, aes(x = x1, y = x2, shape = Class, color = degree)) +
    geom_point(size = 5) +
    theme_minimal() +
    scale_x_continuous(breaks=1:10) +
    scale_y_continuous(breaks=1:5) +
    scale_color_distiller(palette = "YlOrRd", direction = 1) +
    geom_text(aes(label=id), hjust=-0.5, vjust=0, size=5) +
    theme(text = element_text(size=20), axis.title=element_blank(), axis.text=element_blank()) +
    coord_fixed()

reference.points = dataset %>% arrange(clust.coef) %>% top_n(4) %>% select (x1, x2)

repeats = 50
accuracies = data.frame()
pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = 20)
for (noPoints in 1:20) {
    pb$tick()
    accuracy.cum = 0
    
    for (r in 1:repeats) {
        reference.points = data.frame(x1 = runif(noPoints, 0, 10),
                                      x2 = runif(noPoints, 0, 5))
        
        dataset.new = TransformDataset(reference.points, dataset[, 1:3], "Class")
        
        list[trainSet, testSet] <- splitDataset(dataset.new, 0.75)
        
        list[accuracy.train, accuracy.test] = trainTestEvaluate(trainSet, testSet, "svmLinear", 2, 5)   
        
        accuracies = rbind(accuracies, data.frame(train = accuracy.train, test = accuracy.test, points = noPoints, run = r))
    }
}

ggplot(accuracies) +
    geom_smooth(aes(x=points, y=test, color=method))

for (noPoints in 1:20) {
    for (r in 1:repeats) {
        reference.points = dataset %>% arrange(degree) %>% top_n(noPoints) %>% select (x1, x2)
        
        dataset.new = TransformDataset(reference.points, dataset[, 1:3], "Class")
        
        list[trainSet, testSet] <- splitDataset(dataset.new, 0.75)
        
        list[accuracy.train, accuracy.test] = trainTestEvaluate(trainSet, testSet, "svmLinear", 2, 5)   
        
        accuracies = rbind(accuracies, data.frame(train = accuracy.train, test = accuracy.test, points = noPoints, run = r, method = "clust.coef"))
    }
}

## Toy example
angle = 45
theta = angle * pi / 180

tmpD = data.frame(x1 = c(1, 3, 6), x2 = c(1, 2, 1))
tmpM = as.matrix(dist(tmpD))
ggplot(tmpD, aes(x = x1, y = x2)) + geom_point() + xlim(0, 8) + ylim(0, 8)

p = 1
w = 3
x = 2

a = tmpM[p, w]
b = tmpM[p, x]
c = tmpM[w, x]

cosA = (a^2 + b^2 - c^2) / (2 * a * b)
cosB = (a^2 + c^2 - b^2) / (2 * a * c)

## Messy but working solution
peaks = c()

angle = 60
theta = angle * pi / 180

for (p in 1:nrow(dataset)) {
    notNN = c()
    
    for (w in 1:nrow(M)) {
        for (x in 1:nrow(M)) {
            if (x != w) {
                a = M[p, w]
                b = M[p, x]
                c = M[w, x]
                cosA = (a^2 + b^2 - c^2) / (2 * a * b)
                cosB = (a^2 + c^2 - b^2) / (2 * a * c)
    
                if (b < a & c < a & cosA > cos(theta) & cosB > cos(theta)) {
                    notNN = c(notNN, w)
                    break
                }
            }
        }
    }

    NN = setdiff(1:35, c(unique(notNN), p))

    if(all(dataset[p, ]$degree > dataset[NN, ]$degree)){
        peaks = c(peaks, p)
    }
}

accuracies = data.frame()
repeats = 10
reference.points = dataset[peaks, ] %>% select(x1, x2)
#reference.points = dataset %>% arrange(degree) %>% top_n(length(peaks), degree) %>% select (x1, x2)

for (r in 1:repeats) {
    dataset.new = TransformDataset(reference.points, dataset[, 1:3], "Class")
    
    list[trainSet, testSet] <- splitDataset(dataset.new, 0.75)
    
    list[accuracy.train, accuracy.test] = trainTestEvaluate(trainSet, testSet, "svmLinear", 2, 5)   
    
    accuracies = rbind(accuracies, data.frame(train = accuracy.train, test = accuracy.test, points = nrow(reference.points), run = r, method = "peak-degree"))
}

## A different idea

alpha = 0.7
x = seq(0, 10, by=0.01)
y = exp(x)^-alpha
plot(x, y)

new.peaks = sapply(1:nrow(dataset), function(p){dataset[p, ]$degree > (exp(M[p, -p])^-alpha) %*% dataset[-p, ]$degree})
dataset[new.peaks, ]$id
