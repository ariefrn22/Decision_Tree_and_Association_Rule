#import Data
rawdata2 <- read.csv("GroceryStoreDataSet.csv")
View(rawdata2)

# transformasi Data
list <- c()
for(i in 1:nrow(rawdata2)) {
  c <- unlist(strsplit(rawdata2[i,1],","))
  list <- append(list, c)
}
list <- unique(list)
list
m <- matrix(NA, nrow=nrow(rawdata2), ncol=length(list))
for(j in 1:length(list)) {
  for (i in 1:nrow(rawdata2)) {
      m[i,j] <- grepl(list[j], rawdata2[i,], fixed = TRUE)
  }
}

data <- as.data.frame(m)
colnames(data) <- list

# Data Hasil Transformasi
View(data)


#acosiation rule
library(arules)
library(arulesViz)
rules.all<-apriori(data)
rules.all
inspect(rules.all)

rules <- apriori(data, parameter = list(support = 0.009, confidence = 0.25, minlen = 2))
quality(rules) <- round(quality(rules), digits=3)


#menghilangkan reduntdancy
redundant <- is.redundant(rules.sorted)
which(redundant)

rules.pruned<-rules.sorted[!redundant]
inspect(rules.pruned)

#lift 
rules.pruned <- head(rules.pruned, 5, by = "lift")
inspect(rules.pruned)

#visualisasi
dev.new(width=500, height=500)
plot(rules, measure=c("support", "confidence"), shading="lift", interactive=FALSE)

