#this seed gives good results
set.seed(1554)

bank_data <- read.csv(file="bankdata.csv",head=TRUE,sep=",")

#First we make the nominal fields numeric, per the assignment. This is not really the right thing to do--
#the nominal fields should be handled as nominals, and not turned into numbers that are manipulated with
#no concern for the underlying meaning of the data. Nevertheless, I am following instructions.
#Professor Ravikumar also said to use 0 through 3 for region, even though this is clearly not proper,
#as this data is not ordinal. 

#we subtract 1 from each conversion because as.numeric() starts with 1 instead of 0
#I probably could have done this in one line.
#for sex, 0=FEMALE and 1=MALE
bank_data$sex = as.numeric(bank_data$sex) - 1
#for region 0=INNER_CITY 1=RURAL 2=SUBURBAN 3=TOWN
bank_data$region = as.numeric(bank_data$region) - 1
#for the YES/NO attributes, 0=NO 1=YES
bank_data$married = as.numeric(bank_data$married) - 1
bank_data$car = as.numeric(bank_data$car) - 1
bank_data$save_act = as.numeric(bank_data$save_act) - 1
bank_data$current_act = as.numeric(bank_data$current_act) - 1
bank_data$mortgage = as.numeric(bank_data$mortgage) - 1
bank_data$pep = as.numeric(bank_data$pep) - 1

#now we transform the necessary fields to range from 0 to 1, so that all fields are equally represented
#when calculating euclidean distance
bank_data$age = sapply(bank_data$age, function(x) (x-min(bank_data$age)) / (max(bank_data$age) - min(bank_data$age) ) )
bank_data$income = sapply(bank_data$income, function(x) (x-min(bank_data$income)) / (max(bank_data$income) - min(bank_data$income) ) )
bank_data$children = sapply(bank_data$children, function(x) (x-min(bank_data$children)) / (max(bank_data$children) - min(bank_data$children) ) )
bank_data$region = sapply(bank_data$region, function(x) (x-min(bank_data$region)) / (max(bank_data$region) - min(bank_data$region) ) )

#get two random centroids.
#Was easy to copy rows to keep the data frame formats matching. There is probably a better way to deal with this.
#I didn't want a random value that was necessarily in the data set,
#so after copying a random row I make all relevant attributes random

k1 = bank_data[sample(600,1),]
k2 = bank_data[sample(600,1),]
#set the row names (numbers) back to 1
row.names(k1) = NULL
row.names(k2) = NULL
#and lets give them cool names for printing later
k1$id = 'Cluster 1 Centroid'
k2$id = 'Cluster 2 Centroid'
#and now we set everything randomly between 0 and 1
k1[2:11] = sapply(k1[2:11], function(x) runif(1,0.0,1.0))
k2[2:11] = sapply(k2[2:11], function(x) runif(1,0.0,1.0))

#vectors of euclidean distances, note we leave out the ID and PEP attributes
k1dist = apply(bank_data[2:11],1, function(x) sqrt(sum((x - k1[2:11])^2)))
k2dist = apply(bank_data[2:11],1, function(x) sqrt(sum((x - k2[2:11])^2)))

#we compare the distances, adding a new column "cluster" to bank_data
#cluster contains the closer cluster (the lesser distance)
#note I use a logical evaluation which gives a 0 or 1. If it's true (meaning the point is further from k1)
#then the statement evaluates to 1, which becomes 2. Likewise, if it's false (meaing k2 is further)
#the statement evaluates to 0, which becomes 1.
bank_data$cluster = as.numeric(k1dist > k2dist) + 1

#we want "old" centroids, for comparing in the main kmeans loop. When the new centroids match the old centroids
#kmeans stops.
k1old = k1
k2old = k2

#compute new centroids for each cluster
k1[2:11] = apply(subset(bank_data[2:11],bank_data$cluster == 1),2, function(col) mean(col))
k2[2:11] = apply(subset(bank_data[2:11],bank_data$cluster == 2),2, function(col) mean(col))

#now we loop until the previous iteration's centroids match (meaning centroids haven't moved)
while (!all(k1==k1old) || !all(k2==k2old)) {
  #new centroids
  k1dist = apply(bank_data[2:11],1, function(x) sqrt(sum((x - k1[2:11])^2)))
  k2dist = apply(bank_data[2:11],1, function(x) sqrt(sum((x - k2[2:11])^2)))
  #assign points to a cluster
  bank_data$cluster = as.numeric(k1dist > k2dist) + 1
  #make new clusters
  k1old = k1
  k2old = k2
  k1[2:11] = apply(subset(bank_data[2:11],bank_data$cluster == 1),2, function(col) mean(col))
  k2[2:11] = apply(subset(bank_data[2:11],bank_data$cluster == 2),2, function(col) mean(col))
  
}

#Lets make a plot. These plots are not complete or proper, but just give a quick visual peek at the 
#differences between the two clusters.
c1 = subset(bank_data, bank_data$cluster == 1)
c2 = subset(bank_data, bank_data$cluster == 2)
plots = bank_data[1:3,] #Copy for chart labels
plots$id = NULL #get rid of columns we don't want plotted
plots$cluster = NULL

#yes, I realize the 2nd and 3rd means are already in the centroids. This code is relic from an aborted attempt
#to de-transform the code. It works for the untransformed data so I didn't change it.
plots$age = c(mean(bank_data$age),mean(c1$age),mean(c2$age))
plots$sex = c(mean(bank_data$sex),mean(c1$sex),mean(c2$sex))
plots$region = c(mean(bank_data$region),mean(c1$region),mean(c2$region))
plots$income = c(mean(bank_data$income),mean(c1$income),mean(c2$income))
plots$married = c(mean(bank_data$married),mean(c1$married),mean(c2$married))
plots$children = c(mean(bank_data$children),mean(c1$children),mean(c2$children))
plots$car = c(mean(bank_data$car),mean(c1$car),mean(c2$car))
plots$save_act = c(mean(bank_data$save_act),mean(c1$save_act),mean(c2$save_act))
plots$current_act = c(mean(bank_data$current_act),mean(c1$current_act),mean(c2$current_act))
plots$mortgage = c(mean(bank_data$mortgage),mean(c1$mortgage),mean(c2$mortgage))
plots$pep = c(mean(bank_data$pep),mean(c1$pep),mean(c2$pep))

barplot(as.matrix(plots), main="Mean Scaled Values of Features, All Customers vs. k=2 Clustering", ylab= "Mean, Scaled to Min = 0 Max = 1", beside=TRUE, col=rainbow(3))
legend("topleft", c("All Customers - 46% PEP","Cluster 1 - 36% PEP","Cluster 2 - 57% PEP"), cex=0.6,bty="n", fill=rainbow(3));

#final output, per the assignment
k1$pep=NULL
k2$pep=NULL
print(k1)
print(k2)
print(c("Mean PEP Cluster 1 = ", mean(subset(bank_data, bank_data$cluster == 1)$pep)))
print(c("Mean PEP Cluster 2 = ", mean(subset(bank_data, bank_data$cluster == 2)$pep)))