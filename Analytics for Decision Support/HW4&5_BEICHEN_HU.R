### Cancer Moonshot ###
#######################
mydata <- read.csv("Cancer Data12A.csv")#read the data
str(mydata)#structure the data

### Cancer Moonshot ###
#######################
mydata <- read.csv("Cancer Data12A.csv")
str(mydata)

# extracting the year from the date of patent filing
# other methods are possible, such as using package lubridate
yt <- strptime(mydata$Filing_Date, format ='%Y%m%d') 
# Exchange the Filing_Date from int into time type in specific format
# In this case, it transport the int "19971027" into "1997-10-27 CST"
mydata$year <- as.integer(format(yt,"%Y"))
# After return the format from time type into int, 
# but this time, we just pick the year up and throw the month and date away.
# and exchange the new column "year" in mydata set at first. 

### Preparing the Document Term Matrix for clustering
# loading and installing the TM package for text mining
#install.packages("NPL")
#install.packages("tm") 
library(NPL)
library(tm) #tm means text mining, a basic package of processing the nature language.
# It provides some basic infrastructures such as corpus data input, 
# processing, preprocessing, metadata management, create a word - text matrix

### creating the document term matrix (DTM) on the titles of the patent
mydatatitle <-mydata$Patent_Title
ndocs <-length(mydatatitle) # Show the total length of char appearing in Patent_title column [1] 269353
#setting the parameters below helps keep the DTM of reasonable size
minf=ndocs*0.01 # minimum occurrence of words to be used in the DTM
#minf 2693.53
maxf=ndocs*0.40 # maximum occurrence of words to be used in the DTM
#maxf 107741.2

# putting the input data in a format the main function for DTM can read
corpus=Corpus(VectorSource(mydatatitle))
# Corpus means the collection of a set of document, usually a file is a document.
# And multiple documents consists of a corpus. 
# The way to create is using VCorpus(x, readerControl)
# The first parameter "x" must be a Source object, and "tm" package provides some predefine,
# such as DirSource, VectorSource and DataframeSource and so on. 
# They respectly used for processing a directory, a vector(each element is a document) and the data frame structure (such as csv)
# It can use getSources() to get all of the avaliable source, or users can also set up their own source. 
# The second parameter is readerControl. And this parameter must be a list, which contains components "reader" and "language".
# "reader" represents create a documents and "language" means text language.
# The most simple is VectorSource which is used to simple test. 

# main command line to create the DTM with parameters listed in control
dtm = DocumentTermMatrix(corpus,control=list(stopwords=TRUE, wordLengths=c(4,25),removePunctuation = FALSE,removeNumbers = FALSE, bounds = list(global=c(minf,maxf))))
#Description

#Constructs or coerces to a term-document matrix or a document-term matrix.

#Usage

#TermDocumentMatrix(x, control = list())
#DocumentTermMatrix(x, control = list())
#as.TermDocumentMatrix(x, ...)
#as.DocumentTermMatrix(x, ...)
# "x"	means a corpus for the constructors and either a term-document matrix or a document-term matrix or a simple triplet matrix (package slam) or a term frequency vector for the coercing functions.

# "control" means a named list of control options. There are local options which are evaluated for each document and global options which are evaluated once for the constructed matrix. 
# Available local options are documented in termFreq and are internally delegated to a termFreq call.
# This is different for a SimpleCorpus. In this case all options are processed in a fixed order in one pass to improve performance. It always uses the Boost (http://www.boost.org) Tokenizer (via Rcpp) and takes no custom functions as option arguments.

# "stopwords" means either a Boolean value indicating stopword removal using default language specific stopword lists shipped with this package, a character vector holding custom stopwords, or a custom function for stopword removal. Defaults to FALSE.
# "wordLengths" means an integer vector of length 2. Words shorter than the minimum word length wordLengths[1] or longer than the maximum word length wordLengths[2] are discarded. Defaults to c(3, Inf), i.e., a minimum word length of 3 characters.
# "removePunctuation" means a logical value indicating whether punctuation characters should be removed from doc, a custom function which performs punctuation removal, or a list of arguments for removePunctuation. Defaults to FALSE.
# "removeNumbers" means a logical value indicating whether numbers should be removed from doc or a custom function for number removal. Defaults to FALSE.
# "bounds" means a list with a tag local whose value must be an integer vector of length 2. Terms that appear less often in doc than the lower bound bounds$local[1] or more often than the upper bound bounds$local[2] are discarded. Defaults to list(local = c(1, Inf)) (i.e., every token will be used).



#display info on the DTM
inspect(dtm)

# outputting the DTM to a CSV file if we want
dtmm=as.matrix(dtm)
write.csv(dtmm,"cancerdtm.csv")

# exploring the data
# finding the most frequent words in the DTM matrix
FreqMat <- data.frame(ST = colnames(dtmm), Freq = colSums(dtmm))
# Create a data frames and column name is ST and sum the column in Frequent
y <- as.vector(order(-FreqMat$Freq))
# re-order the frequenceof FreqMat$Freq in descending order
FreqMat[y,]
head10 = head(FreqMat[y,],n=10)
# with n=10 we get a lot of useless words like "method" and "methods"
head30 = head(FreqMat[y,],n=30)
# with n=30 we get a lot of useless words like "methods" and other words

# adding the DTMs to the rest of the data frame and outputting that
bigdata <- cbind(mydata$Family_ID,mydata$Patent_or_Publication_ID,as.data.frame(as.matrix(dtm)),make.row.names=TRUE)

# if you want to output your big data frame to a csv file 
write.csv(bigdata,"cancerbigdtm.csv")

# now we can start clustering using the DTM!

### K-means clustering
# for question (2)
# nc is the number of clusters you want to have
#install.packages("caret") #say yes to restarting R 
library(caret)
set.seed(77)
nc <- 10

kmc.cancer= kmeans(dtmm, centers = nc)
table(kmc.cancer$cluster)
kmc.cancer$centers
  # FINISH THIS LINE OF CODE TO DO K-MEANS CLUSTERING WITH nc CLUSTERS

# below I provide a code that gives you the most frequent words per cluster
# I made a loop so that it's not too painful to do for many clusters
nb = 7 # most common words
topwords <-matrix(0,nrow=nc,ncol=nb)
for (i in 1:nc)
{
  clusterdata = subset(as.data.frame(dtmm), kmc.cancer$cluster == i)
  vv <- as.data.frame(tail(sort(colMeans(clusterdata)), n=nb))
  topwords[i,]<- rownames(vv)
}
topwords
#thereof growth diagnosis compounds human related therapy 

nb2 = 14 # most common words
topwords2 <-matrix(0,nrow=nc,ncol=nb2)
for (i in 1:nc)
{
  clusterdata = subset(as.data.frame(dtmm), kmc.cancer$cluster == i)
  vv <- as.data.frame(tail(sort(colMeans(clusterdata)), n=nb2))
  topwords2[i,]<- rownames(vv)
}
topwords2

#Q3
#install.packages("cluster")
#install.packages("factoextra")
library(cluster)
library(factoextra)
set.seed(77)
kmax<-20

wss <- (nrow(dtmm)-1)*sum(apply(dtmm,2,var))
for (i in 2:kmax) wss[i] <- sum(kmeans(dtmm,
                                       centers=i, nstart=5)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
#nc=7

nc31=7
kmc.cancer= kmeans(dtmm, centers = nc31)
table(kmc.cancer$cluster)
nb3 = 15 
topwords31 <-matrix(0,nrow=nc31,ncol=nb3)
for (i in 1:nc31)
{
  clusterdata = subset(as.data.frame(dtmm), kmc.cancer$cluster == i)
  vv <- as.data.frame(tail(sort(colMeans(clusterdata)), n=nb3))
  topwords31[i,]<- rownames(vv)
}
topwords31

set.seed(77)
nc32=10
kmc.cancer= kmeans(dtmm, centers = nc32)
table(kmc.cancer$cluster)
nb3 = 20 
topwords32 <-matrix(0,nrow=nc32,ncol=nb3)
for (i in 1:nc32)
{
  clusterdata = subset(as.data.frame(dtmm), kmc.cancer$cluster == i)
  vv <- as.data.frame(tail(sort(colMeans(clusterdata)), n=nb3))
  topwords32[i,]<- rownames(vv)
}
topwords32

set.seed(77)
nc33=15
kmc.cancer= kmeans(dtmm, centers = nc33)
table(kmc.cancer$cluster)
nb3 = 20
topwords33 <-matrix(0,nrow=nc33,ncol=nb3)
for (i in 1:nc33)
{
  clusterdata = subset(as.data.frame(dtmm), kmc.cancer$cluster == i)
  vv <- as.data.frame(tail(sort(colMeans(clusterdata)), n=nb3))
  topwords33[i,]<- rownames(vv)
}
topwords33
#######change nc to refine until get a good cluster

### for question (4)
# below we identify all the rows where FDA_Applicant contains "NOVARTIS"
indicesnovartis=grep("NOVARTIS",mydata$FDA_Applicant)
mydatanova=mydata[indicesnovartis,]
mydatanovartis=subset(mydatanova,mydatanova$year>=1999)
str(mydatanovartis)
mydatanovartis$Family_ID
mydatanovartis$FDA_Applicant

mydatatitle2 <-mydatanovartis$Patent_Title
ndocs2 <-length(mydatatitle2)
ndocs2

# putting the input data in a format the main function for DTM can read
corpus2=Corpus(VectorSource(mydatatitle2))

#setting the parameters below helps keep the DTM of reasonable size
minf2=ndocs2*0.01 # minimum occurrence of words to be used in the DTM

maxf2=ndocs2*0.40 # maximum occurrence of words to be used in the DTM

# main command line to create the DTM with parameters listed in control
dtm2 = DocumentTermMatrix(corpus2,control=list(stopwords=TRUE, wordLengths=c(4,25),removePunctuation = FALSE,removeNumbers = FALSE,bounds = list(global=c(minf2,maxf2))))
inspect(dtm2)
dtmm2=as.matrix(dtm2)
#install.packages("cluster")
#install.packages("factoextra")
library(cluster)
library(factoextra)
set.seed(77)
kmax<-20
fviz_nbclust(dtmm2, FUN = kmeans, method = "wss", k.max=kmax, nstart=4)
fviz_nbclust(dtmm2, FUN = hcut, method = "silhouette", k.max=kmax)
#nc=10

set.seed(77)
nc41 <- 10
kmc.FDA= kmeans(dtmm2, centers = nc41)
table(kmc.FDA$cluster)
nb4 = 15 # most common words
topwords41 <-matrix(0,nrow=nc41,ncol=nb4)
for (i in 1:nc41)
{
  clusterdata = subset(as.data.frame(dtmm2), kmc.FDA$cluster == i)
  vv <- as.data.frame(tail(sort(colMeans(clusterdata)), n=nb4))
  topwords41[i,]<- rownames(vv)
}
topwords41

##nc=6
set.seed(77)
nc42 <- 6
kmc.FDA= kmeans(dtmm2, centers = nc42)
table(kmc.FDA$cluster)
topwords42 <-matrix(0,nrow=nc42,ncol=nb4)
for (i in 1:nc42)
{
  clusterdata = subset(as.data.frame(dtmm2), kmc.FDA$cluster == i)
  vv <- as.data.frame(tail(sort(colMeans(clusterdata)), n=nb4))
  topwords42[i,]<- rownames(vv)
}
topwords42


####Q5
mydata2011=subset(mydata,mydata$year>=2011)
str(mydata2011)

mydatatitle5 <-mydata2011$Patent_Title
ndocs5 <-length(mydatatitle5)
ndocs5
minf5=ndocs5*0.01 # minimum occurrence of words to be used in the DTM
maxf5=ndocs5*0.40 # maximum occurrence of words to be used in the DTM
corpus5=Corpus(VectorSource(mydatatitle5))

dtm5 = DocumentTermMatrix(corpus5,control=list(stopwords=TRUE, wordLengths=c(4,25),removePunctuation = FALSE,removeNumbers = FALSE, bounds = list(global=c(minf5,maxf5))))

#display info on the DTM
inspect(dtm5)
dtmm5=as.matrix(dtm5)

set.seed(77)
kmax<-20
wss <- (nrow(dtmm5)-1)*sum(apply(dtmm5,2,var))
for (i in 2:kmax) wss[i] <- sum(kmeans(dtmm5,
                                       centers=i, nstart=5)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

##############get a nc
nc5 <- 6
kmc.2011= kmeans(dtmm5, centers = nc5)
table(kmc.2011$cluster)

nb5 = 14
topwords5 <-matrix(0,nrow=nc5,ncol=nb5)
for (i in 1:nc5)
{
  clusterdata = subset(as.data.frame(dtmm5), kmc.2011$cluster == i)
  vv <- as.data.frame(tail(sort(colMeans(clusterdata)), n=nb5))
  topwords5[i,]<- rownames(vv)
}
topwords5


####Q6
mydata1999=subset(mydata,mydata$year>=1999)
str(mydata1999)
mydata1<-mydata1999[which(mydata1999$FDA_Applicant!=""),]
mydata1$FDA_Applicant


mydataCPC <-mydata1$CPC_Inventive
ndocs61 <-length(mydataCPC)
ndocs61
minf6=ndocs61*0.01 
maxf6=ndocs61*0.40 
corpus6=Corpus(VectorSource(mydataCPC))
dtmCPC = DocumentTermMatrix(corpus6,control=list(stopwords=TRUE, wordLengths=c(4,25),removePunctuation = FALSE,removeNumbers = FALSE, bounds = list(global=c(minf6,maxf6))))
inspect(dtmCPC)


mydataFDA <-mydata1$FDA_Applicant
ndocs62 <-length(mydataFDA)
ndocs62
minf7=ndocs62*0.01 
maxf7=ndocs62*0.40 
corpus7=Corpus(VectorSource(mydataFDA))
dtmFDA = DocumentTermMatrix(corpus7,control=list(stopwords=TRUE, wordLengths=c(4,25),removePunctuation = FALSE,removeNumbers = FALSE, bounds = list(global=c(minf7,maxf7))))
inspect(dtmFDA)

bigdata <- cbind(as.data.frame(as.matrix(dtmCPC)),as.data.frame(as.matrix(dtmFDA)))
dtmmBigdata=as.matrix(bigdata)
str(dtmmBigdata)
summary(dtmmBigdata)
write.csv(dtmmBigdata,"bigdata.csv")

set.seed(77)
kmax<-20
wss <- (nrow(dtmmBigdata)-1)*sum(apply(dtmmBigdata,2,var))
for (i in 2:kmax) wss[i] <- sum(kmeans(dtmmBigdata,
                                       centers=i, nstart=5)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

##############get a nc
set.seed(77)
nc6 <- 8
kmc.Bigdata= kmeans(dtmmBigdata, centers = nc6)
table(kmc.Bigdata$cluster)

nb6 = 15 
topwords6 <-matrix(0,nrow=nc6,ncol=nb6)
for (i in 1:nc6)
{
  clusterdata = subset(as.data.frame(dtmmBigdata), kmc.Bigdata$cluster == i)
  vv <- as.data.frame(tail(sort(colMeans(clusterdata)), n=nb6))
  topwords6[i,]<- rownames(vv)
}
topwords6

#change nc to 5 and nb to 10
set.seed(77)
nc7 <-5
kmc.Bigdata= kmeans(dtmmBigdata, centers = nc7)
table(kmc.Bigdata$cluster)

nb7 = 10 
topwords7 <-matrix(0,nrow=nc7,ncol=nb7)
for (i in 1:nc7)
{
  clusterdata = subset(as.data.frame(dtmmBigdata), kmc.Bigdata$cluster == i)
  vv <- as.data.frame(tail(sort(colMeans(clusterdata)), n=nb7))
  topwords7[i,]<- rownames(vv)
}
topwords7

