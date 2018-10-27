#==============================================
#load these libraries
install.packages('tm')
require('tm')
require('SnowballC')

#data pre-processing
all <- Corpus(DirSource("D:/Sem 2/Temporal and spatial data/20news-18828", +
      encoding = "UTF-8", recursive=TRUE), +
      readerControl=list(reader=readPlain,language="en"))

all.p <- tm_map (all, removePunctuation)
all.p  <- tm_map (all.p , content_transformer(tolower))
all.p <- tm_map(all.p, content_transformer(removeWords),stopwords("english"))
all.p <- tm_map(all.p, content_transformer(removeWords),stopwords("SMART"))
all.p <- tm_map (all.p, stemDocument)
all.p <- tm_map (all.p, removeNumbers)
all.p <- tm_map (all.p, stripWhitespace)

#turn into Document-Term Matrix
dtm <- DocumentTermMatrix (all.p)

#remove sparse terms 
dtm <- removeSparseTerms(dtm, 0.99) 

#conversion to data frame
library(tidytext)
DF <- tidy(dtm)

#======================================================
#Feature Selection
#find top 100 most frequent words 
#convert document-term matrix to matrix
m <- as.matrix(dtm)    
#convert matrix to data frame
dataframe_m = tidy(m)     
#sort matrix by sum of frequencies
sortedMatrix <- sort(colSums(m), decreasing=TRUE)   
#select top 100 words from sorted matrix
sorted100 <- head(sortedMatrix, 100)   

#=======================================================
#find similarities
install.packages('proxy')
require('proxy')

#Sample 1000 docs
#-----------------
samplem <- head(as.matrix(dtm),1000)
dataframe_samplem = tidy(samplem)

#Cosine similarity
#-------------------
m_cosdis <- dist(samplem, method="cosine")

#jaccard similarity
#-------------------
m_jacdis <- dist(samplem, method="Jaccard")

#Eucleadian similarity
#---------------------
m_eucdis <- dist(samplem, method="euclidean")

#=========================================================
#plot frequency
install.packages('reshape2')
require('reshape2')
install.packages('ggplot2')
require('ggplot2')

dfplot <- as.data.frame(melt(sortedMatrix))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word,
                      levels=dfplot$word[order(dfplot$value,
                                               decreasing=TRUE)])
dfplot$word <- as.numeric(dfplot$word)
fig <- ggplot(dfplot, aes(x=word, y=value)) + 
      geom_bar(stat="identity", fill="grey", colour="darkred")
fig <- fig + xlab("Term Ranking")
fig <- fig + ylab("Term Frequency")
print(fig)

#=================================================================
#heatmap generation ggplot
ggplot(data = melt(as.matrix(m_cosdis)), aes(x=Var1, y=Var2, fill=value))+
  geom_tile() + ggtitle("Cosine Similarity Matrix") 

ggplot(data = melt(as.matrix(m_jacdis)), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+ ggtitle("Jaccard Similarity Matrix")

ggplot(data = melt(as.matrix(m_eucdis)), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+ ggtitle("Euclidean Similarity Matrix")

#========================================================
#Analyze the correlations (degree of similarity)
Cosine_Euclidean = cor(m_eucdis,m_cosdis,method = "pearson")
Euclidean_Jarcard = cor(m_jacdis,m_eucdis,method = "pearson")
Jarcard_Cosine = cor(m_cosdis,m_jacdis,method = "pearson")
  
#linear regression
cos_vector <- as.vector(head(m_cosdis,500))
euc_vector <- as.vector(head(m_eucdis,500))
jac_vector <- as.vector(head(m_jacdis,500))

fit1 <- lm(formula=cos_vector ~ euc_vector, data=dataframe_m)
fit2 <- lm(formula=euc_vector ~ jac_vector, data=dataframe_m)
fit3 <- lm(formula=jac_vector ~ cos_vector, data=dataframe_m)

scatter.smooth(x=euc_vector, y=cos_vector)
scatter.smooth(x=jac_vector, y=euc_vector)
scatter.smooth(x=cos_vector, y=jac_vector)

#=======================================================
#Standard Deviation
install.packages('plotly')
require('plotly')

data_sd<-as.matrix(m)
euc_sd<-vector()
cos_sd<-vector()
jac_sd<-vector()
i<-0
dp<-c(0,10,20,50,100,500)

for(j in dp)
{
  data_selected<- data_sd[500:1500,1:j]
  
  ed_temp<-dist(data_selected,method = "euclidean")
  cd_temp<-dist(data_selected,method = "cosine")
  jd_temp<-dist(data_selected,method = "jaccard")
  
  eudd<-sd(ed_temp,na.rm = FALSE)
  euc_sd<-c(euc_sd,eudd)
  
  coss<-sd(cd_temp,na.rm = FALSE)
  cos_sd<-c(cos_sd,coss)
  
  jacc<-sd(jd_temp,na.rm = FALSE)
  jac_sd<-c(jac_sd,jacc)
  i<-i+1
}

plot(dp,euc_sd,type="l",ylim=c(-2,15), col="green",xlab = "Features",ylab = "Standard Deviation of Similarity Scores")
lines(dp,cos_sd,type="o",col="red",ylim=c(-2,15))
points(dp,jac_sd,col="blue",ylim=c(-2,15))


#=======================================================
#Rank article pairs

jac_melt <- melt(as.matrix(m_jacdis))
tail(jac_melt[order(jac_melt$value),])

euc_melt <- melt(as.matrix(m_eucdis))
tail(euc_melt[order(euc_melt$value),])

cos_melt <- melt(as.matrix(m_cosdis))
tail(cos_melt[order(cos_melt$value),])


