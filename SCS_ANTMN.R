# compile data
  
library(devtools)
install_github("cbail/textnets")
library(textnets)
library(readr)

library(tmcn)
library(dplyr)
library(data.table) 
library(lubridate)
library(devtools)
#install_github("bstewart/stm",dependencies=TRUE)

library(topicmodels) #used for topic model estimation
library(ldatuning) # used for K selection
library(quanteda) # for text handling and pre-processing
library(dplyr) # general utility
library(xlsx) #writing excel files # for installing this on ubuntu: https://www.r-bloggers.com/installing-rjava-on-ubuntu/
library(parallel) # used for parallel computing when running models


df03 <- read_csv("/Users/xingwenpeng/Desktop/0.8_2009-2012.csv")


df <- rbind(df03)
#write.csv(df,"/Users/xingwenpeng/Desktop/df.csv")
#length(df[df$year>=2014 & df$year<=2018,]$title)

df$source <- trimws(df$source) #trim leading and trailing whitespace
df$content <- trimws(df$content) #trim leading and trailing whitespace
df$id <- rownames(df) #assign row id
df$date <- ymd(df$date) #standardize timstamp 


#clean the source string
unique(df$source) #find unique news outlets for further cleaning 


df$source <- gsub(".*人民邮电报.*", "人民邮电报", df$source)
df$source <- gsub(".*京华时报.*", "京华时报", df$source)
df$source <- gsub(".*中国证券报.*", "中国证券报", df$source)
df$source <- gsub(".*人民日报.*", "人民日报", df$source)
df$source <- gsub(".*第一财经日报.*", "第一财经日报", df$source)
df$source <- gsub(".*京华时报.*", "京华时报", df$source)
df$source <- gsub(".*京华时报.*", "京华时报", df$source)
df$source <- gsub(".*华夏时报.*", "华夏时报", df$source)
df$source <- gsub("APP", "", df$source) #example: "搜狐新闻 APP" to "搜狐新闻"
df$source <- gsub("(数字报)", "", df$source) 
df$source <- gsub("东方城乡报()", "东方城乡报", df$source) 
df$source <- gsub("()", "", df$source,fixed=TRUE) 

df$source <- gsub(".*21cn.*", "21CN", df$source)
df$source <- gsub("21世纪经济报道", "21世纪网", df$source)
df$source <- gsub("21经济网", "21世纪网", df$source)
df$source <- gsub("21经济网", "21世纪网", df$source)
df$source <- gsub("FX168财经网", "FX168财经", df$source)
df$source <- gsub("i时代网", "I时代报", df$source)
df$source <- gsub("I时代报(数字报)", "I时代报", df$source)
df$source <- gsub(".*Techweb.*", "Techweb", df$source) #merge CCTV-related outlets
df$source <- gsub(".*UC头条.*", "UC头条", df$source) #merge CCTV-related outlets
df$source <- gsub(".*ZAKER.*", "ZAKER", df$source) #merge CCTV-related outlets
df$source <- gsub(".*一点资讯.*", "ZAKER", df$source) #merge CCTV-related outlets
df$source <- gsub(".*三秦都市报.*", "三秦都市报", df$source)
df$source <- gsub(".*上海人大.*", "other", df$source) 
df$source <- gsub(".*农商银行.*", "other", df$source) 
df$source <- gsub(".*司法行政.*", "other", df$source) 
df$source <- gsub(".*审计.*", "other", df$source) 
df$source <- gsub(".*人民政府.*", "other", df$source) 
df$source <- gsub(".*委员会.*", "other", df$source) 
df$source <- gsub(".*气象局.*", "other", df$source) 
df$source <- gsub(".*政法综治网.*", "other", df$source) 
df$source <- gsub(".*文明网.*", "other", df$source) 
df$source <- gsub("上海普陀", "other", df$source) 
df$source <- gsub("上海审计", "other", df$source) 
df$source <- gsub("上海杨浦", "other", df$source) 
df$source <- gsub("上海嘉定", "other", df$source) 
df$source <- gsub("上海金山", "other", df$source) 
df$source <- gsub("上海静安", "other", df$source) 
df$source <- gsub("上海黄浦", "other", df$source) 
df$source <- gsub("东营银行", "other", df$source) 
df$source <- gsub("中华会计网校", "other", df$source) 
df$source <- gsub("中华会计网校", "other", df$source) 




library(Rwordseg)

df$seg_content <- segmentCN(df$content,package = "jiebaR",nature = FALSE, useStopDic = TRUE,nosymbol = FALSE, returnType="tm")




###### ANTMN part

network_from_LDA<-function(LDAobject,deleted_topics=c(),topic_names=c(),save_filename="",topic_size=c()) {
  # Importing needed packages
  require(lsa) # for cosine similarity calculation
  require(dplyr) # general utility
  require(igraph) # for graph/network managment and output
  
  print("Importing model")
  
  # first extract the theta matrix form the topicmodel object
  theta<-LDAobject@gamma
  # adding names for culumns based on k
  colnames(theta)<-c(1:LDAfit@k)
  
  # claculate the adjacency matrix using cosine similarity on the theta matrix
  mycosine<-cosine(as.matrix(theta))
  colnames(mycosine)<-colnames(theta)
  rownames(mycosine)<-colnames(theta)
  
  # Convert to network - undirected, weighted, no diagonal
  
  print("Creating graph")
  
  topmodnet<-graph.adjacency(mycosine,mode="undirected",weighted=T,diag=F,add.colnames="label") # Assign colnames
  # add topicnames as name attribute of node - importend from prepare meta data in previous lines
  if (length(topic_names)>0) {
    print("Topic names added")
    V(topmodnet)$name<-topic_names
  } 
  # add sizes if passed to funciton
  if (length(topic_size)>0) {
    print("Topic sizes added")
    V(topmodnet)$topic_size<-topic_size
  }
  newg<-topmodnet
  
  # delete 'garbage' topics
  if (length(deleted_topics)>0) {
    print("Deleting requested topics")
    
    newg<-delete_vertices(topmodnet, deleted_topics)
  }
  
  # run community detection and attach as node attribute
  print("Calculating communities")
  
  mylouvain<-(cluster_louvain(newg)) 
  mywalktrap<-(cluster_walktrap(newg)) 
  myspinglass<-(cluster_spinglass(newg)) 
  myfastgreed<-(cluster_fast_greedy(newg)) 
  myeigen<-(cluster_leading_eigen(newg)) 
  
  V(newg)$louvain<-mylouvain$membership 
  V(newg)$walktrap<-mywalktrap$membership 
  V(newg)$spinglass<-myspinglass$membership 
  V(newg)$fastgreed<-myfastgreed$membership 
  V(newg)$eigen<-myeigen$membership 
  
  # if filename is passsed - saving object to graphml object. Can be opened with Gephi.
  if (nchar(save_filename)>0) {
    print("Writing graph")
    write.graph(newg,paste0(save_filename,".graphml"),format="graphml")
  }
  
  # graph is returned as object
  return(newg)
}
write.csv(df,"/Users/xingwenpeng/Desktop/df.csv")
# Data Prep


df$year <- year(df$date)
#unique(df$year)

data <- df[df$year>="0" & df$year<="2019",c("seg_content")]

#unique(data$year)

colnames(data) <- c("text")
data$index<-seq(1:nrow(data))

### removing extremely short documents.
removed_short<-subset(data,nchar(as.character(data$text))<100)
data2<-subset(data,!nchar(as.character(data$text))<100)

### removing duplicate documents
removed_df<-data2[duplicated(data2$text),]
data3 <- data2[!duplicated(data2$text),]

### Text pre-processing
##### import data to quanteda format
mycorpus <- corpus(data3)

##### using quanteda stopwords, with single letters as well
stopwords_and_single<-c(stopwords(language = "zh", source = "misc"),LETTERS,letters)
##### preparing dfm obeject. No stemming due to its impact on topic quality
dfm_counts <- dfm(mycorpus,tolower = TRUE, remove_punct = TRUE,remove_numbers=TRUE, 
                  remove = stopwords_and_single,stem = FALSE,
                  remove_separators=TRUE) 
##### trimming tokens too common or too rare to imporve efficiency of modeling
dfm_counts2<-dfm_trim(dfm_counts, max_docfreq = 0.99, min_docfreq=0.005,docfreq_type="prop")
##### converting to LDA ready object
dtm_lda <- convert(dfm_counts2, to = "topicmodels")

## Selecting the appropriate number of topics
result <- FindTopicsNumber(
  dtm_lda,
  topics = c(1:10 * 10, 1:4 * 20 + 100, 0:2 * 50 + 200, seq(from = 80, to = 100, by = 1)),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  mc.cores = detectCores(),
  verbose = TRUE
) 

FindTopicsNumber_plot(result) # Based on the plot, 66 seems the most efficent model. 
  
result_refined <- FindTopicsNumber(
  dtm_lda,
  topics = c(70:120),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  mc.cores = detectCores(),
  verbose = TRUE
)

    FindTopicsNumber_plot(result_refined)

# running the model
LDA.80<- LDA(dtm_lda, k=96, method = "Gibbs")

###########################



#mynewnet<-network_from_LDA(LDAobject=LDAfit,deleted_topics=c(5,6,11,12,20,27,37),topic_names=mynames,save_filename="trythis",topic_size = topic.proportion)

#######################
# extracting excel matrices for topic interpretation
LDAfit<-LDA.80

mybeta<-data.frame(LDAfit@beta)
colnames(mybeta)<-LDAfit@terms
mybeta<-t(mybeta)
colnames(mybeta)<-seq(1:ncol(mybeta))
mybeta=exp(mybeta)

dfm_forsize<-data.frame(dfm_counts2)
dfm_forsize<-dfm_forsize[,-1]
sizevect<-rowSums(dfm_forsize)
meta_theta_df<-data.frame(size=sizevect,LDAfit@gamma)

topic.frequency <- colSums(meta_theta_df[,2:ncol(meta_theta_df)]*as.vector(meta_theta_df[,1]))
topic.proportion <- topic.frequency/sum(topic.frequency)



### First we print top 50 words
nwords=200
topwords <- mybeta[1:nwords,]
for (i in 1:LDAfit@k) {
  tempframe <- mybeta[order(-mybeta[,i]),]
  tempframe <- tempframe[1:nwords,]
  tempvec<-as.vector(rownames(tempframe))
  topwords[,i]<-tempvec
}
rownames(topwords)<-c(1:nwords)
write.csv(topwords, "TopWords_09-12-200.csv",fileEncoding="GBK")#80个主题前50个词


### Print top 30 documents
metadf<-data3
# notice that the "text" column is again named "text". If column name is different, name "text" needs to be changed.
meta_theta_df<-cbind(metadf[,"text"],LDAfit@gamma)
ntext=30
toptexts <- mybeta[1:ntext,]
for (i in 1:LDAfit@k) {
  print(i)
  tempframe <- meta_theta_df[order(-as.numeric(meta_theta_df[,i+1])),]
  tempframe <- tempframe[1:ntext,]
  tempvec<-as.vector(tempframe[,1])
  toptexts[,i]<-tempvec
}
rownames(toptexts)<-c(1:ntext)

write.csv(toptexts, "TopDocMents_09-12-30.csv",fileEncoding="GBK") #80个主题前30个文章
df<-toptexts
dfAfter1<- gsub(" ", "", df[1,])
dfAfter2<- gsub(" ", "", df[2,])
dfAfter3<- gsub(" ", "", df[3,])
dfAfter4<- gsub(" ", "", df[4,])
dfAfter5<- gsub(" ", "", df[5,])
dfAfter6<- gsub(" ", "", df[6,])
dfAfter7<- gsub(" ", "", df[7,])
dfAfter8<- gsub(" ", "", df[8,])
dfAfter9<- gsub(" ", "", df[9,])
dfAfter10<- gsub(" ", "", df[10,])
dfAfter11<- gsub(" ", "", df[11,])
dfAfter12<- gsub(" ", "", df[12,])
dfAfter13<- gsub(" ", "", df[13,])
dfAfter14<- gsub(" ", "", df[14,])
dfAfter15<- gsub(" ", "", df[15,])
dfAfter16<- gsub(" ", "", df[16,])
dfAfter17<- gsub(" ", "", df[17,])
dfAfter18<- gsub(" ", "", df[18,])
dfAfter19<- gsub(" ", "", df[19,])
dfAfter20<- gsub(" ", "", df[20,])
dfAfter21<- gsub(" ", "", df[21,])
dfAfter22<- gsub(" ", "", df[22,])
dfAfter23<- gsub(" ", "", df[23,])
dfAfter24<- gsub(" ", "", df[24,])
dfAfter25<- gsub(" ", "", df[25,])
dfAfter26<- gsub(" ", "", df[26,])
dfAfter27<- gsub(" ", "", df[27,])
dfAfter28<- gsub(" ", "", df[28,])
dfAfter29<- gsub(" ", "", df[29,])
dfAfter30<- gsub(" ", "", df[30,])
dfFinal<-t(cbind(dfAfter1,dfAfter2,dfAfter3,dfAfter4,dfAfter5,dfAfter6,dfAfter7,dfAfter8,dfAfter9,dfAfter10,dfAfter11,dfAfter12,dfAfter13,dfAfter14,dfAfter15,dfAfter16,dfAfter17,dfAfter18,dfAfter19,dfAfter20,dfAfter21,dfAfter22,dfAfter23,dfAfter24,dfAfter25,dfAfter26,dfAfter27,dfAfter28,dfAfter29,dfAfter30))
write.csv(dfFinal, "TopDocMents_09-12-30-changed.csv",fileEncoding="GBK")


### Extrating unique words for topic (FREX words)
mybeta<-data.frame(LDAfit@beta)
colnames(mybeta)<-LDAfit@terms
mybeta<-t(mybeta)
colnames(mybeta)<-seq(1:ncol(mybeta))
mybeta=exp(mybeta)


# change myw to change the weight given to uniqueness
myw=0.3
word_beta_sums<-rowSums(mybeta)
my_beta_for_frex<-mybeta
for (m in 1:ncol(my_beta_for_frex)) {
  for (n in 1:nrow(my_beta_for_frex)) {
    my_beta_for_frex[n,m]<-1/(myw/(my_beta_for_frex[n,m]/word_beta_sums[n])+((1-myw)/my_beta_for_frex[n,m]))
  }
  print (m)
}
nwords=200
topfrex <- my_beta_for_frex[1:nwords,]
for (i in 1:LDAfit@k) {
  tempframe <- my_beta_for_frex[order(-my_beta_for_frex[,i]),]
  tempframe <- tempframe[1:nwords,]
  tempvec<-as.vector(rownames(tempframe))
  topfrex[,i]<-tempvec
}
rownames(topfrex)<-c(1:nwords)
write.csv(topfrex, "TopFREXWords_200_k90_09-12.csv",fileEncoding="GBK")

### using the network from LDA function:
#mynewnet<-network_from_LDA(LDAobject=LDAfit,save_filename="trythis")
mynewnet<-network_from_LDA(LDAobject=LDAfit,save_filename="trythis",topic_size = topic.proportion)


typeof(mynewnet)

result2009 <- result
results_refined2009 <- result_refined 
LDAfit2009 <- LDAfit
topwords2009 <- topwords
toptexts2009 <- toptexts
topfrex2009 <- topfrex
k80_2009nx <- mynewnet

save(result2009,results_refined2009, LDAfit2009, topwords2009,toptexts2009, topfrex2009,k80_2009nx,file = "209-12-_output.rda")

ybeta<-data.frame(LDAfit@gamma)
write.csv(ybeta, "gamma.csv",fileEncoding="GBK")


