options(stringsAsFactors = FALSE)

Press <- tribble()

Press <- Press %>% 
  bind_rows(South_Florida_Sun_Sentinel, Telegraph_Herald, The_Desert_Sun, 
            The_Dily_Beast,The_Examiner, The_Journal_News, Arizona, 
            Charleston_Gazette_Mail, Los_Angeles_Times, El_Paso_Times, Local_Media_1,
            Local_Media_2, Chicago_Tribune, New_York_Post, New_York_Times,
            USA_Today, Wall_Street_Journal, Washington_Post)

Press.2 <- Press %>% 
  select(-X__1) %>% 
  select(-index)

Press <- Press.2

rm(AllArticles, Press.2, South_Florida_Sun_Sentinel, Telegraph_Herald, The_Desert_Sun, 
   The_Dily_Beast,The_Examiner, The_Journal_News, Arizona, 
   Charleston_Gazette_Mail, Los_Angeles_Times, El_Paso_Times, Local_Media_1,
   Local_Media_2, Chicago_Tribune, New_York_Post, New_York_Times,
   USA_Today, Wall_Street_Journal, Washington_Post)

Press$index <- 1:nrow(Press)

library(quanteda)

mycorpus <- corpus(Press)

stopwords_and_single <- c(stopwords("english"), LETTERS,letters)
dfm_press <- dfm(mycorpus,tolower = TRUE, remove_punct = TRUE,remove_numbers=TRUE, 
                 remove = stopwords_and_single,stem = FALSE,
                 remove_separators=TRUE) 

docnames(dfm_press) <- dfm_press@docvars$index

dfm_press2 <- dfm_trim(dfm_press, max_docfreq = 0.95, min_docfreq = 0.005, 
                       docfreq_type = "prop")

library(lubridate)

Press$date <- parse_date_time(Press$date, "mdy")

library(RNewsflow)

dfm_P <- delete.duplicates(dfm_press2, similarity = .95, 
                           keep = "first", tf.idf = FALSE)

dtm_lda <- convert(dfm_P, to = "topicmodels")

mycores<-detectCores()-1

full_data<-dtm_lda

n <- nrow(full_data)

candidate_alpha<- c(50)
candidate_k <- c(seq(1,10)*10)

for (eachalpha in candidate_alpha) { 
  print ("now running ALPHA:")
  print (eachalpha)
  print(Sys.time())
  cluster <- makeCluster(detectCores(logical = TRUE) - 1)
  registerDoParallel(cluster)
  
  clusterEvalQ(cluster, {
    library(topicmodels)
  })
  
  folds <- 5
  splitfolds <- sample(1:folds, n, replace = TRUE)
  clusterExport(cluster, c("full_data", "splitfolds", "folds", "candidate_k"))

  system.time({
    results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
      k <- candidate_k[j]
      print(k)
      results_1k <- matrix(0, nrow = folds, ncol = 2)
      colnames(results_1k) <- c("k", "perplexity")
      for(i in 1:folds){
        train_set <- full_data[splitfolds != i , ]
        valid_set <- full_data[splitfolds == i, ]
        
        fitted <- LDA(train_set, k = k, method = "Gibbs")
        
        results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
      }
      return(results_1k)
    }
  })
  stopCluster(cluster)
  
  results_df <- as.data.frame(results)
  results_df$myalpha<-as.character(eachalpha)
  MainresultDF<-rbind(MainresultDF,results_df)
}

MainresultDF$kalpha=paste0(as.character(MainresultDF$k),MainresultDF$myalpha) 
ggplot(MainresultDF) +geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))

MainresultDF<-MainresultDF[order(MainresultDF$k),]

cars.spl <- with(MainresultDF, smooth.spline(k, perplexity, df = 3))
with(cars, predict(cars.spl, x = MainresultDF$k, deriv = 2))

plot(with(cars, predict(cars.spl, x = MainresultDF$k, deriv = 2)), type = "l")

abline(v=35)

press_lda <- LDA(dtm_lda, 
                 k = 35, 
                 method = "Gibbs", 
                 control = list(verbose=500, 
                                seed = 9898, 
                                burnin = 1000,
                                keep = 50,
                                iter = 4000))

            

                 ####################################################################
                 ###############                                     ################
                 ###############          ANTMN Method               ################
                 ###############    by D. Walter & Y. Ophir (2019)   ################
                 ###############                                     ################
                 ####################################################################

save_filename <- "Caravana"

deleted_topics <- c(5, 10, 20)

topic_names <- c("Anti-immigrant Sentiments/White supremacy",
                 "Military/Security",
                 "Issues Midterm Elections",
                 "Abuse and Misstreatment at the Border",
                 "Boilerplate",
                 "Caravan Brings Diseases and is Dangerous",
                 "Clashes Between Migrants and CBP",
                 "Situations in the Caravan",
                 "Midterm Elections in Texas",
                 "Boilerplate",
                 "Caravan Continues Walking",
                 "Troops at the Border",
                 "Legislative Changes and Procedures to Ask for Asylum",
                 "Midterm Elections in Indiana and Tennessee",
                 "Midterm Elections in Florida and Georgia",
                 "US Citizens and Migrants Eating Together/People Helping the Caravan with Food",
                 "Trump's Political Use of the Caravan",
                 "Pittsburgh's Synagogue Shooting",
                 "Caravan in Mexico",
                 "Boilerplate",
                 "Middle Easterners in the Caravan",
                 "US Responsibility to Allow In People who Suffer in Other Countries",
                 "US National Security",
                 "Bundy's Support Caravan/Hurricane Willa",
                 "Situations at the Border",
                 "Shelters in Tijuana",
                 "Jim Acosta and Trump",
                 "Context Caravan",
                 "Facebook/Social Media Missinformation",
                 "Conservative Depictions of the Caravan",
                 "Aid to Stop Future Caravans",
                 "Fox and the Caravan",
                 "Midterm Elections",
                 "Partial Shutdown in Asylum Procedures",
                 "Trump's Attempt to End 14th Ammendment")

###Add Duplicates
docnamestoadd<-(setdiff(docnames(dfm_press2),docnames(dfm_P)))
deleted_dups<-dfm_press2[docnames(dfm_press2) %in% docnamestoadd,]
library(RNewsflow)
deleted_doc_sim<-documents.compare(deleted_dups,dfm_P)
deleted_doc_sim<-deleted_doc_sim[deleted_doc_sim$similarity>0.9,]

theta_df_no_dups<-cbind(LDAfit@documents,LDAfit@gamma)

matches_theta_results <- list()

for (eachnum in 1:nrow(deleted_dups)){
  temp1<-deleted_doc_sim[deleted_doc_sim$x==as.character(docnames(deleted_dups)[eachnum]),]
  temp1<-temp1[order(temp1$similarity,decreasing = TRUE),]
  matchdoc<-temp1[1,2]
  
  
  temp2<-theta_df_no_dups[theta_df_no_dups[,1]==matchdoc,]
  temp2[1]<-as.character(docnames(deleted_dups)[eachnum])
  
  matches_theta_results[[eachnum]]<-temp2
  
}

matches_theta_results_DF<-as.data.frame(do.call(rbind, matches_theta_results))

theta_df_ALL<-rbind(theta_df_no_dups,matches_theta_results_DF)

theta_df_ALL_sorted<-theta_df_ALL
colnames(theta_df_ALL_sorted)<-c("index2",paste0("V",seq(1,35)))
theta_df_ALL_sorted<-theta_df_ALL_sorted[order(as.integer(theta_df_ALL_sorted$index2)),]

sorted_meta<-Press[order(as.integer(Press$index)),]

missing <- setdiff(sorted_meta$index, theta_df_ALL_sorted$index2)

sorted_meta2 <- sorted_meta %>% 
  filter(!index == 988)
meta_theta_df_ALL<-cbind(sorted_meta2,theta_df_ALL_sorted)

meta_theta_df_ALL<-meta_theta_df_ALL[order(as.integer(meta_theta_df_ALL$index)),]

meta_theta_df_ALL[,7:((7+LDAfit@k)-1)]<- sapply(meta_theta_df_ALL[,7:((7+LDAfit@k)-1)],as.numeric)

##Calculate topic proportion
right$type <- "Partisan" 

msm$type <- "National"

local$type <- "Local"

meta_theta_by_type <- rbind(right, msm, local)

dfm_forsize<-data.frame(dfm_P)
indextemp<-dfm_forsize[,1]
dfm_forsize<-dfm_forsize[,-1]
sizevect<-rowSums(dfm_forsize)

index_and_size<-data.frame(index=indextemp,size=sizevect)

matches_size_results=list()

for (eachnum in 1:nrow(deleted_dups)){
  temp1<-deleted_doc_sim[deleted_doc_sim$x==as.character(docnames(deleted_dups)[eachnum]),]
  temp1<-temp1[order(temp1$similarity,decreasing = TRUE),]
  matchdoc<-temp1[1,2]
  
  temp2<-index_and_size[index_and_size[,1]==matchdoc,]
  
  matches_size_results[[eachnum]]<-c(as.character(docnames(deleted_dups)[eachnum]),temp2[1,2])
}

new_size_df<-as.data.frame(do.call(rbind, matches_size_results))
colnames(new_size_df)<-c("index","size")

size_df_ALL<-rbind(index_and_size,new_size_df)

size_df_ALL<-size_df_ALL[order(as.integer(size_df_ALL$index)),]
size_df_ALL[,2]<-as.integer(size_df_ALL[,2])


topic.frequency <- colSums(meta_theta_by_type[,7:41]*as.vector(size_df_ALL[,2]))
topic.proportion <- topic.frequency/sum(topic.frequency)

##Calculate topic proportion by type of newspaper
type_topic_sizes<-list()
type_topic_sizes[["topic_nums"]]<-seq(1,35)

for (type in unique(meta_theta_by_type)) {
  print (type)
  temp_meta_theta_df_ALL<-meta_theta_by_type[which(meta_theta_by_type$type==myregion),]
  temp_size_df_ALL<-size_df_ALL[which(size_df_ALL$index %in% temp_meta_theta_df_ALL$index2),]
  temp_topic.frequency <- 
    colSums(temp_meta_theta_df_ALL[,7:41]*as.vector(temp_size_df_ALL[,2]))
  temp_topic.proportion <- temp_topic.frequency/sum(temp_topic.frequency)
  type_topic_sizes[[type]]<-temp_topic.proportion
}

type_topic_sizes_df<-as.data.frame(do.call(cbind, type_topic_sizes))

##Run ANTMN

mynewnet<-network_from_LDA(LDAobject=LDAfit,
                           topic_size=topic.proportion,
                           deleted_topics=deleted_topics,
                           topic_names = topic_names,
                           save_filename= save_filename,
                           net_node_attributes=region_topic_sizes_df)
