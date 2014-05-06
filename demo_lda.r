## First install the amcat-r package from github
#install.packages('devtools')
#library(devtools)
#install_github("amcat/amcat-r") 

library(amcatr)
source('lda.r') # from amcat-r-tools folder

conn = amcat.connect('http://preview.amcat.nl') # AmCAT vraagt om je inloggegevens

project_id = 403
articleset_id = 10277 # 1083 english articles about the Rwandan genocide in The Guardian, NYT and The Toronto Star
tokens = amcat.gettokens(conn, project_id, articleset_id, page_size=10, module="corenlp_lemmatize")

dtm = amcat.dtm.create(tokens$aid, tokens$lemma, tokens$freq)

termstats = amcat.term.statistics(dtm)
termstats = termstats[termstats$termfreq > 2 & termstats$nonalpha == F,]
voca = as.character(termstats[order(termstats$tfidf, decreasing=T),][1:8000,'term'])
dtm = dtm[,voca]

m = amcat.lda.fit(dtm)
top.topic.words(m$topics)

## visualize topics
meta = amcat.getarticlemeta(conn, articleset_id)
m = amcat.lda.addMeta(m, meta)

## visualize topics
par(mar=c(5,3,3,3))
amcat.plot.lda.wordcloud(m, topic_nr=1)
amcat.plot.lda.time(m, topic_nr=1, date_interval='week') # default time variable is m$meta$date 
amcat.plot.lda.category(m, 1) # default category variable is m$meta$medium
amcat.plot.lda.topic(m, 1)

# print topic plots for all topics
amcat.plot.lda(m, date_interval='day', path='/tmp/clouds/') ## set path to existing directory

## in progress
## Plot model fit for different values of K (takes a long time as it calculats a model for every value in k_values)
#source('lda_ktests.r')
#fit_scores = amcat.lda.find.best.K(dtm, k_values=seq(2,100,10))
