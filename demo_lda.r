setwd("~/projects/amcat-r-tools")
source('../amcat-r/R/corpus.r') # temporary solution till devtools gets fixed
source('../amcat-r/R/amcatr.r')
source('lda.r')

#install.packages('devtools')
#library(devtools)
#install_github("amcat/amcat-r")

conn = amcat.connect('http://preview.amcat.nl') # AmCAT vraagt om je inloggegevens

project_id = 6
articleset_id = 45 
tokens = amcat.gettokens(conn, project_id, articleset_id, page_size=10, module="tadpole")

dtm = amcat.dtm.create(tokens$aid, tokens$lemma, tokens$freq)

termstats = amcat.term.statistics(dtm)
voca = as.character(termstats[order(termstats$tfidf, decreasing=T),][1:10000,'term'])
dtm = dtm[,voca]

m = amcat.lda.fit(dtm)
top.topic.words(m$topics)

## visualize topics
meta = amcat.getarticlemeta(conn, articleset_id)
m = amcat.lda.addMeta(m, meta)

amcat.plot.lda(m, date_interval='year', path='/tmp/clouds/')
