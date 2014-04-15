setwd("~/projects/amcat-r-tools")

library(devtools)
install_github("amcat/amcat-r")
source('lda.r')

conn = amcat.connect('http://preview.amcat.nl') # AmCAT vraagt om je inloggegevens

project_id = 6
articleset_id = 45 
tokens = amcat.gettokens(conn, project_id, articleset_id, page_size=10, module="tadpole")

dtm = amcat.dtm.create(tokens$aid, tokens$lemma, tokens$freq)
termstats = lda.vocabulary.statistics(dtm)
voca = as.character(termstats[order(termstats$tfidf, decreasing=T),][1:10000,'term'])
dtm = dtm[,voca]

corpus = amcat.lda.fit(dtm)
top.topic.words(corpus$topics)

## visualize topics
meta = amcat.articlemeta(conn, 10106)
m = lda.addMeta(m, meta)

amcat.plot.lda(corpus, path='/tmp/clouds/')




# alternatief (topicmodels package)
#dt = lda.dataframe_to_dtmatrix(features) # document-term matrix in sparsematrix structuur van tm
#lda = LDA(dt, 25, LDAcontrol=list(iter=100))
#terms(lda, 10) 

### select and name clusters
labels = list("cluster1"=1,
              "cluster2"=c(2,3),
              "cluster3"=5)

cluster.doc = lda.getclusters(m, labels) # niet geaggregeerd: waardes zijn aantal woorden per artikel (rij) dat aan cluster (kolom) toegekend is
head(cluster.doc)

# Artikelen met meer woorden (meta$length) hebben vanzelfsprekend een hogere score op clusters:
# Daarom kan het nuttig zijn om hierop te wegen (maar niet altijd, want als we per tijdseenheid of medium kijken, dan kunnen we lange artikelen ook juist zwaarder willen laten wegen)
cluster.doc = lda.getclusters(m, labels, weight.length=T) # op lengte wegen kan met weight.length=TRUE
head(cluster.doc)

# lda.getcluster kan ook aggregeren:
lda.getclusters(m, labels, date='day') # aggregeer op datum, default is aggfunc='sum' (hier kunnen we ook wegen op length, maar dan zouden we korte en lange artikelen als even belangrijk zien, wat we niet willen denk ik)
lda.getclusters(m, labels, date='year') # aggregeer op datum per 'day','week', 'month', of 'year'
lda.getclusters(m, labels, media=T) # aggregeer op medium
lda.getclusters(m, labels, date='week',media=T) # aggregeer op beide

# met print.plot kun je dan ook direct plotten
cluster.date = lda.getclusters(m, labels, date='week', print.plot=T)
cluster.medium = lda.getclusters(m, labels, media=T, print.plot=T)
cluster.datemed = lda.getclusters(m, labels, date='day', media=T, print.plot=T)

