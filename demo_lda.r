setwd("~/projects/amcat-r-tools")
source('../amcat-r/R/amcatr.r')
source('../amcat-r/R/corpus.r')
source('lda.r')

conn = amcat.connect('http://amcat.vu.nl') # AmCAT vraagt om je inloggegevens

articleset_id = 7927 # query "eu", 4 kranten, december 2013 (n = 370)
features = amcat.features(conn, articleset_id)

ldadata = lda.prepareFeatures(features, docfreq.thres=0, docfreq_pct.max=100, use.pos=c('noun','verb','NN'))
m = lda.cluster(ldadata, nclusters=25, niterations=100)
top.topic.words(m$topics)

# alternatief (topicmodels package)
#dt = lda.dataframe_to_dtmatrix(features) # document-term matrix in sparsematrix structuur van tm
#lda = LDA(dt, 25, LDAcontrol=list(iter=100))
#terms(lda, 10) 


## get meta for more fun ##

meta = amcat.getMeta(conn, articleset_id)
m = lda.addMeta(m, meta)

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

