library(lda)
library(reshape)
library(topicmodels)
library(slam)
library(tm)
library(wordcloud)
library(RColorBrewer)

lda.addMeta <- function(corpus, meta){
  corpus = c(corpus, list(meta=meta[match(rownames(corpus$dtm), meta$id),]))
  corpus
}

amcat.plot.lda <- function(corpus, path='/tmp/clouds/'){
  for(i in 1:nrow(corpus$document_sums)){
    print(paste('Plotting:',i))
    fn = paste(path, i, ".png", sep="")
    if (!is.null(fn)) png(fn, width=1280,height=800)
    par(mar=c(4.5,4.5,2,2), cex.axis=1.7)
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), widths=c(2.5,1.5), heights=c(1,2))
    amcat.plot.lda.date(corpus, date, i)
    amcat.plot.lda.wordcloud(corpus, i)
    par(mar=c(11,4.5,2,6))
    amcat.plot.lda.medium(corpus, i)
    if (!is.null(fn)) dev.off()
  }
}

amcat.plot.lda.date <- function(corpus, date, i, pct=T){
  if(date == 'day') d = as.Date(format(corpus$meta$date, '%Y-%m-%d'))
  if(date == 'month') d = as.Date(paste(format(corpus$meta$date, '%Y-%m'),'-01',sep=''))
  if(date == 'week') d = as.Date(paste(format(corpus$meta$date, '%Y-%W'),1), '%Y-%W %u')
  if(date == 'year') d = as.Date(paste(format(corpus$meta$date, '%Y'),'-01-01',sep=''))
  
  values = corpus$document_sums[i,]
  if(pct == T) values = values / sum(values)
  agg = aggregate(values, by=list(date=d), FUN='sum')
  plot(agg$date, agg$x, type='l', xlab='', main='', ylab='',
       xlim=c(min(agg$date), max(agg$date)), ylim=c(0, max(agg$x)), bty='L', lwd=5)
}

amcat.plot.lda.medium <- function(corpus, i, pct=T){
  medium = corpus$meta$medium
  values = corpus$document_sums[i,]
  if(pct == T) values = values / sum(values)
  agg = aggregate(values, by=list(medium=medium), FUN='sum')
  colors=rainbow(nrow(agg))
  barplot(t(agg$x), main='', beside=TRUE,horiz=FALSE,
          density=NA,
          col='lightgrey',
          xlab='',
          ylab="",
          axes=TRUE, names.arg=agg$medium, cex.names=1.5, cex.axis=1.5, adj=1, las=2)
}


amcat.plot.lda.wordcloud <- function(corpus, i){
  x = corpus$topics[i,]
  x = sort(x[x>5], decreasing=T)[1:100]
  names = sub("/.*", "", names(x))
  freqs = x**.5
  pal <- brewer.pal(6,"YlGnBu")
  wordcloud(names, freqs, scale=c(6,.5),min.freq=1, max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
}

lda.vocabulary.statistics <- function(dt){
  vocabulary = colnames(dt)
  data.frame(term = vocabulary,
             characters = nchar(vocabulary),
             number = grepl("[0-9]", vocabulary),
             termfreq = col_sums(dt),
             docfreq = col_sums(dt > 0),
             tfidf = tapply(dt$v/row_sums(dt)[dt$i], dt$j, mean) * log2(nDocs(dt)/col_sums(dt > 0)))
}

