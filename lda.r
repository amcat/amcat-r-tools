library(lda)
library(reshape)
library(topicmodels)
library(slam)
library(tm)
library(wordcloud)
library(RColorBrewer)

amcat.lda.addMeta <- function(m, meta){
  if('meta' %in% names(m)) {m$meta = meta[match(rownames(m$dtm), meta$id),]
  } else m = c(m, list(meta=meta[match(rownames(m$dtm), meta$id),]))
  m
}

amcat.plot.lda <- function(m, date_interval='year', path='/tmp/clouds/'){
  for(i in 1:nrow(m$document_sums)){
    print(paste('Plotting:',i))
    fn = paste(path, i, ".png", sep="")
    if (!is.null(fn)) png(fn, width=1280,height=800)
    par(mar=c(4.5,4.5,2,2), cex.axis=1.7)
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), widths=c(2.5,1.5), heights=c(1,2))
    amcat.plot.lda.date(m, date_interval, i)
    amcat.plot.lda.wordcloud(m, i)
    par(mar=c(11,4.5,2,6))
    amcat.plot.lda.medium(m, i)
    if (!is.null(fn)) dev.off()
  }
}


amcat.plot.lda.date <- function(m, date_interval, i, pct=T){
  if(date_interval == 'day') d = as.Date(format(m$meta$date, '%Y-%m-%d'))
  if(date_interval == 'month') d = as.Date(paste(format(m$meta$date, '%Y-%m'),'-01',sep=''))
  if(date_interval == 'week') d = as.Date(paste(format(m$meta$date, '%Y-%W'),1), '%Y-%W %u')
  if(date_interval == 'year') d = as.Date(paste(format(m$meta$date, '%Y'),'-01-01',sep=''))
  
  values = m$document_sums[i,]
  if(pct == T) values = values / sum(values)
  agg = aggregate(values, by=list(date=d), FUN='sum')
  plot(agg$date, agg$x, type='l', xlab='', main='', ylab='',
       xlim=c(min(agg$date), max(agg$date)), ylim=c(0, max(agg$x)), bty='L', lwd=5)
}

amcat.plot.lda.medium <- function(m, i, pct=T){
  medium = m$meta$medium
  values = m$document_sums[i,]
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


amcat.plot.lda.wordcloud <- function(m, i){
  x = m$topics[i,]
  x = sort(x[x>5], decreasing=T)[1:100]
  x = x[!is.na(x)]
  names = sub("/.*", "", names(x))
  freqs = x**.5
  pal <- brewer.pal(6,"YlGnBu")
  wordcloud(names, freqs, scale=c(6,.5), min.freq=1, max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
}


