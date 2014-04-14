library(lda)
library(reshape)
library(topicmodels)
library(slam)
library(tm)
library(wordcloud)
library(RColorBrewer)

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

lda.selectclusters <- function(model, selection) {
  w = t(model$document_sums)
  result = data.frame(rows=1:dim(w)[1])
  select <- function(i) sum(w[i,selection[[n]]])
  for (n in names(selection))  {
    #print(n)
    result = cbind(result, sapply(result$rows, select))   
  }
  result = result[,-1]
  colnames(result) <- names(selection)
  result
}

lda.getclusters <- function(clusters, labels, date=NULL, media=FALSE, aggfunc='sum', weight.length=F, column.pct=F, print.plot=F) { 
  m = lda.selectclusters(clusters, labels)
  if(weight.length == T) m = m / clusters$meta$length
  break_by = list()
  if(!is.null(date)){
    if(date == 'day') d = as.Date(format(clusters$meta$date, '%Y-%m-%d'))
    if(date == 'month') d = as.Date(paste(format(clusters$meta$date, '%Y-%m'),'-01',sep=''))
    if(date == 'week') d = as.Date(paste(format(clusters$meta$date, '%Y-%W'),1), '%Y-%W %u')
    if(date == 'year') d = as.Date(paste(format(clusters$meta$date, '%Y'),'-01-01',sep=''))
    break_by = c(break_by, list(date=d))
  } 
  if(media == T) break_by = c(break_by, list(medium=clusters$meta$medium))
  if(length(break_by) == 0) break_by = list(id=clusters$meta$id)
  m = aggregate(m, by=break_by, FUN=aggfunc)
  if(column.pct==T) for(column in names(labels)) m[,column] = m[,column] / sum(m[,column]) 
  if(print.plot == T) lda.plot(m, date, media, aggfunc)
  m
}

lda.plot <- function(m, date, media, aggfunc){
  par(mfrow=c(1, 1))
  plot.new()
  if(media == T) {
    meds = unique(m$medium)
    par(mfrow=c((ceiling(length(meds)/3)+1), 3))
    if(!is.null(date)) {
      for(medium in meds) lda.plotdate(m$date[m$medium==medium], m[m$medium==medium,3:ncol(m)], aggfunc, medium, print.legend=F)
      plot(1, type = "n", axes=FALSE, xlab="", ylab="")
      clus = colnames(m[,3:ncol(m)])
      legend(x = "top",inset = 0,
             legend = clus, 
             col=rainbow(length(clus)), lwd=5, cex=.8)
    } else for(medium in meds) lda.plotbars(medium, m[m$medium==medium,2:ncol(m)], aggfunc)
  } else {
    if(!is.null(date)) {lda.plotdate(m$date, m[,2:ncol(m)], aggfunc, '')
    } else lda.plotbars('', rowSums(m[,2:ncol(m)]), aggfunc='sum')
  }
}

lda.plotdate <- function(x, lines, aggfunc, main_label, print.legend=T){
  lines[is.na(lines)] = 0
  x = x[order(x)]
  lines = lines[order(x),]
  if(print.legend==T) par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  plot(1, type="n", axes=F, xlab='Date', main=main_label, ylab=aggfunc,
       xlim=c(min(x), max(x)), ylim=c(0, max(lines)), bty='L')
  
  axis(1, labels=x, at=x)
  axis(2, las=1)
  #box()
  colors=rainbow(ncol(lines))
  for (i in 1:ncol(lines)) {
    lines(x=x, y=lines[,i], col=colors[i])
  }
  
  if(print.legend == T) {
    legend("topright", legend=colnames(lines), inset=c(-0.2,0), col=colors, cex=.5, pch = c(1,2), lty = c(1,2))
    par(mar=c(5, 4, 4, 2) + 0.1)
  }
}

lda.plotbars <- function(name, values, aggfunc){
  colors=rainbow(ncol(values))
  labels = colnames(values)
  barplot(t(values)[,1], main=name, beside=TRUE,horiz=TRUE,
          density=NA,
          col=colors,
          xlab=aggfunc,
          ylab="",
          axes=TRUE, names.arg=labels, cex.names=0.8, las=1)
}

chi2 <- function(a,b,c,d) {
  # Compute the chi^2 statistic for a 2x2 crosstab containing the values
  # [[a, b], [c, d]]
  ooe <- function(o, e) {(o-e)*(o-e) / e}
  tot = 0.0 + a+b+c+d
  a = as.numeric(a)
  b = as.numeric(b)
  c = as.numeric(c)
  d = as.numeric(d)
  (ooe(a, (a+c)*(a+b)/tot)
   +  ooe(b, (b+d)*(a+b)/tot)
   +  ooe(c, (a+c)*(c+d)/tot)
   +  ooe(d, (d+b)*(c+d)/tot))
}

