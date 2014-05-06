library(reshape)
library(wordcloud)
library(RColorBrewer)

amcat.lda.addMeta <- function(m, meta){
  if('meta' %in% names(m)) {m$meta = meta[match(rownames(m$dtm), meta$id),]
  } else m = c(m, list(meta=meta[match(rownames(m$dtm), meta$id),]))
  m
}

## PLOTTING
amcat.plot.lda.alltopics <- function(m, time_var=m$meta$date, category_var=m$meta$medium, date_interval='day', path='/tmp/clouds/'){
  for(topic_nr in 1:nrow(m$document_sums)){
    print(paste('Plotting:',topic_nr))
    fn = paste(path, topic_nr, ".png", sep="")
    if (!is.null(fn)) png(fn, width=1280,height=800)
    amcat.plot.lda.topic(m, topic_nr, time_var, category_var, date_interval)
    if (!is.null(fn)) dev.off()
  }
  par(mfrow=c(1,1))
}

amcat.plot.lda.topic <- function(m, topic_nr, time_var=m$meta$date, category_var=m$meta$medium, date_interval='day', pct=F, value='total'){
  par(mar=c(4.5,3,2,1), cex.axis=1.7)
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), widths=c(2.5,1.5), heights=c(1,2))
  amcat.plot.lda.time(m, topic_nr, time_var, date_interval, pct=pct, value=value)
  amcat.plot.lda.wordcloud(m, topic_nr)
  amcat.plot.lda.category(m, topic_nr, category_var, pct=pct, value=value)
  par(mfrow=c(1,1))
}

amcat.prepare.time.var <- function(time_var, date_interval){
  if(class(time_var) == 'Date'){
    if(date_interval == 'day') time_var = as.Date(format(time_var, '%Y-%m-%d'))
    if(date_interval == 'month') time_var = as.Date(paste(format(time_var, '%Y-%m'),'-01',sep=''))
    if(date_interval == 'week') time_var = as.Date(paste(format(time_var, '%Y-%W'),1), '%Y-%W %u')
    if(date_interval == 'year') time_var = as.Date(paste(format(time_var, '%Y'),'-01-01',sep=''))
  } 
  time_var
}

amcat.fill.time.gaps <- function(d, date_interval){
  if(class(d$time) == 'numeric'){
    for(t in min(d$time):max(d$time)) 
      if(!t %in% d$time) d = rbind(d, data.frame(time=t, value=0))
  }
  if(class(d$time) == 'Date'){
    date_sequence = seq.Date(from=min(d$time), to=max(d$time), by=date_interval)
    for(i in 1:length(date_sequence)){
      t = date_sequence[i]
      if(!t %in% d$time) d = rbind(d, data.frame(time=t, value=0))
    }
  }
  d[order(d$time),]
}

amcat.prepare.plot.values <- function(m, break_var, topic_nr, pct=F, value='total'){
  d = data.frame(value=m$document_sums[topic_nr,], break_var=break_var)
  if(value == 'relative') d$value= d$value / colSums(m$document_sums)
  if(pct == T) d$value = d$value / sum(d$value)
  d = aggregate(d[,c('value')], by=list(break_var=d$break_var), FUN='sum')  
  d
}

amcat.plot.lda.time <- function(m, topic_nr, time_var=m$meta$date, date_interval='day', pct=F, value='total'){
  par(mar=c(3,3,3,1))
  time_var = amcat.prepare.time.var(time_var, date_interval)  
  d = amcat.prepare.plot.values(m, break_var=time_var, topic_nr=topic_nr, pct=pct, value=value)
  colnames(d) = c('time','value')
  d = amcat.fill.time.gaps(d, date_interval)
  plot(d$time, d$value, type='l', xlab='', main='', ylab='', xlim=c(min(d$time), max(d$time)), ylim=c(0, max(d$value)), bty='L', lwd=5, col='darkgrey')
  d
}

amcat.plot.lda.category <- function(m, topic_nr, category_var=m$meta$medium, pct=F, value='total'){
  par(mar=c(10,0,1,2))
  d = amcat.prepare.plot.values(m, break_var=category_var, topic_nr=topic_nr, pct=pct, value=value)
  colnames(d) = c('category','value')
  barplot(as.matrix(t(d[,c('value')])), main='', beside=TRUE,horiz=FALSE,
          density=NA,
          col='darkgrey',
          xlab='',
          ylab="",
          axes=T, names.arg=d$category, cex.names=1.5, cex.axis=1.5, adj=1, las=2)
  d
}

amcat.plot.lda.wordcloud <- function(m, topic_nr){
  x = m$topics[topic_nr,]
  x = sort(x[x>5], decreasing=T)[1:100]
  x = x[!is.na(x)]
  names = sub("/.*", "", names(x))
  freqs = x**.5
  pal <- brewer.pal(6,"YlGnBu")
  wordcloud(names, freqs, scale=c(6,.5), min.freq=1, max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
}

## GRAPHS

amcat.ucmatrix.to.simmatrix<- function(ucmatrix, similarity.measure){
  if(similarity.measure=='pearson_cor') sim.matrix = cor(t(ucmatrix))
  if(similarity.measure=='spearman_cor') sim.matrix = cor(t(ucmatrix), method='spearman')
  if(similarity.measure=='hellinger_dist') {
    library(topicmodels)
    sim.matrix = distHellinger(as.matrix(ucmatrix))
  }
  if(similarity.measure=='cosine') {
    library(lsa)
    sim.matrix = cosine(t(as.matrix(ucmatrix))) 
  }
  sim.matrix
}

amcat.simmatrix.to.simgraph <- function(m){
  m[lower.tri(m, diag=T)] = NA
  m.indices = which(!is.na(m),arr.ind=T)
  m.values = na.omit(as.vector(m))
  data.frame(node.X=m.indices[,1], node.Y=m.indices[,2], similarity=m.values)
}

amcat.ucmatrix.to.simgraph <- function(ucmatrix, similarity.measure='pearson_cor'){
  sim = amcat.ucmatrix.to.simmatrix(ucmatrix, similarity.measure)
  amcat.simmatrix.to.simgraph(sim)
}

amcat.unit.similarity.graph <- function(ucmatrix, unit.id.list, similarity.measure='pearson_cor'){
  d = aggregate(ucmatrix, by=unit.id.list, FUN='sum')
  ucmatrix = d[,(length(unit.id.list)+1):ncol(d)]
  simgraph = amcat.ucmatrix.to.simgraph(ucmatrix, similarity.measure)
  
  meta = as.data.frame(d[,names(unit.id.list)], stringsAsFactors=F)
  colnames(meta) = names(unit.id.list)
  meta = cbind(id=1:nrow(meta), meta)
  meta$topic_totals = rowSums(ucmatrix)
  
  list(graph_df=simgraph, meta=meta)
}

amcat.lda.similarity.graph <- function(m, unit.id.list, vertex_label=names(unit.id.list)[1], similarity.measure='pearson_cor'){
  library(igraph)
  doc_topic_matrix = t(m$document_sums)
  unit.similarity = amcat.unit.similarity.graph(doc_topic_matrix, unit.id.list, similarity.measure)
  g = graph.data.frame(unit.similarity$graph_df, directed=F, vertices=unit.similarity$meta)
  E(g)$weight = E(g)$similarity
  g
}


