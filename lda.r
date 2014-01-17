library(lda)
library(reshape)

lda.create.matrix <- function(wordnrs, freqs, documents) {
    # wordnrs = vector of word indexes into voca
    # freqs = vector of frequencies of the same length as words
    # documents = vector of documents indices that has the samen length as words
    # voca = character vector of words
    
    docs = unique(documents)
    
    n = length(docs)
    corpus = vector("list", n)
    i = 2
    for (i in 1:n) {
      if (i%%100 == 0) print(paste("Document",i," / ", n))
      
      select = documents == docs[i]
      
      corpus[[i]] = matrix(as.integer(c(wordnrs[select] - 1, freqs[select])), nrow=2, byrow=T)
    }
    corpus
  }

lda.cluster <- function(ldadata, nclusters = 25, niterations=25) {
  m = lda.collapsed.gibbs.sampler(ldadata$matrix,
                                  nclusters,
                                  ldadata$voca$word,
                                  niterations,
                                  0.1,
                                  0.1,
                                  compute.log.likelihood=TRUE)
  m = c(m, list(article_ids = ldadata$article_ids))
  m
}

lda.addMeta <- function(lda_output, meta){
  lda_output = c(lda_output, list(meta=meta[match(lda_output$article_ids, meta$id),]))
  lda_output
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

test = as.Date('2010-02-28')
test = format(test, '%Y-%W')
as.Date(paste(test,1), '%Y-%W %u')

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

fixUnitId <- function(data){
  ## if unit_level is 'paragraph' or 'sentence', merge parnr/sentnr into article id 
  if('paragraph' %in% colnames(data) | 'sentence' %in% colnames(data)){ 
    data$id = as.character(data$id)
    if('paragraph' %in% colnames(data)) data$id = paste(data$id,data$paragraph, sep='-')
    if('sentence' %in% colnames(data)) data$id = paste(data$id,data$sentence, sep='-')  
    data$id = as.factor(data$id)
  }
  data$id
}

lda.prepareFeatures <- function(features, features.reference=data.frame(), docfreq.thres=5, docfreq_pct.max=50, over.thres=1.5, chi.thres=5, use.pos=c()) {
  ## prepares data for LDA. 
  ## A reference set can be given in order to filter words based on frequency difference
  ## docfreq.thres and docfreq_pct.max apply to features set only
  ## use docfreq.thres to filter words on a minimum nr of documents in which they occur
  ## use docfreq_pct.max to filter words on the maximum percentage of documents in the corpus
  features$id = fixUnitId(features)
  
  if (docfreq_pct.max < 100 | docfreq.thres > 0) {
    print('Calculating document frequency')
    docfreq = aggregate(hits ~ word, features, FUN='length') # in how many documents does a word occur?
    too_rare = docfreq$word[docfreq$hits < docfreq.thres]
    docfreq$docfreq_pct = (docfreq$hits / length(unique(features$id))) * 100
    too_common = docfreq$word[docfreq$docfreq_pct > docfreq_pct.max]
    print(paste('  ', length(too_rare), 'words are too rare (< docfreq.thres)'))
    print(paste('  ',length(too_common), 'words are too common (> docfreq_pct.max)'))
  }
  
  print('Selecting vocabulary')
  if (length(use.pos) > 0) {
    print(paste('  ', 'Only using words with POS tag:', paste(use.pos,collapse=', ')))
    features = features[features$pos %in% use.pos,]
    if(nrow(features.reference) > 0) features.reference = features.reference[features.reference$pos %in% use.pos,]
  }
  
  if(nrow(features.reference) > 0){
    features.all = rbind(features, features.reference)
    features.all$source = c(rep('target', nrow(features)), rep('reference', nrow(features.reference)))
    
    words = cast(features.all, word + pos ~ source, value="hits", fun.aggregate=sum)
    if (docfreq.thres > 0) words = words[!words$word %in% too_rare,]
    if (docfreq_pct.max < 100) words = words[!words$word %in% too_common,]
    
    words$chi = chi2(words$target, words$reference, sum(words$target) - words$target, sum(words$reference) - words$reference)
    words$over = (words$target / words$reference) / (sum(words$reference) / sum(words$target))
    
    voca = words[words$over > over.thres & words$chi > chi.thres,]
    voca = voca[order(voca$over),]
  } else {
    words = aggregate(hits ~ word + pos, features, FUN='sum')
    if (docfreq.thres > 0) words = words[!words$word %in% too_rare,]
    if (docfreq_pct.max < 100) words = words[!words$word %in% too_common,]
    voca = words
  }
  
  print(paste('  ','Vocabulary size =', nrow(voca)))
  print('Building matrix')
  features = features[features$word %in% voca$word,]
  ldamatrix = lda.create.matrix(match(features$word, voca$word), features$hits, features$id)
  list(matrix=ldamatrix, voca=voca, article_ids=unique(features$id))
}
