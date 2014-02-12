codebook.getChildren <- function(hierarchy, code){
  children = as.character(hierarchy[hierarchy$parent==code,]$code)
  allchildren = children
  while(length(children) > 0){
    children = as.character(hierarchy[hierarchy$parent %in% children,]$code)
    allchildren = c(allchildren, children)
  }
  allchildren
}

codebook.aggCode <- function(hits, hierarchy, code){
  columns = c(code, as.character(codebook.getChildren(hierarchy, code)))
  occ = hits[hits$code %in% columns,]
  print(code)
  print(paste("    nr of children =", length(columns)))
  if(nrow(occ) > 0) {
    occ = tapply(occ$hits, occ$id, FUN='sum')
    occ = data.frame(id=as.numeric(names(occ)),agg_hits=occ, code=code, row.names=NULL)
  } else occ = data.frame(id=c(), agg_hits=c(), code=c())
  occ
}

codebook.aggAllCodes <- function(hits, hierarchy, codes=c()){
  if(length(codes)==0) codes = unique(c(as.character(hierarchy$parent),as.character(hierarchy$code)))
  codes = as.character(codes)
  aggscores = data.frame()
  print('Aggregating hits')
  for(code in codes){
    aggscore = codebook.aggCode(hits, hierarchy, code)
    if(nrow(aggscore)==0) next
    aggscores = rbind(aggscores, aggscore)
  }
  print('Done!')
  aggscores
}

codebook.appendAggHits <- function(hits, hierarchy){
  hits = hits[,colnames(hits)[!colnames(hits) == 'agg_hits']]
  agghits = codebook.aggAllCodes(hits, hierarchy)
  agghits = merge(agghits, hits, by=c('id','code'), all.x=T)
  agghits$hits[is.na(agghits$hits)] = 0
  agghits
}


codebook.codebookcat <- function(hierarchy, depth=0) {
  # depth 0 is root, depth 1 is cat, etc
  
  get.ancestors <- function (hierarchy){
    p = data.frame(c=hierarchy$code, p1 = hierarchy$parent)
    for (i in 1:nrow(p)) { #nrow(p) is max number of required merges, but we'll break sooner
      m = p[, c("c", "p1")]
      colnames(m) = paste("p", c(i, i+1), sep="")
      p = merge(p, m, all.x=T)
      if (all(is.na(p[, ncol(p)]))) {
        # last column is all NA, so we are done. Drop the column and break to return
        p = p[, -ncol(p)]
        break
      }
    }
    p = p[, sort(colnames(p))]
    return(p)
  }
  
  anc = get.ancestors(hierarchy)
  x = rep(NA, nrow(anc))
  for (i in 1:(ncol(anc) - depth)) {
    print(paste("i",i,"ncol",ncol(anc), "depth", depth))
    col = anc[, i]
    parent = anc[, i+depth]
    x[!is.na(parent)] = as.character(col[!is.na(parent)])
  }
  
  return(x[match(hierarchy$code, anc$c)])
}

#' Add categories (ancestors) to a codebook 'hierarchy'
#' 
#' Adds one or more categories to codebook codes. Suppose that you have a hierarchy like
#' 
#' code, parent
#' issue, NA
#' economy, issue
#' unemployment, economy
#' environment, issue
#' 
#' The first category or 'root' for all objects will be 'issue'. The second category would be
#' 'economy' for economy and unemployment, and 'environment' for environment. For 'issue', the second
#' category would simply be issue:
#' 
#' code, parent, cat1, cat2
#' issue, NA, issue, issue
#' economy, issue, issue, economy
#' unemployment, economy, issue, economy
#' environment, issue, issue, environment
#' #' 
#' @param hierarchy the hierarchy data frame from \code{\link{amcat.gethierarchy}}
#' @param maxdepth the maxium number of ancestors per code
#' @return The hierarchy data frame with a column added for each code
#' @export
codebook.add.cats <- function(hierarchy, maxdepth=2) {
  for(depth in 0:maxdepth) {
    target = paste("cat", (depth+1), sep=".")
    hierarchy[, target] = codebook.codebookcat(hierarchy, depth)  
    if (depth > 0) {
      fallback = paste("cat", (depth), sep=".")
      hierarchy[is.na(hierarchy[,target]), target] = hierarchy[is.na(hierarchy[, target]), fallback]
    }
  }
  # Thanks, http://stackoverflow.com/questions/16441952/sort-a-data-frame-by-multiple-columns-whose-names-are-contained-in-a-single-obje
  sortnames = paste("cat", (0:maxdepth) + 1, sep=".")
  hierarchy[do.call("order", hierarchy[, sortnames]),]
}

codebook.getqueries <- function(queries, index) {
  result = list()
  for (i in 1:length(queries)) {
    c = as.character(index[i])                
    new = as.character(queries[i])
    if (!is.na(new)) 
      result[[c]] = c(result[[c]], new)
  }
  
  # combine queries to single string
  parenthesize <- function(s) {
    spaces = grepl(" ", s)
    s[spaces] =  paste("(", s[spaces], ")", sep="")
    return(s)  
  }
  combined = NULL
  for(q in names(result))
    combined = rbind(combined, c(label=q, query= paste(parenthesize(result[[q]]), collapse=" ")))
  return(as.data.frame(combined, stringsAsFactors=F))
}

