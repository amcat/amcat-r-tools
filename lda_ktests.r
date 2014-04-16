library(Rmpfr)

amcat.lda.harmonicMean <- function(logLikelihoods, precision=2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

amcat.lda.find.best.K <- function(dtm, package='lda', k_values=seq(2, 500, 50), iter=100, burnin=100, ll_interval=10){
  if(package == 'lda'){
    models <- lapply(k_values, function(k) amcat.lda.fit(dtm, K=k, num.iterations=iter, burnin=burnin, compute.log.likelihood=T))
    ll_list <- lapply(models, function(model) model$log.likelihoods[2,seq(1, iter, ll_interval)])
    
  } 
  if(package == 'topicmodels') {
    models <- lapply(k_values, function(k) LDA(ap, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep=ll_interval)))
    ll_list <- lapply(models, function(model)  model@logLiks[-c(1:(burnin/ll_interval))])
  }
  
  hm_list <- sapply(ll_list, function(ll) amcat.lda.harmonicMean(ll))
  
  plot(k_values, hm_list, type = "l")
  print(paste('Optimum number of topics =', k_values[which.max(hm_list)]))
  data.frame(k=k_values,hm_fit=hm_list)
}

