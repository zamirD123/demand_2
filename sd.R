# get utility from parametrs

get_utility_from_parametes <- function(delta_j, x_j, nu_i, sigma) {
  par_sum <- 0
  L <- length(x_j)
  # for (l in 1:L) {
  #   par_sum <- par_sum + x_j[[l]]*nu_i[[l]]*sigma[[l]]
  # }
  par_sum <- as.vector(x_j*nu_i)%*%sigma
  u <- exp(delta_j +par_sum)
  return(u)
}

get_mkt_id_vector <- function(prdc_id) {
  
  mkt_name <- df %>%
    filter(ID == prdc_id) %>% 
    select(mkt)
    
  mkt_name <- mkt_name[[1]]
  
  mkt_id_vector <- df %>%
    filter(mkt==mkt_name) %>%
    select(ID) %>% unlist

  return(mkt_id_vector)
}

get_consumer_prdk_prob <- function(j,nu_i,delta,x,sigma) {
  mkt_id_vector <- get_mkt_id_vector(j)
  M <- length(mkt_id_vector)
  prdcts_util <- rep(0,M)
  
  for (m in 1:M) {
    k <- mkt_id_vector[[m]]
    prdcts_util[m] <- get_utility_from_parametes(delta[k],x[1,],nu_i,sig)
  }
  numinator <- prdcts_util[j]
  denominator <- 1 +sum(prdcts_util)
  return(numinator/denominator)
}

get_prdk_mktshare <- function(j,nu,delta,x,sigma) {
  ns <- nrow(nu)
  consumer_util_j <- rep(0,ns)
  for (i in 1:ns) {
    consumer_util_j[i] <- get_consumer_prdk_prob(j,nu[i,],delta,x,sigma)
  }
  return(mean(consumer_util_j))
}

get_all_mktshares <- function(nu,delta,x,sigma) {
  J <- nrow(x)
  mktshares <- rep(0,J)
  for (j in 1:J){
    mktshares[j] <- get_prdk_mktshare(j,nu,delta,x,sigma)
    print(j)
  }
  return(mktshares)
}

mktshares <- get_all_mktshares(nu,delta_1,x,sigma)


Contraction_mapping <- function(j,nu,delta,x,sigma,tol) {
  lns_j <- log(df[[j,"s_j"]])
  lnpi_j <- log(get_prdk_mktshare(j,nu,delta,x,sigma))
  delta_j <- delta[[j]]
  delta_t <- delta_j +lns_j-lnpi_j
  
  while(abs(delta_t - delta_j) >tol) {
    delta[j] <- delta_t
    lnpi_j <- log(get_prdk_mktshare(j,nu,delta,x,sigma))
    delta_j <- delta[[j]]
    delta_t <- delta_j +lns_j-lnpi_j
  }
  print("yess")
  return(delta_t)
}

Contraction_mapping(3,nu,delta_1,x,sig,2)
