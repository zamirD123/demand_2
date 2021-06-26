# df <- df2
# ns <- 15
# set.seed(1991)
# nu <- cbind(
#   rnorm(ns),
#   rnorm(ns)
# )
# 
# tol <- exp(-19)
# 
# sigma <- (c(0.01,0.03))
# 
# x <- c("eurpr","li")
# 
# x_mat <- df %>% select(x) %>%mutate(eurpr=eurpr/1000) %>%  as.matrix
# 
# mu_mat <- (x_mat%*%diag(sigma))%*%t(nu)
# 
# 
# colnames(mu_mat) <- paste("u",1:ns,sep = "_")
# df <- df %>% mutate(delta_t =  dln_s)
# df <- df %>% cbind(mu_mat)
# df <- df %>% mutate_at(vars(starts_with("u_")),~.+delta_t)
# df <- df %>% mutate_at(vars(starts_with("u_")),~exp(.))
# denuminator <- df %>%
#   select(starts_with("u_"),mkt) %>%
#   group_by(mkt) %>%
#   summarise_at(vars(starts_with("u_")),~sum(.)) %>% mutate_at(vars(-mkt),~.+1)
# cnam <- colnames(denuminator)
# 
# denuminator <- df %>% select(mkt,starts_with("u_")) %>% 
#   inner_join(denuminator,
#             by = "mkt") %>% select(-contains(".x"))
# colnames(denuminator) <-cnam
# 
# denuminator <- denuminator %>% select(-mkt)
# numinator <- df %>% select(starts_with("u_"))
# pi_ij <- numinator/denuminator
# pi_j <- pi_ij %>% rowMeans() %>% tibble
# df <- df %>% mutate(delta_t1= delta_t +log(s_j) -log(pi_j))
# eps <- norm(as.matrix(df$delta_t)-as.matrix(df$delta_t1))
# 
# 
# while (eps>tol) {
#   
# df$delta_t <- df$delta_t1
# df <- df %>% select(-starts_with("u_"))
# df <- df %>% cbind(mu_mat)
# df <- df %>% mutate_at(vars(starts_with("u_")),~.+delta_t)
# df <- df %>% mutate_at(vars(starts_with("u_")),~exp(.))
# denuminator <- df %>%
#   select(starts_with("u_"),mkt) %>%
#   group_by(mkt) %>%
#   summarise_at(vars(starts_with("u_")),~sum(.)) %>% mutate_at(vars(-mkt),~.+1)
# cnam <- colnames(denuminator)
# 
# denuminator <- df %>% select(mkt,starts_with("u_")) %>% 
#   inner_join(denuminator,
#              by = "mkt") %>% select(-contains(".x"))
# colnames(denuminator) <-cnam
# 
# denuminator <- denuminator %>% select(-mkt)
# numinator <- df %>% select(starts_with("u_"))
# pi_ij <- numinator/denuminator
# pi_j <- pi_ij %>% rowMeans() %>% tibble
# df <- df %>% mutate(delta_t1= delta_t +log(s_j) -log(pi_j))
# eps <- norm(as.matrix(df$delta_t)-as.matrix(df$delta_t1))
# }
#      


# 
#   ds <- df %>% 
#     mutate(xi = delta_t1 - x_mat%*%beta)
# 
# beta <- c(0.2,0.3)
# get_xi(beta)
# df$xi

df <- df2
# constract_mapping <- function(sigma) {
#   #cleaning previuos tries
#   df <- df %>% select(-contains("u_i"))
#   
#   #calculating mu
#   x_mat <- df %>%         # products characteristics 
#     select(x) %>%
#     mutate(eurpr=eurpr/1000) %>%
#     as.matrix
#   
#   mu_mat <- (x_mat%*%diag(sigma))%*%t(nu) #matrix of individual prefernces 
#   colnames(mu_mat) <- paste("u",1:ns,sep = "_") #name the prefernces of consumers i                                                 as u_i
#   
#   # setting initial delta
#   df <- df %>% mutate(delta_t =  dln_s)
#   
#   #setting diff = 1 for first run
#   dif <- 1
#   
#   #iterating intol dif < tol
#   while (dif >tol ){
#     #cleaning previuos tries
#     df <- df %>% select(-starts_with("u_"))
#     #calculating prefernaces
#     df <- df %>% cbind(mu_mat) # binding the mues with products data
#     df <- df %>%
#       mutate_at(vars(starts_with("u_")),~.+delta_t) # adding mu with delta 
#     df <- df %>%
#       mutate_at(vars(starts_with("u_")),~exp(.)) #rising in the exponenet
#     
#     #denominator
#     mkt_denuminator <- df %>%           #calculate denominator by market
#       select(starts_with("u_"),mkt) %>%
#       group_by(mkt) %>%
#       summarise_at(vars(starts_with("u_")),~sum(.)) %>%
#       mutate_at(vars(-mkt),~.+1)
#     
#     cnam <- colnames(mkt_denuminator)
#     
#     denuminator <- df %>%          # products denominator
#       select(mkt,starts_with("u_")) %>% 
#       inner_join(mkt_denuminator,
#                  by = "mkt") %>% select(-contains(".x")) 
#     
#     colnames(denuminator) <-cnam
#     denuminator <- denuminator %>% select(-mkt)
#     
#     #nominator
#     numinator <- df %>% select(starts_with("u_"))
#     
#     #pi_il
#     pi_ij <- numinator/denuminator
#     
#     #pi_j
#     pi_j <- pi_ij %>% rowMeans() %>% tibble
#     
#     #delta_t+1
#     df <- df %>% mutate(delta_t1= delta_t +log(s_j) -log(pi_j))
#     
#     #dif = ||delta_t+1 - delta_t||
#     dif <- norm(as.matrix(df$delta_t)-as.matrix(df$delta_t1))
#     df$delta_t <- df$delta_t1
#   }
#   return(df$delta_t)
# }

objectiv1 <- function(beta) {
  xi <- delta - x_mat%*%beta
  xi_mat <- cbind(xi,xi,xi) %>% as.matrix
  expec <- colMeans(xi_mat*z_mat) %>% as.matrix
  expec_norm <- norm(expec)
  return(expec_norm)
}


# val$value
# val
objective2 <- function(sigma) {
  delta <<- constract_mapping(sigma)
  ddd <- optim(c(0,0),objectiv1,upper = c(0,Inf),lower=c(-Inf,0),method = "L-BFGS-B")
  return(ddd$value)
}

objective2(sigma)

