library(snowfall)
library(data.table)
load("result_data/final_data.rdata")
STEP_SIZE = .0000001 # step size
PRECISION = .000000001
N_CPU=30

##generic gradient, the estimation doesn't change much between the two variables
gradient_function <- function(old_val, data, expval){
  return(-sum(
    ((data$c_wrt-data$s_wrt) * expval/(1+expval)) +
      (data$c_wrt /(1+expval))
  )
  )
}

sfInit(parallel=TRUE,cpus=N_CPU)
sfExport("gradient_function")
sfExport("STEP_SIZE")
sfExport("PRECISION")
sfExport("final_data")
sfLibrary(data.table)
mu_table <- data.table(expand.grid(country=ALL_COUNTRIES,
                       datetime=ALL_TIMES,
                       type=c("NEWS","TWITTER"),stringsAsFactors=F))

mu_values <- parApply(sfGetCluster(),mu_table,1, function(mu_vals){
  data <- final_data[final_data$type==mu_vals["type"] &
                       country == mu_vals["country"] & 
                       final_data$datetime == as.integer(mu_vals["datetime"])]
  mu_old = 0
  mu_new = 1
  i = 0
  while(i < 1000000  & abs(mu_old - mu_new) > PRECISION ){
    i <- i + 1
    mu_old = mu_new
    expval <- exp(data$simp_v_w + mu_old)
    grad <- gradient_function(mu_old, data, expval)
    mu_new = mu_old - STEP_SIZE * grad
  }
  return(mu_new)
})
mu_table$mu_est <- mu_values
final_data <- merge(final_data,mu_table,by=c("country","datetime","type"))
save(final_data,file="result_data/final_data_w_mu.rdata")
 
sfStop()
sfInit(parallel=TRUE,cpus=N_CPU)
sfExport("gradient_function")
sfExport("STEP_SIZE")
sfExport("PRECISION")
sfExport("final_data")
sfLibrary(data.table)
v_table <- data.table(expand.grid(category=ALL_CATEGORIES,
                                   datetime=ALL_TIMES,
                                   type=c("NEWS","TWITTER"),stringsAsFactors=F))

v_values <- parApply(sfGetCluster(),v_table,1, function(v_vals){
  data <- final_data[final_data$type==v_vals["type"] &
                       category == v_vals["category"] & 
                       final_data$datetime == as.integer(v_vals["datetime"])]
  print(data)
  v_old = 0
  v_new = 1
  i = 0
  while(i < 1000000  & abs(v_old - v_new) > PRECISION ){
    i <- i + 1
    v_old = v_new
    expval <- exp(data$mu_est + v_old)
    grad <- gradient_function(v_old, data, expval)
    v_new = v_old - STEP_SIZE * grad
  }
  return(v_new)
})

v_table$v_est <- v_values
final_data <- merge(final_data,v_table,by=c("category","datetime","type"))
save(final_data,file="result_data/final_data_w_mu_v.rdata")