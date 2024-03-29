library(snowfall)
library(data.table)

N_SAMPLES_PER_TIME_PERIOD <- 1000
THRESHOLD <- .05
MAX_ITERATION <- 25
N_CPU=30

load("all_stuff.rdata")
load("result_data/final_data_w_mu_v.rdata")

N_TIME_PERIODS <- length(ALL_TIMES)

##Slow but right
get_resampled <- function(t, eta_sample_set, sigma, data, 
                          times=ALL_TIMES,
                          n_samples=N_SAMPLES_PER_TIME_PERIOD){
  ##get likelihoods
  
  date_data <- data[data$datetime == times[t]]
  weights <- rep(NA,n_samples)
  for(sample_num in 1:n_samples){
    p_param <- 1/(1+ exp(-(date_data$mu_est+date_data$v_est+eta_sample_set[sample_num])))
    weights[sample_num] <- dbinom(date_data$c_wrt,date_data$s_wrt, p_param)
  }
  weights <- weights/sum(weights)
  #print(weights[weights > .001])
  ##it seems wise to resample here:
  
  sampled <- sample(eta_sample_set,size=n_samples,replace=T,prob=weights)
  #print(paste("unique samples: ", length(unique(sampled))/n_samples))
  return(sampled)
  
}

sfInit(parallel=TRUE,cpus=N_CPU)
sfExport("get_resampled")
sfExport("N_SAMPLES_PER_TIME_PERIOD")
sfExport("N_TIME_PERIODS")
sfExport("final_data")
sfExport("THRESHOLD")
sfExport("MAX_ITERATION")
sfExport("ALL_COUNTRIES")
sfExport("ALL_TIMES")
sfLibrary(data.table)

runs <- data.table(expand.grid(category=ALL_CATEGORIES,
                               type=c("NEWS","TWITTER")))
set.seed(0)
                                  
results <- parApply(sfGetCluster(),runs,1, function(run){
    category_var <- run["category"]
    type_var <- run["type"]
    ##A is global across the word
    ##sigma is defined per country
    A <- 1.5
    old_A <- -1
    sigma <- as.list(sapply(ALL_COUNTRIES,function(l){return(5)}))
    old_sigma <- as.list(sapply(ALL_COUNTRIES,function(l){return(-1)}))
    turn_iter <- 0
    
    A_estimation_matrix <- data.frame(x=c(),y=c())
    
    ##while not convergence or max iterations
    while( (any(abs(unlist(old_sigma)-unlist(sigma)) > THRESHOLD) | abs(A-old_A) > THRESHOLD) & 
             turn_iter < MAX_ITERATION ) {
      
      per_cat_eta_samples_list <- list()
      
      turn_iter <- turn_iter + 1
      print(paste0("turn: ",turn_iter))
      
      ##get samples for each country
      for(country_var in ALL_COUNTRIES){  
        print(paste("STARTING:::::::: ",country_var,category_var))
        data <- final_data[country == country_var & category == category_var & type == type_var]
        country_sigma <- sigma[[country_var]]
        ##E STEP
        ##initialize with p0
        eta_samples <- matrix(-1,nrow=length(ALL_TIMES),ncol=N_SAMPLES_PER_TIME_PERIOD)
        init_date_data <- data[data$datetime == ALL_TIMES[1]]
        init_p <- init_date_data$c_wrt/init_date_data$s_wrt
        mean_param <- log(init_p/(1-init_p)) - init_date_data$v_est - init_date_data$mu_est
        eta_samples[1,] <- rnorm(N_SAMPLES_PER_TIME_PERIOD,mean_param,country_sigma)
        
        ##Forward Filtering (SIR)
        for(t in 2:N_TIME_PERIODS){
          #print(paste0("\t",t))
          eta_samples[t,] <- rnorm(n=N_SAMPLES_PER_TIME_PERIOD,
                                   mean=A*eta_samples[t-1,],
                                   sd=rep(country_sigma,N_SAMPLES_PER_TIME_PERIOD))
          eta_samples[t,] <- get_resampled(t, eta_samples[t,],country_sigma,data,ALL_TIMES)
        }
        ##Backwards Smoothing
        for(t in seq(N_TIME_PERIODS,1,-1)){
          eta_samples[t, ] <- sample(eta_samples[t,],size=N_SAMPLES_PER_TIME_PERIOD,replace=T)
        }
        
        ##prep for M step of A, M step for sigma
        ##Add the samples from here into the eventual re-estimation of 
        a_est_for_country <- data.frame(x=c(),y=c())
        for(time_it in 1:(length(ALL_TIMES)-1)){
          a_est_for_country <- rbind(a_est_for_country, 
                                     data.frame(x=mean(eta_samples[time_it,]),
                                                y=mean(eta_samples[time_it+1,])))
        }
        A_estimation_matrix <- rbind(A_estimation_matrix, a_est_for_country)
        
        ##Estimate sigma
        m2 <- data.frame(x=c(),y=c())
        for(time_it in 2:(length(ALL_TIMES))){
          m2 <- rbind(m2, data.frame(x=mean(eta_samples[time_it,]),
                                     y=A*mean(eta_samples[time_it-1,])))
        } 
        old_sigma[[country_var]] <- country_sigma
        sigma[[country_var]] <- sqrt(sum((m2$x-m2$y)^2)/nrow(m2))
        print(paste("Country:", country_var, 
                    "Type: ", type_var,
                    "Old sigma:", old_sigma[[country_var]], 
                    "New sigma:", sigma[[country_var]], 
                    "diff:", abs(old_sigma[[country_var]]-sigma[[country_var]]) ))  
        ##save the samples
        esl <- list(x=eta_samples)
        names(esl) <- paste(type_var,country_var,category_var,sep="_")
        plot(apply(eta_samples,1,mean))
        per_cat_eta_samples_list <- c(per_cat_eta_samples_list,esl)
      } ##end for country
      
      ##UPDATE A
      a_lm <- lm(y~x-1,A_estimation_matrix)
      old_A <- A
      A <- as.numeric(a_lm$coefficients[1])
      print(paste("Old A:", old_A, " New A:", A, "diff:", abs(old_A-A) ))
      
    } ##end while iter
    save(per_cat_eta_samples_list, file=file.path("result_data",paste0(type_var,"_",category_var,"_eta_samples.rdata")))
    df <- data.frame(type=type_var,
                    category=category_var,
                    iter=turn_iter,
                    a=A,
                    country=names(sigma),
                    sigma=as.vector(unlist(sigma)))
    save(df,file=file.path("result_data",paste0(type_var,"_",category_var,"_results.rdata")))
    return(df)
})
