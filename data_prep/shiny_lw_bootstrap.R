## Start processing time
start <- Sys.time()

## Load packages
options(java.parameters="-Xmx2g")
library(dplyr)
library(magrittr)

## Set survey name
#   Season: Autumn, Spring
se <- "Autumn"
yr <- 2015

### Read in data
lw <- read.csv("data/WB_lw.csv",header=T) %>% 
  filter(species != "Unidentified Species",year==yr,season==se) %>% 
  droplevels()
catch <- read.csv("data_prep/WB_catch.csv",header=T) %>% 
  filter(species != "Unidentified Species",year==yr,season==se) %>% 
  droplevels()

########################################################################################
########################################################################################
## Set TL variable as a numeric and round the final counts to a whole number
catch$count_final <- round(catch$count_final,digits=0)

## Set fixed bin widths and bin labels. Using fixed bin widths allows for "zeros" to 
##  appear (for testing purposes) and flexibility to run over multiple species.
binpar <- seq(0,1010,10)
binlabels <- as.data.frame(cbind(seq(0,1000,10)))
colnames(binlabels) <- "bins"

## Create a factor of species names
spec_list <- unique(lw$species)

len_final <- do.call(rbind,lapply(spec_list,function(i) {
  ## Filter out size modes from length data
  df <- filter(lw,species == i & tl != "NA" & !is.na(size))
  df_xs <- filter(df,size == "XS")
  df_sm <- filter(df,size=="S")
  df_md <- filter(df,size=="M")
  df_lg <- filter(df,size=="L")
  df_xl <- filter(df,size == "XL")
  df_all <- filter(df,size == "ALL")
  
  ## Filter out size modes from catch data
  df2 <- filter(catch,species == i & !is.na(size))
  df3_xs <- filter(df2,size == "XS")
  df3_sm <- filter(df2,size=="S")
  df3_md <- filter(df2,size=="M")
  df3_lg <- filter(df2,size=="L")
  df3_xl <- filter(df2,size == "XL")
  df3_all <- filter(df2,size == "ALL")
  
  ## Determine sample size of total catch in each size mode. Subtract the number of
  ##  measured lengths from total catch to prevent overestimating the total number 
  ##  of fish caught.
  n_xs <- (sum(df3_xs$count_final))-(nrow(df_xs))
  n_sm <- (sum(df3_sm$count_final))-(nrow(df_sm))
  n_md <- (sum(df3_md$count_final))-(nrow(df_md))
  n_lg <- (sum(df3_lg$count_final))-(nrow(df_lg))
  n_xl <- (sum(df3_xl$count_final))-(nrow(df_xl))
  n_all <- (sum(df3_all$count_final))-(nrow(df_all))
  samplesize <- data.frame(n_xs,n_sm,n_md,n_lg,n_xl,n_all)
  rm(n_xs,n_sm,n_md,n_lg,n_xl,n_all)
  
  ########################################################################################
  ###BOOTSTRAPPING FUNCTIONS
  
  ## Input number of iterations
  boot <- 1000
  
  ########################################################################################
  
  ########################
  ### EXTRA SMALL
  
  ##  Create a function to select a random sample of lengths with a sample size of 
  ##    the total number of extra small size class fish.
  sample_xs_fun <- function(sample_xs) {
    df_xs_subvector <- sample(df_xs$tl,as.numeric(samplesize[1,1]),replace=TRUE)
  }
  sample_xs <- sample_xs_fun(sample_xs)
  
  ## Calculate length bin proportions, repeat (n = boot)
  boot_xs <- lapply(1:boot,
                    function(boot_xs2) {
                      data <- as.data.frame(as.numeric(sample_xs_fun(sample_xs)))
                      colnames(data) <- c("tl")
                      data2 <- cut(data$tl,breaks=binpar,right=F,ordered.results=T)
                      data3 <- as.data.frame(table(data2),stringAsFactors=TRUE,optional=FALSE)
                      lcat <- as.numeric(data3$data2)
                      freq <- as.numeric(data3$Freq)
                      res <- (freq/sum(freq))
                      res2 <- cbind(res)
                    }
  )
  ## Calculate mean proportions
  {
    mean_xs <- as.data.frame(do.call(cbind,boot_xs))
    mean_xs <- transform(mean_xs,mean = apply(mean_xs, 1, mean))
    mean_xs$bin <- binlabels$bins
    mean_xs$size <- "xs"
    mean_xs <- select(mean_xs,mean,bin,size)
  }
  mean_xs
  
  ########################
  ### SMALL
  
  ##  Create a function to select a random sample of lengths with a sample size of 
  ##    the total number of small size class fish.
  sample_sm_fun <- function(sample_sm) {
    df_sm_subvector <- sample(df_sm$tl,as.numeric(samplesize[1,2]),replace=TRUE)
  }
  sample_sm <- sample_sm_fun(sample_sm)
  
  ## Calculate length bin proportions, repeat (n = boot)
  boot_sm <- lapply(1:boot,
                    function(boot_sm2) {
                      data <- as.data.frame(as.numeric(sample_sm_fun(sample_sm)))
                      colnames(data) <- c("tl")
                      data2 <- cut(data$tl,breaks=binpar,right=F,ordered.results=T)
                      data3 <- as.data.frame(table(data2),stringAsFactors=TRUE,optional=FALSE)
                      lcat <- as.numeric(data3$data2)
                      freq <- as.numeric(data3$Freq)
                      res <- (freq/sum(freq))
                      res2 <- cbind(res)
                    }
  )
  ## Calculate mean proportions
  {
    mean_sm <- as.data.frame(do.call(cbind,boot_sm))
    mean_sm <- transform(mean_sm,mean = apply(mean_sm, 1, mean))
    mean_sm$bin <- binlabels$bins
    mean_sm$size <- "sm"
    mean_sm <- select(mean_sm,mean,bin,size)
  }
  mean_sm
  
  ########################
  ### MEDIUM
  
  ##  Create a function to select a random sample of lengths with a sample size of 
  ##    the total number of medium size class fish.
  sample_md_fun <- function(sample_md) {
    df_md_subvector <- sample(df_md$tl,samplesize[1,3],replace=TRUE)
  }
  sample_md <- sample_md_fun(sample_md)
  
  ## Calculate length bin proportions, repeat (n = boot)
  boot_md <- lapply(1:boot,
                    function(boot_md2) {
                      data <- as.data.frame(as.numeric(sample_md_fun(sample_md)))
                      colnames(data) <- c("tl")
                      data2 <- cut(data$tl,breaks=binpar,right=F,ordered.results=T)
                      data3 <- as.data.frame(table(data2),stringAsFactors=TRUE,optional=FALSE)
                      lcat <- as.numeric(data3$data2)
                      freq <- as.numeric(data3$Freq)
                      res <- (freq/sum(freq))
                      res2 <- cbind(res)
                    }
  )
  ## Calculate mean proportions
  {
    mean_md <- as.data.frame(do.call(cbind,boot_md))
    mean_md <- transform(mean_md,mean = apply(mean_md, 1, mean))
    mean_md$bin <- binlabels$bins
    mean_md$size <- "md"
    mean_md <- select(mean_md,mean,bin,size)
  }
  mean_md
  
  ########################
  ### LARGE
  
  ##  Create a function to select a random sample of lengths with a sample size of 
  ##    the total number of large size class fish.
  sample_lg_fun <- function(sample_lg) {
    df_lg_subvector <- sample(df_lg$tl,samplesize[1,4],replace=TRUE)
  }
  sample_lg <- sample_lg_fun(sample_lg)
  
  ## Calculate length bin proportions, repeat (n = boot)
  boot_lg <- lapply(1:boot,
                    function(boot_lg2) {
                      data <- as.data.frame(as.numeric(sample_lg_fun(sample_lg)))
                      colnames(data) <- c("tl")
                      data2 <- cut(data$tl,breaks=binpar,right=F,ordered.results=T)
                      data3 <- as.data.frame(table(data2),stringAsFactors=TRUE,optional=FALSE)
                      lcat <- as.numeric(data3$data2)
                      freq <- as.numeric(data3$Freq)
                      res <- (freq/sum(freq))
                      res2 <- cbind(res)
                    }
  )
  ## Calculate mean proportions
  {
    mean_lg <- as.data.frame(do.call(cbind,boot_lg))
    mean_lg <- transform(mean_lg,mean = apply(mean_lg, 1, mean))
    mean_lg$bin <- binlabels$bins
    mean_lg$size <- "lg"
    mean_lg <- select(mean_lg,mean,bin,size)
  }
  mean_lg
  
  ########################
  ### EXTRA LARGE
  
  ##  Create a function to select a random sample of lengths with a sample size of 
  ##    the total number of extra large size class fish.
  sample_xl_fun <- function(sample_xl) {
    df_xl_subvector <- sample(df_xl$tl,samplesize[1,5],replace=TRUE)
  }
  sample_xl <- sample_xl_fun(sample_xl)
  
  ## Calculate length bin proportions, repeat (n = boot)
  boot_xl <- lapply(1:boot,
                    function(boot_xl2) {
                      data <- as.data.frame(as.numeric(sample_xl_fun(sample_xl)))
                      colnames(data) <- c("tl")
                      data2 <- cut(data$tl,breaks=binpar,right=F,ordered.results=T)
                      data3 <- as.data.frame(table(data2),stringAsFactors=TRUE,optional=FALSE)
                      lcat <- as.numeric(data3$data2)
                      freq <- as.numeric(data3$Freq)
                      res <- (freq/sum(freq))
                      res2 <- cbind(res)
                    }
  )
  ## Calculate mean proportions
  {
    mean_xl <- as.data.frame(do.call(cbind,boot_xl))
    mean_xl <- transform(mean_xl,mean = apply(mean_xl, 1, mean))
    mean_xl$bin <- binlabels$bins
    mean_xl$size <- "xl"
    mean_xl <- select(mean_xl,mean,bin,size)
  }
  mean_xl
  
  ########################
  ### ALL
  
  ##  Create a function to select a random sample of lengths with a sample size of 
  ##    the total number of all size class fish.
  sample_all_fun <- function(sample_all) {
    df_all_subvector <- sample(df_all$tl,samplesize[1,6],replace=TRUE)
  }
  sample_all <- sample_all_fun(sample_all)
  
  ## Calculate length bin proportions, repeat (n = boot)
  boot_all <- lapply(1:boot,
                     function(boot_all2) {
                       data <- as.data.frame(as.numeric(sample_all_fun(sample_all)))
                       colnames(data) <- c("tl")
                       data2 <- cut(data$tl,breaks=binpar,right=F,ordered.results=T)
                       data3 <- as.data.frame(table(data2),stringAsFactors=TRUE,optional=FALSE)
                       lcat <- as.numeric(data3$data2)
                       freq <- as.numeric(data3$Freq)
                       res <- (freq/sum(freq))
                       res2 <- cbind(res)
                     }
  )
  ## Calculate mean proportions
  {
    mean_all <- as.data.frame(do.call(cbind,boot_all))
    mean_all <- transform(mean_all,mean = apply(mean_all, 1, mean))
    mean_all$bin <- binlabels$bins
    mean_all$size <- "all"
    mean_all <- select(mean_all,mean,bin,size)
  }
  mean_all
  
  ######################################################################################
  ## EXPAND LENGTHS PROPORTIONALLY
  
  #################################  Extra Small  #######################################
  ## "Extra Small" size mode
  
  for(j in 1:100) {
    if (!is.na(mean_xs[j,1])) {
      xs_n <- (mean_xs[j,1])*(samplesize[1])
      xs_n <- sum(round(xs_n,digits=0))
      final_xs_len <- sample(df_xs$tl,xs_n,replace=TRUE) } else {
        final_xs_len <- as.numeric(NULL)
      }
    final_len_xs <- if((exists("final_len_xs"))==F) {
      final_xs_len } else {
        c(final_len_xs,final_xs_len)
      }
  }
  
  #####################################################################################
  ## "Small" size mode
  
  for(j in 1:100) {
    if (!is.na(mean_sm[j,1])) {
      sm_n <- (mean_sm[j,1])*(samplesize[2])
      sm_n <- sum(round(sm_n,digits=0))
      final_sm_len <- sample(df_sm$tl,sm_n,replace=TRUE) } else {
        final_sm_len <- as.numeric(NULL)
      }
    final_len_sm <- if((exists("final_len_sm"))==F) {
      final_sm_len } else {
        c(final_len_sm,final_sm_len)
      }
  }
  
  #####################################################################################
  ## "Medium" size mode
  
  for(j in 1:100) {
    if (!is.na(mean_md[j,1])) {
      md_n <- (mean_md[j,1])*(samplesize[3])
      md_n <- sum(round(md_n,digits=0))
      final_md_len <- sample(df_md$tl,md_n,replace=TRUE) } else {
        final_md_len <- as.numeric(NULL)
      }
    final_len_md <- if((exists("final_len_md"))==F) {
      final_md_len } else {
        c(final_len_md,final_md_len)
      }
  }
  
  #####################################################################################
  ## "Large" size mode
  
  for(j in 1:100) {
    if (!is.na(mean_lg[j,1])) {
      lg_n <- (mean_lg[j,1])*(samplesize[4])
      lg_n <- sum(round(lg_n,digits=0))
      final_lg_len <- sample(df_lg$tl,lg_n,replace=TRUE) } else {
        final_lg_len <- as.numeric(NULL)
      }
    final_len_lg <- if((exists("final_len_lg"))==F) {
      final_lg_len } else {
        c(final_len_lg,final_lg_len)
      }
  }
  
  #####################################################################################
  ## "Extra Large" size mode
  
  for(j in 1:100) {
    if (!is.na(mean_xl[j,1])) {
      xl_n <- (mean_xl[j,1])*(samplesize[5])
      xl_n <- sum(round(xl_n,digits=0))
      final_xl_len <- sample(df_xl$tl,xl_n,replace=TRUE) } else {
        final_xl_len <- as.numeric(NULL)
      }
    final_len_xl <- if((exists("final_len_xl"))==F) {
      final_xl_len } else {
        c(final_len_xl,final_xl_len)
      }
  }
  
  #####################################################################################
  ## "All" size mode estimated
  
  for(j in 1:100) {
    if (!is.na(mean_all[j,1])) {
      all_n <- (mean_all[j,1])*(samplesize[6])
      all_n <- sum(round(all_n,digits=0))
      final_all_len <- sample(df_all$tl,all_n,replace=TRUE) } else {
        final_all_len <- as.numeric(NULL)
      }
    final_len_all <- if((exists("final_len_all"))==F) {
      final_all_len } else {
        c(final_len_all,final_all_len)
      }
  }
  
  #####################################################################################
  #####################################################################################
  
  ## Measured lengths
  obs_len <- df$tl
  
  ## Expanded lengths
  est_len <- c(final_len_xs,final_len_sm,final_len_md,final_len_lg,
               final_len_xl,final_len_all)
  
  ## Combine lengths and assign species to lengths
  est_len <- as.data.frame(c(est_len,obs_len))
  est_len %<>% mutate(species=i,year=yr,season=se)
  
  est_len_final <- if(exists("est_len_final")==F) {
    est_len } else {
      data.frame(c(est_len$tl,est_len_final$tl),
                 c(est_len$species,est_len_final$species))
    }
  colnames(est_len_final) <- c("tl_exp","species","year","season")
  est_len_final
}))

## Read in all previous year's expanded LW data
lw_all <- read.csv("data/WB_expLengths.csv",header=T) %>% 
  filter(species != "Unidentified Species") %>% 
  select(-X)

## Bind new data with previous data set
final_exp_lw <- bind_rows(lw_all,len_final)

## Create and save the lengths into an excel file
write.csv(final_exp_lw,"data/WB_expLengths.csv")

## End processing time
end <- Sys.time()
(end-start)
