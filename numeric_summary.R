require(devtools)
require(roxygen2)
require(dplyr)
require(tibble)

numeric_summary <- function(df)
{
  num_ind <- which(sapply(df,class) %in% c("integer","numeric","double"))
  num_df <- df %>% select(colnames(df)[num_ind])
  min <- sapply(num_df,min,na.rm = T)
  max <- sapply(num_df,max,na.rm = T)
  per.25 <- sapply(num_df,quantile,na.rm = T)[2,]
  per.50 <- sapply(num_df,quantile,na.rm = T)[3,]
  per.75 <- sapply(num_df,quantile,na.rm = T)[4,]
  mean <- sapply(num_df,mean,na.rm = T)
  median <- sapply(num_df,median,na.rm = T)
  variance <- sapply(num_df,function(x){options(scipen = 10)
    var(x,na.rm = T)})
  sd <- sapply(num_df,sd,na.rm = T)
  per.na <- round(colSums(is.na(num_df))/nrow(df),3)*100
  count.na <- colSums(is.na(num_df))
  per.out <- sapply(num_df,function(x){
    round(length(boxplot.stats(x)$out)/nrow(num_df),3)*100})
  res <- data.frame(min,max,per.25,per.50,per.75,mean,median,variance,sd,count.na,per.na,per.out)
  res <- as.data.frame(t(res),stringsAsFactors = F)
  res <- rownames_to_column(res,var = "STATISTIC")
  return(res)
}