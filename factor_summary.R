require(devtools)
require(roxygen2)
require(dplyr)
require(tibble)

factor_summary <- function(df)
{
  fact.char_ind <- which(sapply(df,class) %in% c("character","factor"))
  fact_df <- df %>% select(colnames(df)[fact.char_ind])
  uniq.levels <- sapply(fact_df,function(x){length(levels(as.factor(x)))})
  mode <- sapply(fact_df,function(x){levels(as.factor(x))[which(table(x) == max(table(x)))]})
  mode.freq <- sapply(fact_df,function(x){max(table(x))})
  per.levels <- sapply(fact_df,function(x){
    round(sum(cumsum(sort(table(x)/sum(table(x))*100,decreasing = T)) < 80)/length(levels(as.factor(x)))*100,3)})
  res <- data.frame(uniq.levels,mode,mode.freq,per.levels)
  res <- as.data.frame(t(res),stringsAsFactors = F)
  res <- rownames_to_column(res,var = "STATISTIC")
  return(res)
}