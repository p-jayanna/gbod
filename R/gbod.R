library(dplyr)
library(plyr)
library(ggplot2)

outlierplot<-function(df){
ggplot(df, aes(x=df$norm_x, y=df$norm_y, color=df$label)) + geom_point()
}

outlierscoreplot<-function(df){
  ggplot(df, aes(x=df$norm_x, y=df$norm_y, group=df$outlierscore)) + geom_point(aes( color=df$outlierscore, size=df$outlierscore))
  }

#' A GBOD Function
#'
#' This function allows you to find a outliers in large data sets.
#' @param agata dataframe with ertrag and durchsatz columns , grid scale, and outlier percentage  .
#' @keywords outlier detection, grid 
#' @export
#' @examples
#' gbod()
gbod<-function(df, n, outlierpercent)
{
  df_norm<-normalize(df,n)
  df_floor<-getFlooredvalue(df_norm,n)
  df_grouped <-df_floor  %>%
    group_by(norm_x,norm_y) 
  df_groupcount<-group_size(df_grouped)
  df_grouped<- df_grouped %>% slice(1) %>% mutate(grid_size=df_groupcount) %>% ungroup
  df_grouped$label<-ifelse(df_grouped$grid_size >= nrow(df_omited)*(outlierpercent/100), 1, 0)  
  return(arrange(df_grouped, grid_size))
}


#' A GBODSCORE Function
#'
#' This function allows you to caculate outlier score in large datasets.
#' @param agata dataframe with ertrag and durchsatz columns , grid scale, and outlier percentage  .
#' @keywords outlier detection, grid 
#' @export
#' @examples
#' gbod()
gbodscore<-function(df,n,outlierpercent){
  df_norm<-normalize(df,n)
  #head(df_norm)
  df_floor<-getFlooredvalue(df_norm,n)
  #head(df_floor)
  df_grouped <-df_floor  %>%
    group_by(norm_x,norm_y) 
  df_groupcount<-group_size(df_grouped)
  df_grouped<- df_grouped %>% slice(1) %>% mutate(grid_size=df_groupcount) %>% ungroup
  for(i in outlierpercent){
    df_grouped[[paste("score", i, sep = "_") ]]<-ifelse(df_grouped$grid_size >= nrow(df_omited)*(i/100), 1, 0)
  }
  df_grouped$outlierscore <- rowSums(df_grouped[ , grepl( "score" , names( df_grouped ) ) ])
  return(arrange(df_grouped, grid_size))
}
 
getFlooredvalue<-function(df, n){
  #to get a floored value
  #  Input:
  #df (pd.DataFrame): a dataframe to be floored
  #Return:
  #df_norm (pd.DataFrame): a floored dataframe.
  df[df==n]<-n-1
  df_floor<-floor(df)
  return(df_floor)
  }

normalize <- function(df, n, rename=TRUE){
  #Normalize a dataframe for all co lumn
  #  Input:
  #df (pd.DataFrame): to normalized dataframe
  #scale (int): to scaled value
  #rename (boolean): if rename is ture, then each column name will be appended with a surfix "_norm"
  #Return:
  #df_norm (pd.DataFrame): a normalized dataframe, values are in [0, scale] interval
  df_norm = df
  colnames(df_norm)<-c("x","y")
  df_max_ertrag <- max(df_norm$x, na.rm = TRUE)
  df_min_ertrag <- min(df_norm$x, na.rm = TRUE)
  df_norm$x<-(df_norm$x-df_min_ertrag)*n/(df_max_ertrag-df_min_ertrag)
  df_max_durchsatz <- max(df_norm$y, na.rm = TRUE)
  df_min_durchsatz <- min(df_norm$y,na.rm = TRUE)
  df_norm$y<-(df_norm$y-df_min_durchsatz)*n/(df_max_durchsatz-df_min_durchsatz)
  
  if(rename==TRUE)
    colnames(df_norm) <- paste("norm", colnames(df_norm), sep = "_")
  return(df_norm)
}
