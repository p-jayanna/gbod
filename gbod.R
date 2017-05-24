library(dplyr)
library(plyr)
library(ggplot2)

#df_csv <- read.csv('/home/praveen/AGATA/gbod.csv', stringsAsFactors = F)
#finaldataset<-gbod(df_csv,10,5)

outlierplot<-function(df){
ggplot(df, aes(x=df$norm_ertrag, y=df$norm_durchsatz, color=df$label)) +geom_point()
}
#outlierplot(finaldataset)

#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param agata dataframe with ertrag and durchsatz columns , grid scale, and outlier percentage  .
#' @keywords outlier detection, grid 
#' @export
#' @examples
#' gbod()
gbod<-function(df, n, outlierpercent)
{
  #grid based outlier detection
  #  Input:
  #df (pd.DataFrame): dataframe
  #n (int): grid row length
  #outlier percent: outlier percentage"
  #Return:
  #df_norm (pd.DataFrame):  list of data frames
  #normalize the data frame
  df_selected <- select(df,ertrag,durchsatz)
  df_omited<-na.omit(df_selected)
  df_norm<-normalize(df_omited)
  df_floor<-getFlooredvalue(df_norm)
  df_grouped <-df_floor  %>%
    group_by(norm_ertrag, norm_durchsatz) 
  df_groupcount<-group_size(df_grouped)
  df_grouped<- df_grouped %>% slice(1) %>% mutate(grid_size=df_groupcount) %>% ungroup
  df_grouped$label<-ifelse(df_grouped$grid_size >= nrow(df_omited)*(0.001), "True", "False")  
  return(arrange(df_grouped, grid_size))
}
getFlooredvalue<-function(df){
  #to get a floored value
  #  Input:
  #df (pd.DataFrame): a dataframe to be floored
  #Return:
  #df_norm (pd.DataFrame): a floored dataframe.
  df[df==10]<-9
  df_floor<-floor(df)
  return(df_floor)
  }

normalize <- function(df, n=10, rename=TRUE){
  #Normalize a dataframe for all co lumn
  #  Input:
  #df (pd.DataFrame): to normalized dataframe
  #scale (int): to scaled value
  #rename (boolean): if rename is ture, then each column name will be appended with a surfix "_norm"
  #Return:
  #df_norm (pd.DataFrame): a normalized dataframe, values are in [0, scale] interval
  df_norm = df
  df_max_ertrag <- max(df$ertrag, na.rm = TRUE)
  df_min_ertrag <- min(df$ertrag, na.rm = TRUE)
  df_norm$ertrag<-(df$ertrag-df_min_ertrag)*n/(df_max_ertrag-df_min_ertrag)
  df_max_durchsatz <- max(df$durchsatz, na.rm = TRUE)
  df_min_durchsatz <- min(df$durchsatz,na.rm = TRUE)
  df_norm$durchsatz<-(df$durchsatz-df_min_durchsatz)*n/(df_max_durchsatz-df_min_durchsatz)
  
  if(rename==TRUE)
    colnames(df_norm) <- paste("norm", colnames(df_norm), sep = "_")
  return(df_norm)
}

