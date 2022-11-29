#' Row Chart
#'
#' @param df
#'
#' @description
#' This \code{row_miss} returns a bar chart that shows you the missing values per case.
#'
#' @return bar chart of missing values per case
#' @export
#' @import ggplot2
#' @examples
#' \dontrun{
#' library(VIM)
#' data(sleep)
#' row_miss(sleep)
#' }



row_miss <- function(df){
  if(!any(is.na(df)))retun(cat("No missing data."))
   rn <- rownames(df)
   cn <- colnames(df)
   mydf <- data.frame()
   for(row in rn){
     num <- 0
     for(column in cn){
       if(is.na(df[row,column])){
         num <- num+1
       }
     }
     mydf<- rbind(mydf, num)
   }
   mytable <- as.data.frame(table(mydf))
   mytable[, "cu"] <- cumsum(mytable$Freq)
   ggplot(mytable)+
     geom_bar(aes(x= mydf,
                  y = Freq),
              stat= "Identity",
              fill = "steelblue")+
     geom_point(aes(x= mydf,
                   y = cu),
               stat="Identity")+
     geom_line(aes(x= mydf,
                   y = cu))+
     theme_minimal()+
     labs(
       title = "Missing Values per Case",
       x = "Number of Missing Values",
       y = "Number of Cases")
}

row_miss(sleep)
