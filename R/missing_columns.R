#' plotMiss
#'
#' Plotting missing variables and displaying the proportion
#' of missing variables by column
#'
#' @param x A dataframe
#' @import ggplot2 dplyr
#'
#' @return ggplot bar plot, if no missing data returns a message
#' @examples
#' \dontrun{
#' data(sleep, package = "VIM")
#' plotMiss(sleep)
#' }
#' @export
#'
plotMiss <- function(x){
  len = nrow(x)
  if(!any(is.na(x)))return(cat("No missing data"))

  columnSums <- colSums(is.na(x))
  missing_df  = data.frame(missing_vals = columnSums,
                           total = len)

  missing_df = missing_df %>%
    mutate(proportion = 100*(missing_vals/total)) %>%
    arrange(desc(proportion))
  missing_df['vars'] = rownames(missing_df)

  ggplot(missing_df, aes(y = proportion, x = reorder(vars, proportion))) +
    geom_bar(stat="identity", fill="cornflower blue") +
    coord_flip() +
    scale_y_continuous(limits=c(0,100), breaks=seq(0.0, 100.0, by=10.0)) +
    theme_minimal() +
    labs(
      title = "Missing Values By Variable",
      y = "Percent",
      x = "Variable"
    )
}

