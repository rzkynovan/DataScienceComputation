# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

drop_columns_with_na <- function(df, null_threshold_percent) {
  threshold <- null_threshold_percent / 100
  na_proportion <- sapply(df, function(x) mean(is.na(x))) # anonymous function & sapply
  cols_to_drop <- names(na_proportion[na_proportion > threshold])

  df_new <- df[, !(names(df) %in% cols_to_drop)]
  return(df_new)
}

