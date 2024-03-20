#' transform_age
#'
#' @param var a age variable
#'
#' @return a new age variable
#'
#' @export
#'
#' @import dplyr
#'
#' @import readxl
#'
#' @import tidyverse
#'
#' @import magrittr
#'
#' @examples
#' age <- "0岁"
#'
#' transform_age(age)
#'
#' path <- system.file("extdata", "pop7.xlsx", package = "zly", mustWork = TRUE)
#'
#' pop7 <- readxl::read_xlsx(path)
#'
#' head(pop7)
#'
#' pop7 %>%
#'   dplyr::mutate(年龄 = transform_age(年龄)) %>%
#'   dplyr::sample_n(10)
#'
transform_age <- function(var) {
  case_when(
    var == "0岁" ~ "0-4岁",
    var == "1-4岁" ~ "0-4岁",
    var == "5-9岁" ~ "5-14岁",
    var == "10-14岁" ~ "5-14岁",
    var == "15-19岁" ~ "15-24岁",
    var == "20-24岁" ~ "15-24岁",
    var == "25-29岁" ~ "25-44岁",
    var == "30-34岁" ~ "25-44岁",
    var == "35-39岁" ~ "25-44岁",
    var == "40-44岁" ~ "25-44岁",
    var == "45-49岁" ~ "45-59岁",
    var == "50-54岁" ~ "45-59岁",
    var == "55-59岁" ~ "45-59岁",
    .default = "≥60岁"
  )
}
