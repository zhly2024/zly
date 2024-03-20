#' transform_pop
#'
#' @param path data path
#'
#' @return a tidy population data
#' @export
#'
#' @import tidyxl
#' @import tidyverse
#' @import unpivotr
#' @import dplyr
#' @importFrom rlang .data
#' @import magrittr
#' @import stringr
#' @examples
#' path <- system.file("extdata", "pop7raw.xlsx", package = "zly", mustWork = TRUE)
#' pop7_transformed <- transform_pop(path)
#' head(pop7_transformed)


transform_pop <- function(path){

  is_blank <- NULL #for note appeared in R CMD check,no use

  pop <- tidyxl::xlsx_cells(path,sheets = "Sheet1")

  #提取省份

  province <- pop %>%
    dplyr::filter(row>3,col==1,!is_blank) %>%
    dplyr::mutate(character = stringr::str_replace_all(character," ","")) %>%
    dplyr::select(row,col,province=character)

  table <- pop %>%
    dplyr::filter(row>3,col>1,!is_blank) %>%
    dplyr::mutate(character = str_replace_all(character," ","")) %>%
    unpivotr::behead("up-left", "age") %>%
    unpivotr::behead("up", "sex") %>%
    dplyr::select(.data$row,.data$col,.data$age,.data$sex,.data$numeric) %>%
    unpivotr::enhead(province,"up-left") %>%
    dplyr::select(.data$province,.data$age,.data$sex,.data$numeric) %>%
    dplyr::filter(.data$age != "合计") %>%
    dplyr::filter(.data$sex != "小计" & .data$sex != "合计") %>%
    dplyr::filter(.data$province != "全国")
}

