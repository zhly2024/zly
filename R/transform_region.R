#' transform_region
#'
#' @param var a region variable
#'
#' @return a new region variable
#'
#' @export
#'
#' @import magrittr
#'
#' @import dplyr
#'
#' @import tidyverse
#'
#' @examples
#' region <- "北京"
#' transform_region(region)
#'
transform_region <- function(var) {
  case_when(
    var == "北京" ~ "华北",
    var == "天津" ~ "华北",
    var == "河北" ~ "华北",
    var == "山西" ~ "华北",
    var == "内蒙古" ~ "华北",
    var == "辽宁" ~ "东北",
    var == "吉林" ~ "东北",
    var == "黑龙江" ~ "东北",
    var == "上海" ~ "华东",
    var == "江苏" ~ "华东",
    var == "浙江" ~ "华东",
    var == "安徽" ~ "华东",
    var == "福建" ~ "华东",
    var == "江西" ~ "华东",
    var == "山东" ~ "华东",
    var == "河南" ~ "华中",
    var == "湖北" ~ "华中",
    var == "湖南" ~ "华中",
    var == "广东" ~ "华南",
    var == "广西" ~ "华南",
    var == "海南" ~ "华南",
    var == "重庆" ~ "西南",
    var == "四川" ~ "西南",
    var == "贵州" ~ "西南",
    var == "云南" ~ "西南",
    var == "西藏" ~ "西南",
    var == "陕西" ~ "西北",
    var == "甘肃" ~ "西北",
    var == "青海" ~ "西北",
    var == "宁夏" ~ "西北",
    var == "新疆" ~ "西北",
    .default = "全国"
  )
}
