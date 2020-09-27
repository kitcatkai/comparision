#'
#' #' Pipe
#'
#' Put description here
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs specify what lhs and rhs are
#' @examples
#' # some examples if you want to highlight the usage in the package
NULL

#' @export

load_mat <- function(infile){
  df <- read.csv(infile)
  df[df==""] <- NA
  result <- df %>% tidyr::drop_na() %>%
    dplyr::group_by(age_bracket, industry_group_name) %>%
    dplyr::summarise(total=total) %>%
    dplyr::mutate(freq = total / sum(total) * 100)
  ggplot2::ggplot(data=result, ggplot2::aes(x=reorder(industry_group_name, total), y=total, fill=factor(age_bracket))) + ggplot2::geom_bar(stat = "identity", position = 'dodge') + ggplot2::xlab("Industry") +
    ggplot2::ylab("Number of Members") + ggplot2::coord_flip() + ggplot2::guides(fill=ggplot2::guide_legend(title="Age Bracket")) + ggplot2::theme_bw()
}


