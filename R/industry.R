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

load_file <- function(file1, file3, group_by = NULL){
  df <- read.csv(file1, na.strings=c("","NA"))
  df[df==""] <- NA
  first_order <- ''
  if (is.null(group_by)) {
    first_order <- 'age_bracket'
  } else {
    first_order <- group_by
  }

  load_result_1 <- function(first_order) {
    result <- df %>%
      dplyr::group_by(first_order = get(first_order), industry_group_name_a) %>%
      dplyr::summarise(total = dplyr::n_distinct(member_id))

    result_1 <- ggplot2::ggplot(data=result, ggplot2::aes(x=reorder(industry_group_name_a, total), y=total, fill=first_order)) + ggplot2::geom_bar(stat = "identity", position = 'dodge') + ggplot2::xlab("Industry") +
      ggplot2::ylab("Number of Members") + ggplot2::coord_flip() + ggplot2::guides(fill=ggplot2::guide_legend(title=first_order)) + ggplot2::theme_bw()

    result_1
  }

  result_1 <- load_result_1(first_order)

  main_duplicate <- df
  main_duplicate <- main_duplicate %>% dplyr::filter(gap > 0)

  result <- main_duplicate %>% dplyr::filter(gap > 0) %>%
    dplyr::group_by(industry_group_name_b) %>%
    dplyr::summarise(mean = mean(gap), median = median(gap)) %>% dplyr::arrange(desc(median))

  main_duplicate$industry_group_name_b <- factor(main_duplicate$industry_group_name_b, levels = result$industry_group_name_b)

  x_axis <- main_duplicate %>%
    dplyr::summarise(mean = mean(gap), median = median(gap))

  #Industry Transition Period Plot
  result_2 <- ggplot2::ggplot(main_duplicate , ggplot2::aes(gap, industry_group_name_b)) + ggplot2::geom_boxplot() +   ggplot2::labs(y="Destination Industry",
                                                                                                                                     x="Months",
                                                                                                                                     title = "Time To Transit To An Industry ",
                                                                                                                                     subtitle="Sorted by Median in Ascending Order") +   #theme with white background
    ggplot2::theme_bw() +

    #eliminates background, gridlines, and chart border
    ggplot2::theme(
      plot.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank()
    ) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = unlist(x_axis['mean'])), col = "red")

  df <- read.csv(file3)
  result_3 <- ggplot2::ggplot(df, ggplot2::aes(cosine_similarity_group)) +
    ggplot2::geom_density(fill = "blue") +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = "Density", x='Similarity')  +
    ggplot2::scale_x_continuous(breaks=seq(0, 1, 1)) +
    ggplot2::facet_grid(industry_group_name_b~industry_group_name_a, scales = "free")

  newList <- list("demo" = result_1, "transit" = result_2, "skills" = result_3)
  }


