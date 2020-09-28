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

load_file <- function(file1, file2, file3, group_by = NULL){
  df <- read.csv(file1)
  df[df==""] <- NA
  first_order <- ''
  if (is.null(group_by)) {
    first_order <- 'age_bracket'
  } else {
    first_order <- group_by
  }
  print(colnames(df))
  load_result_1 <- function(first_order) {
    result <- df %>% tidyr::drop_na() %>%
      dplyr::group_by(first_order = get(first_order), industry_group_name) %>%
      dplyr::summarise(total=total) %>%
      dplyr::mutate(freq = total / sum(total) * 100)
    print(colnames(result))
    result_1 <- ggplot2::ggplot(data=result, ggplot2::aes(x=reorder(industry_group_name, total), y=total, fill=first_order)) + ggplot2::geom_bar(stat = "identity", position = 'dodge') + ggplot2::xlab("Industry") +
      ggplot2::ylab("Number of Members") + ggplot2::coord_flip() + ggplot2::guides(fill=ggplot2::guide_legend(title="Age Bracket")) + ggplot2::theme_bw()
    print(result_1)
    result_1
  }
  print("checkpoint1")
  result_1 <- load_result_1(first_order)
  print("checkpoint2")
  #Industry Transition Period Plot
  df <- read.csv(file2,na.strings=c("","NA"))
  df <- df %>% dplyr::filter(!industry_group_name_b %in% c('Arts', 'Agriculture', 'Public Safety')) %>% dplyr::filter(!industry_group_name_a %in% c('Arts', 'Agriculture', 'Public Safety'))
  df <- df[complete.cases(df), ]

  #df$industry_group_name_a <- factor(df$y_industry_group_name_a)
  #summarise the median and mean
  #filter out threshold need to fix this hardcode

  result <- df %>%
    dplyr::group_by(industry_group_name_b) %>%
    dplyr::summarise(mean = mean(average_length), median = median(average_length)) %>% dplyr::arrange(desc(median))

  df$industry_group_name_b <- factor(df$industry_group_name_b, levels = result$industry_group_name_b)

  #sort by average
  result <- df %>%
    dplyr::group_by(industry_group_name_b) %>%
    dplyr::summarise(mean = mean(average_length), median = median(average_length))

  #Industry Transition Period Plot
  result_2 <- ggplot2::ggplot(df , ggplot2::aes(average_length, industry_group_name_b)) + ggplot2::geom_boxplot() +   ggplot2::labs(y="Destination Industry",
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
    ggplot2::geom_vline(ggplot2::aes(xintercept = 3.23), col = "red")



  df <- read.csv(file3)
  result_3 <- ggplot2::ggplot(df, ggplot2::aes(cosine_similarity_group)) +
    ggplot2::geom_density(fill = "blue") +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = "Density", x='Similarity')  +
    ggplot2::scale_x_continuous(breaks=seq(0, 1, 1)) +
    ggplot2::facet_grid(industry_group_name_b~industry_group_name_a, scales = "free")

  newList <- list("demo" = result_1, "transit" = result_2, "skills" = result_3)
  }


