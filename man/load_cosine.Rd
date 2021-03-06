% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparison.R
\name{load_cosine}
\alias{load_cosine}
\title{Generates cosine histogram across all possible pairwise industries}
\usage{
load_cosine(file, field = NULL)
}
\arguments{
\item{file}{Path to the input file}

\item{field}{Column name of the cosine index. By default, \code{cosine_similarity_group} is the column field.}
}
\value{
\code{load_cosine()} returns 1 ggplot object \code{skills}
}
\description{
\code{load_cosine()} loads .csv file that contains \code{industry_group_name_a}, \code{industry_group_name_b} and \code{cosine_similarity_group} column fields.
The function returns 1 ggplot:
\enumerate{
\item \code{skills} - Histogram plot of cosine similarity for all possible pairwise industries
}
}
\examples{

#load library
library('comparison')

#calling load_file function
test <- load_cosine("./dummy_data.csv", field = 'cosine_similarity_group')

#Plot histogram across all possible pairwise industries
test$skills

}
\author{
Kai Wei Tan \href{mailto:kaitan@linkedin.com}{kaitan@linkedin.com}
}
