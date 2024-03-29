% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_question.R
\name{plot_question}
\alias{plot_question}
\title{Plot question}
\usage{
plot_question(
  data = construct_data(p = 17),
  str_question = "neg_mind",
  str_patient = "1",
  plotly = TRUE
)
}
\arguments{
\item{data}{tibble returned from construct_data(p = 17)}

\item{str_question}{string specifying which question to plot 
("neg_mind", "pos_mind", "old_strategy" or "new_strategy")}

\item{str_patient}{string specifying which patient (= Deltager) to extract
"1", "2", ..., "10", "Gennemsnit" or "Alle")}

\item{plotly}{logical indicating if plotly or ggplotly shoud be returned}
}
\value{
plotly object
}
\description{
Plot question
}
\examples{
data <- construct_data(p = 17)
plot_question(data = data, str_question = "neg_mind")

}
