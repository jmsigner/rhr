% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rhrReadData.R
\name{rhrReadData}
\alias{rhrReadData}
\title{read separator; delimited text files}
\usage{
rhrReadData(file, sep = ",", skip = 0, hasHeader = TRUE, sepDec = ".",
  stringsAsFactors = FALSE, filename = NULL, ...)
}
\arguments{
\item{file}{A character; the file name}

\item{sep}{character; the separator}

\item{skip}{numeric; the number of lines to skip}

\item{hasHeader}{logical; indicates whether or not a header is presence}

\item{sepDec}{character; indicating the decimal separator}

\item{stringsAsFactors}{logical; indicating whether or not strings should be read as factors.}

\item{filename}{character: if file and filename differ (e.g., when called through shiny)}

\item{...}{passed to scan}
}
\value{
object of class \code{data.frame}
}
\description{
Wrapper around \code{readLines} to read data with different field separators and run some checks, mainly used for the GUI
}
\details{
...
}
\examples{
\dontrun{
data(datSH)
write.csv(datSH, file.path(tempdir(), "file1.csv"), sep = ",", dec = ".")
dat <- rhrReadData(file.path(tempdir(), "file1.csv"), sep = ",", sepDec = ",")
head(dat)
rhr2md(dat)
}
}

