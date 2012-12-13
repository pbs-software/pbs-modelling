\name{getYes}
\alias{getYes}
\title{Prompt the User to Choose Yes or No}

\description{
  Display a message prompt with "Yes" and "No" buttons.
}
\usage{
getYes(message, title="Choice", icon="question")
}
\arguments{
  \item{message}{message to display in prompt window.}
  \item{title}{title of prompt window.}
  \item{icon}{icon to display in prompt window; options are 
    \code{"error"}, \code{"info"}, \code{"question"}, or \code{"warning"}.}
}
\value{
  Returns \code{TRUE} if the "Yes" button is clicked, \code{FALSE} if the "No" button is clicked.
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{showAlert}}, \code{\link{getChoice}}, \code{\link{chooseWinVal}}
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
  if(getYes("Print the number 1?"))
    print(1) else print("hallucination")
})
}
}