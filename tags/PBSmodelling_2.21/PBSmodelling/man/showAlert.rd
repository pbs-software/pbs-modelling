\name{showAlert}
\alias{showAlert}
\title{Display a Message in an Alert Window}

\description{
  Display an alert window that contains a specified message and 
  an OK button for dismissing the window.
}
\usage{
showAlert(message, title="Alert", icon="warning")
}
\arguments{
  \item{message}{message to display in alert window}
  \item{title}{title of alert window}
  \item{icon}{icon to display in alert window; options are 
    \code{"error"}, \code{"info"}, \code{"question"}, or \code{"warning"}.}
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\seealso{
	\code{\link{getYes}}
}
\examples{
\dontrun{
showAlert("Hello World!")
}
}