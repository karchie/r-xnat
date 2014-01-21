\name{XNAT-class}
\Rdversion{1.1}
\docType{class}
\alias{XNAT}
\alias{\S4method{close}{XNAT}}
\alias{\S4method{doStoredSearch}{XNAT}}
\alias{\S4method{getData}{XNAT}}
\alias{\S4method{getFields}{XNAT}}
\alias{\S4method{getProjects}{XNAT}}
\alias{\S4method{getSubjects}{XNAT}}
\alias{\S4method{getTypes}{XNAT}}
\alias{\S4method{initialize}{XNAT}}
\alias{\S4method{request}{XNAT}}

\title{Class \code{"XNAT"}}
\description{
R.XNAT is a client package for the XNAT imaging informatics platform, via
XNAT's RESTful web services API.
}
\section{Objects from the Class}{
  One XNAT object represents a single login session to an XNAT
  server. Objects can be created for a new session by calling the
  function \code{XNAT(baseURL, username, password)}.
  Objects can also be created for an existing session by calling the
  constructor directly, as \code{new("XNAT", baseURL, jsid)}. This form
  is most likely to be used from scripts, as \code{jsid} is the
  JSESSIONID corresponding to an active login session.
}
\section{Slots}{
  \describe{
    \item{\code{baseURL}:}{Base URL of XNAT server}
    \item{\code{jsid}:}{JSESSIONID for the login session }
  }
}
\section{Methods}{
  \describe{
    \item{close}{signature(.Object = "XNAT"):
      End the login session. The object can no longer be used after
      \code{close} is called. }
    \item{doStoredSearch}{signature(.Object = "XNAT", searchID = "character"):
      Executes the indicated stored search, returning the resulting data
      frame.}
    \item{getData}{signature(.Object = "XNAT", type = "character",
	project = "character", add.fields = "list"):
      Executes a search on the server, returning a data frame describing all
      objects of the named type visible to the user. If \code{project} is
      specified, the search is constrainted to that project. \code{add.fields}
      may specify additional columns to include in the frame, in the form
      (xsi_type_1, field_1, xsi_type_2, field_2, ...)}
    \item{getFields}{signature(.Object = "XNAT", xsitype = "character"): 
      Returns a table of the name and description of all fields that can
      be displayed for the named type. The name columns is similar to the
      names of the columns that would be returned from a \code{getData} on
      that type, modulo some upper/lowercase conversion and a few special
      cases that get renamed.}
    \item{getProjects}{signature(.Object = "XNAT"):
      Returns a table describing all projects on the XNAT server visible
      to the user.}
    \item{getSubjects}{signature(.Object = "XNAT", project =
	"character"):
      Returns a table describing all subjects on the XNAT server in
      projects visible to the user. If \code{project} is specified,
      restricts the search to the named project.}
    \item{getTypes}{signature(.Object = "XNAT"):
      Returns a table listing the XSI type and name of all searchable
      data types on the XNAT server. The type column enumerates all
      valid values for the \code{type} argument to \code{getData(...)}.}
    \item{initialize}{signature(.Object = "XNAT", baseURL =
	"character", jsid = "character"):
      Sets the session parameters for the XNAT object.}
  }
}
\section{Functions}{
  \describe{
    \item{XNAT}{signature(baseURL = "character",
	username = "character", password = "character"):
      Authenticates against the named XNAT server using the provided
      credentials to begin a new session. Returns an XNAT object that
      uses the newly create session. If both \code{username} and
      \code{password} are omitted, the client will attempt an anonymous
      session (which will work on central.xnat.org and fail nearly
      anywhere else). If \code{username} is provided but
      \code{password} is omitted, the user will be prompted for a
      password via Tcl/Tk.}
  }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
  Archie, K. A., Haselgrove, C.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
central <- XNAT("https://central.xnat.org")
projects <- getProjects(central)
types <- getTypes(central)

}
\keyword{classes}
