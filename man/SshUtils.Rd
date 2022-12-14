% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sshUtils.R
\name{SshUtils}
\alias{SshUtils}
\title{Ssh utils}
\description{
Very naive system based ssh utils. This should be replaced with:
https://github.com/ropensci/ssh
}
\examples{
TODO
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SshUtils-new}{\code{SshUtils$new()}}
\item \href{#method-SshUtils-testConnection}{\code{SshUtils$testConnection()}}
\item \href{#method-SshUtils-sshExecute}{\code{SshUtils$sshExecute()}}
\item \href{#method-SshUtils-syncRemoteFiles}{\code{SshUtils$syncRemoteFiles()}}
\item \href{#method-SshUtils-syncLocalFiles}{\code{SshUtils$syncLocalFiles()}}
\item \href{#method-SshUtils-clone}{\code{SshUtils$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SshUtils-new"></a>}}
\if{latex}{\out{\hypertarget{method-SshUtils-new}{}}}
\subsection{Method \code{new()}}{
Init
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SshUtils$new(
  projectManager = NULL,
  username = NULL,
  address = NULL,
  keyfile = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{projectManager}}{ProjectManager to init connection}

\item{\code{username}}{character for username}

\item{\code{address}}{character for address}

\item{\code{keyfile}}{character for keyfile path}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SshUtils-testConnection"></a>}}
\if{latex}{\out{\hypertarget{method-SshUtils-testConnection}{}}}
\subsection{Method \code{testConnection()}}{
Test connection
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SshUtils$testConnection()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SshUtils-sshExecute"></a>}}
\if{latex}{\out{\hypertarget{method-SshUtils-sshExecute}{}}}
\subsection{Method \code{sshExecute()}}{
execute command
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SshUtils$sshExecute(expr, outputFile = NULL, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{expr}}{character to execute ssh call}

\item{\code{outputFile}}{character to output file}

\item{\code{...}}{passed to private$execCmd}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SshUtils-syncRemoteFiles"></a>}}
\if{latex}{\out{\hypertarget{method-SshUtils-syncRemoteFiles}{}}}
\subsection{Method \code{syncRemoteFiles()}}{
Copy files to HPC
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SshUtils$syncRemoteFiles(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{passed to private$syncFiles}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SshUtils-syncLocalFiles"></a>}}
\if{latex}{\out{\hypertarget{method-SshUtils-syncLocalFiles}{}}}
\subsection{Method \code{syncLocalFiles()}}{
Copy files to local
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SshUtils$syncLocalFiles(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{passed to private$syncFiles}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SshUtils-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SshUtils-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SshUtils$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
