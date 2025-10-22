% Generated manually because roxygen was not run
\name{hadeda_find_protobuf_include}
\alias{hadeda_find_protobuf_include}
\title{Locate the system Protocol Buffers include directory}
\description{
Searches common installation prefixes, environment variables, and package
managers to find the Protocol Buffers headers that ship with the \code{protoc}
compiler. The helper looks for directories that contain
\code{google/protobuf} so they can be added to \code{RProtoBuf}'s import path.
}
\usage{
hadeda_find_protobuf_include()
}
\value{
A character vector of include directories containing \code{google/protobuf}.
The helper returns an empty character vector when no directories are found.
}
\examples{
hadeda_find_protobuf_include()
}
\seealso{\code{hadeda_read_services2()}}
