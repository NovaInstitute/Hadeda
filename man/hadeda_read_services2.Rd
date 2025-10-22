% Generated manually because roxygen was not run
\name{hadeda_read_services2}
\alias{hadeda_read_services2}
\title{Read gRPC service definitions from an absolute proto path}
\description{
Wraps \code{grpc::read_services()} so that \code{.proto} files located outside the
current working directory can be parsed without triggering the
\dQuote{none of the files exist} error from \code{RProtoBuf::readProtoFiles()}.
}
\usage{
hadeda_read_services2(
  file,
  package = "RProtoBuf",
  pattern = "\\\\.proto$",
  lib.loc = NULL
)
}
\arguments{
  \item{file}{Path to the \code{.proto} file that declares the services.}
  \item{package}{Optional package name forwarded to
    \code{RProtoBuf::readProtoFiles()}. Set to \code{NULL} to skip package
    lookup.}
  \item{pattern}{Filename pattern passed to
    \code{RProtoBuf::readProtoFiles()}. Use \code{NULL} to keep the default
    expression.}
  \item{lib.loc}{Library location vector handed to
    \code{RProtoBuf::readProtoFiles()}.}
}
\value{
A named list describing the RPC stubs defined in \code{file}. The structure
mirrors the output of \code{grpc::read_services()} so that the result can be
used directly with \code{grpc::grpc_client()}.
}
\seealso{\code{grpc::read_services()}, \code{RProtoBuf::readProtoFiles()}}
