% Generated manually because roxygen was not run
\name{hadeda_read_services2}
\alias{hadeda_read_services2}
\title{Read gRPC service definitions from an absolute proto path}
\description{
Wraps \code{grpc::read_services()} so that \code{.proto} files located outside the
current working directory can be parsed without triggering the
\dQuote{none of the files exist} error from \code{RProtoBuf::readProtoFiles()} or
\code{RProtoBuf::readProtoFiles2()}.
}
\usage{
hadeda_read_services2(file, package, pattern = "\\\\.proto$", lib.loc, proto_path = NULL)
}
\arguments{
  \item{file}{Path to the \code{.proto} file that declares the services.}
  \item{package}{Optional package name forwarded to
    \code{RProtoBuf::readProtoFiles()}. When omitted, the helper defaults to
    \code{"RProtoBuf"}. Set to \code{NULL} to skip package lookup.}
  \item{pattern}{Filename pattern passed to
    \code{RProtoBuf::readProtoFiles()}. Use \code{NULL} to keep the default
    expression.}
  \item{lib.loc}{Library location vector handed to
    \code{RProtoBuf::readProtoFiles()}. Defaults to \code{NULL}.}
  \item{proto_path}{Character vector of additional import directories forwarded
    to the \code{protoPath} argument of \code{RProtoBuf::readProtoFiles2()}.
    Files must live under one of these directories so that relative imports such
    as \code{google/protobuf/wrappers.proto} can be resolved. When supplied, the
    helper automatically appends any directories discovered by
    \code{hadeda_find_protobuf_include()}.}
}
\value{
A named list describing the RPC stubs defined in \code{file}. The structure
mirrors the output of \code{grpc::read_services()} so that the result can be
used directly with \code{grpc::grpc_client()}.
}
\seealso{\code{grpc::read_services()}, \code{RProtoBuf::readProtoFiles()}, \code{RProtoBuf::readProtoFiles2()}, \code{hadeda_find_protobuf_include()}}
