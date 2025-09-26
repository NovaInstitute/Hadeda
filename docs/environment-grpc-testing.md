# Environment setup instructions to allow gRPC testing

This checklist explains how to prepare a workstation so that the examples in
`vignettes/hadeda-grpc-setup.Rmd` can execute end-to-end against the Hedera
public testnet. The process assumes a Unix-like environment, but the same tools
exist on Windows via WSL, Chocolatey, or the official installers linked below.

## 1. Install system dependencies

1. Install the Protocol Buffers compiler (`protoc`).
   - **macOS**: `brew install protobuf`
   - **Ubuntu/Debian**: `sudo apt-get install -y protobuf-compiler`
   - **Windows**: Download the `protoc` binary from
     <https://github.com/protocolbuffers/protobuf/releases> and place it on your
     `PATH`.
2. Install the `grpcurl` command line utility.
   - **macOS**: `brew install grpcurl`
   - **Linux**: Download the latest release from
     <https://github.com/fullstorydev/grpcurl/releases> and extract the binary to
     `/usr/local/bin`.
   - **Windows**: Extract the `.exe` to a directory on your `PATH`.
3. Ensure OpenSSL 1.1.1 or newer is available. The R `openssl` package will link
   against the system library. Most modern distributions already satisfy this
   requirement.

## 2. Configure Hedera operator credentials

1. Create or fund a Hedera testnet account using the [Hedera portal](https://portal.hedera.com/).
2. Export the account ID and private key in your shell profile so that they are
   available to R sessions:

   ```sh
   export HADEDA_OPERATOR_ID="0.0.xxxx"
   export HADEDA_OPERATOR_KEY="-----BEGIN PRIVATE KEY-----\n...\n-----END PRIVATE KEY-----\n"
   ```

   The private key must be PEM encoded. When using a hardware wallet, ensure the
   SDK or wallet software can produce the PEM representation of your Ed25519 key.
3. Restart the terminal or source the profile to apply the changes.
4. Verify the variables are available before launching R:

   ```sh
   echo "Operator ID: $HADEDA_OPERATOR_ID"
   printf "%s" "$HADEDA_OPERATOR_KEY" | head -n 2
   ```

## 3. Install R packages

1. Install the development version of Hadeda and the supporting libraries:

   ```r
   install.packages("pak")
   pak::pak(c("hadeda-r/hadeda", "openssl", "grpc", "jsonlite"))
   ```

2. Confirm that `openssl::signature_create()` and `grpc::grpc_channel_create()`
   run without error in an interactive R session. On Linux you may need to set
   `LD_LIBRARY_PATH` if the OpenSSL runtime resides outside the standard search
   paths.

## 4. Fetch protobuf definitions

1. Choose a writable project directory and run the helper shipped with Hadeda:

   ```r
   proto_root <- hadeda::hadeda_grpc_use_proto_bundle(dest = "proto", version = "0.47.0")
   ```

2. Inspect the directory to confirm the `services/` folder and shared `.proto`
   files exist. These paths feed into the `protoc` invocation demonstrated in the
   gRPC vignette.

## 5. Validate network connectivity

1. Use `grpcurl` to request network version info before attempting signed calls:

   ```sh
   grpcurl -plaintext -import-path "$proto_root" \
     -proto services/network_service.proto \
     testnet.hedera.com:50211 proto.NetworkService/getVersionInfo
   ```

   A JSON response indicates that your firewall allows outbound gRPC traffic.
2. When running from restrictive networks (corporate VPNs, cloud CI, etc.), open
   TCP port `50211` (main Hedera gRPC port) and ensure HTTP/2 is permitted.

## 6. Run the Hadeda vignette

1. Launch R in the project root and render the vignette manually:

   ```r
   rmarkdown::render("vignettes/hadeda-grpc-setup.Rmd")
   ```

2. Inspect the console output for messages about missing binaries or failed
   signatures. The helper functions throw descriptive errors when environment
   variables are absent or when the Ed25519 key cannot be parsed.

Following these steps prepares the machine for gRPC testing. Keep the operator
key secure by restricting file permissions and avoid committing it to version
control.
