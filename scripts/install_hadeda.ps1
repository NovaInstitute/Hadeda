<#
.SYNOPSIS
  Install all Hadeda package prerequisites on Windows, bundle Pandoc if needed,
  restore renv dependencies, and build/install the package.

.PARAMETER PandocVersion
  Override the Pandoc release to download and bundle (defaults to 3.8.2.1).
#>

[CmdletBinding()]
param (
  [string]$PandocVersion = "3.8.2.1"
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Write-Step {
  param([string]$Message)
  Write-Host "==> $Message"
}

function Throw-IfMissing {
  param([string]$Command)
  if (-not (Get-Command $Command -ErrorAction SilentlyContinue)) {
    throw "Missing dependency: '$Command'. Please install it and run the script again."
  }
}

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$projectRoot = (Resolve-Path (Join-Path $scriptDir "..")).ProviderPath
Set-Location $projectRoot

Write-Step "Running from $projectRoot"

Throw-IfMissing -Command "Rscript"
Throw-IfMissing -Command "R"

function Ensure-Pandoc {
  if (Get-Command "pandoc.exe" -ErrorAction SilentlyContinue) {
    $pandocPath = (Get-Command "pandoc.exe").Source
    Write-Step "System pandoc detected at $pandocPath"
    return
  }

  $pandocDir = Join-Path $projectRoot "tools\pandoc"
  $pandocBin = Join-Path $pandocDir "bin"
  $bundledPandoc = Join-Path $pandocBin "pandoc.exe"

  if (Test-Path $bundledPandoc) {
    Write-Step "Using bundled pandoc at $bundledPandoc"
  }
  else {
    $arch = (Get-CimInstance Win32_OperatingSystem).OSArchitecture
    if ($arch -notlike "*64*") {
      throw "Unsupported Windows architecture: $arch (Pandoc publishes 64-bit builds only)."
    }

    $asset = "pandoc-$PandocVersion-windows-x86_64.zip"
    $url = "https://github.com/jgm/pandoc/releases/download/$PandocVersion/$asset"
    $tmp = New-TemporaryFile

    Write-Step "Downloading pandoc $PandocVersion from $url"
    Invoke-WebRequest -Uri $url -OutFile $tmp.FullName -UseBasicParsing

    $extractRoot = Join-Path $pandocDir "tmp"
    if (Test-Path $extractRoot) { Remove-Item $extractRoot -Recurse -Force }
    if (Test-Path $pandocDir) { Remove-Item $pandocDir -Recurse -Force }
    New-Item -ItemType Directory -Force -Path $extractRoot | Out-Null

    Expand-Archive -Path $tmp.FullName -DestinationPath $extractRoot -Force

    $inner = Get-ChildItem -Path $extractRoot -Directory | Select-Object -First 1
    if (-not $inner) {
      throw "Pandoc archive extraction failed; no inner directory detected."
    }

    New-Item -ItemType Directory -Force -Path $pandocBin | Out-Null
    Copy-Item -Path (Join-Path $inner.FullName "pandoc.exe") -Destination $bundledPandoc -Force

    $shareSource = Join-Path $inner.FullName "share"
    if (Test-Path $shareSource) {
      Copy-Item -Path $shareSource -Destination $pandocDir -Recurse -Force
    }

    Remove-Item $extractRoot -Recurse -Force
    Remove-Item $tmp.FullName -Force

    Write-Step "Bundled pandoc installed to $bundledPandoc"
  }

  $env:PATH = "$pandocBin;$env:PATH"
  Write-Step "PATH updated to include bundled pandoc"
}

Ensure-Pandoc

function Ensure-PkgConfigBinary {
  if (Get-Command "pkg-config.exe" -ErrorAction SilentlyContinue) {
    return
  }

  if (Get-Command "choco" -ErrorAction SilentlyContinue) {
    Write-Step "Installing pkg-config via Chocolatey"
    choco install pkgconfiglite -y --no-progress
  }
  elseif (Get-Command "winget" -ErrorAction SilentlyContinue) {
    Write-Step "Installing pkg-config via winget (MSYS2)"
    winget install --id MSYS2.MSYS2 -e --source winget
    $msysPath = "C:\msys64\usr\bin"
    if (Test-Path $msysPath) {
      $env:PATH = "$msysPath;$env:PATH"
    }
  }
  else {
    throw "pkg-config not found and no supported package manager available. Install pkg-config manually (e.g. Chocolatey 'pkgconfiglite') and rerun the script."
  }

  if (-not (Get-Command "pkg-config.exe" -ErrorAction SilentlyContinue)) {
    throw "pkg-config installation failed; ensure it is accessible on PATH."
  }
}

function Ensure-GrpcNative {
  $grpcDetected = (Start-Process -FilePath "pkg-config" -ArgumentList "--exists", "grpc", "grpc++" -NoNewWindow -Wait -PassThru).ExitCode -eq 0
  if ($grpcDetected) {
    Write-Step "gRPC development libraries detected via pkg-config"
    return
  }

  if (Get-Command "choco" -ErrorAction SilentlyContinue) {
    if (-not (Get-Command "vcpkg.exe" -ErrorAction SilentlyContinue)) {
      Write-Step "Installing vcpkg via Chocolatey to provide gRPC libraries"
      choco install vcpkg -y --no-progress
    }
  }
  elseif (Get-Command "winget" -ErrorAction SilentlyContinue) {
    if (-not (Get-Command "vcpkg.exe" -ErrorAction SilentlyContinue)) {
      Write-Step "Installing vcpkg via winget to provide gRPC libraries"
      winget install --id Microsoft.Vcpkg -e --source winget
    }
  }
  else {
    throw "gRPC development libraries not detected and no supported package manager found. Install gRPC (with pkg-config metadata) manually and rerun."
  }

  $vcpkgExe = Get-Command "vcpkg.exe" -ErrorAction SilentlyContinue
  if (-not $vcpkgExe) {
    throw "vcpkg installation failed; ensure vcpkg.exe is on PATH."
  }

  $triplet = "x64-windows"
  Write-Step "Installing gRPC via vcpkg ($triplet)"
  & $vcpkgExe.Source install "grpc:$triplet" --recurse

  $pkgconfigPath = Join-Path (Split-Path $vcpkgExe.Source) "installed\$triplet\lib\pkgconfig"
  if (Test-Path $pkgconfigPath) {
    if ($env:PKG_CONFIG_PATH) {
      $env:PKG_CONFIG_PATH = "$pkgconfigPath;$env:PKG_CONFIG_PATH"
    }
    else {
      $env:PKG_CONFIG_PATH = $pkgconfigPath
    }
  }

  $grpcDetected = (Start-Process -FilePath "pkg-config" -ArgumentList "--exists", "grpc", "grpc++" -NoNewWindow -Wait -PassThru).ExitCode -eq 0
  if (-not $grpcDetected) {
    throw "pkg-config still cannot locate gRPC after attempted installation. Ensure the pkgconfig directory is exported via PKG_CONFIG_PATH."
  }

  Write-Step "gRPC development libraries installed via vcpkg"
}

Ensure-PkgConfigBinary
Ensure-GrpcNative

$env:RENV_PATHS_LIBRARY = Join-Path $projectRoot "renv\library"
$env:RENV_CONFIG_INSTALL_OVERWRITE = "TRUE"
if (-not (Test-Path $env:RENV_PATHS_LIBRARY)) {
  New-Item -ItemType Directory -Force -Path $env:RENV_PATHS_LIBRARY | Out-Null
}

Write-Step "Restoring renv dependencies and installing gRPC package"
Rscript -e @'
Sys.setenv(
  RENV_PATHS_LIBRARY = Sys.getenv("RENV_PATHS_LIBRARY"),
  RENV_CONFIG_INSTALL_OVERWRITE = Sys.getenv("RENV_CONFIG_INSTALL_OVERWRITE")
)
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cran.rstudio.com")
}
renv::consent(provided = TRUE)
lib <- Sys.getenv("RENV_PATHS_LIBRARY")
if (nzchar(lib)) {
  .libPaths(c(lib, .libPaths()))
}
renv::restore(prompt = FALSE, library = lib)
'@

Rscript -e @'
Sys.setenv(
  RENV_PATHS_LIBRARY = Sys.getenv("RENV_PATHS_LIBRARY"),
  RENV_CONFIG_INSTALL_OVERWRITE = Sys.getenv("RENV_CONFIG_INSTALL_OVERWRITE")
)
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cran.rstudio.com")
}
if (!requireNamespace("grpc", quietly = TRUE)) {
  lib <- Sys.getenv("RENV_PATHS_LIBRARY")
  if (nzchar(lib)) {
    .libPaths(c(lib, .libPaths()))
  }
  tryCatch(
    remotes::install_github("christiaanpauw/grpc", lib = lib, upgrade = "never"),
    error = function(e) stop("Failed to install grpc from GitHub (system gRPC libraries required): ", conditionMessage(e), call. = FALSE)
  )
}
'@

Write-Step "Building package (this may take a few minutes)"
& R CMD build $projectRoot

$tarball = Get-ChildItem -Path $projectRoot -Filter "hadeda_*.tar.gz" | Sort-Object LastWriteTime -Descending | Select-Object -First 1
if (-not $tarball) {
  throw "Package tarball not found after build."
}

Write-Step "Installing package from $($tarball.Name)"
& R CMD INSTALL $tarball.FullName

Write-Step "Hadeda installation completed successfully"
