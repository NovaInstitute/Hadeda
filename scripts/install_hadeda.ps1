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

$env:RENV_PATHS_LIBRARY = Join-Path $projectRoot "renv\library"

Write-Step "Restoring renv dependencies"
Rscript -e @'
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cran.rstudio.com")
}
renv::consent(provided = TRUE)
renv::restore(prompt = FALSE)
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
