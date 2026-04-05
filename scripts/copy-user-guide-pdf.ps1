$projectRoot = Split-Path -Parent $PSScriptRoot
$source = Join-Path $projectRoot 'scripts\pdf-source\App_UserGuide_20260405.pdf'
$targetDir = Join-Path $projectRoot 'docs\pdf'
$target = Join-Path $targetDir 'App_UserGuide_20260405.pdf'

if (-not (Test-Path -LiteralPath $source)) {
  throw "User Guide PDF source not found: $source"
}

New-Item -ItemType Directory -Force -Path $targetDir | Out-Null

if (Test-Path -LiteralPath $target) {
  try {
    $sourceHash = (Get-FileHash -Algorithm SHA256 -LiteralPath $source).Hash
    $targetHash = (Get-FileHash -Algorithm SHA256 -LiteralPath $target).Hash
    if ($sourceHash -eq $targetHash) {
      Write-Host 'User Guide PDF already up to date.'
      exit 0
    }
  } catch {
    Write-Host 'User Guide PDF target exists but could not be hashed; continuing to copy.'
  }
}

Copy-Item -LiteralPath $source -Destination $target -Force
Write-Host 'User Guide PDF copied to docs/pdf.'
