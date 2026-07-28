param(
  [Parameter(Mandatory = $true, Position = 0)]
  [ValidateSet("run", "comp", "build", "repl")]
  [string]$Command,

  [Parameter(Position = 1)]
  [string]$Source,

  [Parameter(Position = 2)]
  [string]$Output
)

$projectPath = (wsl -d Ubuntu -- wslpath -a $PSScriptRoot).Trim()
$sourcePath = if ($null -eq $Source) { $null } else { $Source.Replace("\", "/") }

$wslArguments = @(
  "-d", "Ubuntu", "--cd", $projectPath, "--",
  "env", "LIBGL_ALWAYS_SOFTWARE=1", "SDL_VIDEODRIVER=wayland",
  "opam", "exec", "--switch=suchu", "--",
  "dune", "exec", "suchu-cli", "--", $Command
)

if ($Command -in @("run", "comp", "build")) {
  if ([string]::IsNullOrWhiteSpace($sourcePath)) {
    throw "Usage: .\suchu.ps1 $Command <programme.suchu>";
  }
  $wslArguments += $sourcePath
}

if ($Command -eq "build" -and -not [string]::IsNullOrWhiteSpace($Output)) {
  $wslArguments += @("-o", $Output.Replace("\", "/"))
}

if ($Command -eq "comp") {
  if ([string]::IsNullOrWhiteSpace($Output)) {
    throw "Usage: .\suchu.ps1 comp <programme.suchu> <sortie.ml>"
  }
  $wslArguments += $Output.Replace("\", "/")
}

& wsl @wslArguments
exit $LASTEXITCODE
