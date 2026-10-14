<#
.SYNOPSIS
  Export one worksheet of an Excel binary workbook (.xlsb) to CSV.

.DESCRIPTION
  R has no maintained reader for the .xlsb container (readxlsb was archived from CRAN),
  so vendor workbooks published in that format have to be flattened before the pipeline
  can read them. Excel automation is the only reader guaranteed to be correct for a
  proprietary binary format, which is why this is a staging step rather than something
  R does inline.

  Values are pulled from Range.Value2 and written here rather than through Excel's own
  CSV writer. Excel's writer emits the *displayed* text, so a cell formatted to one
  decimal exports as "77.3" and the stored precision is gone; Value2 returns the stored
  double, which is round-tripped with the "R" format specifier.

  Cells holding a boolean are written blank. In the BNEF workbooks a FALSE in a numeric
  series means "no data for this year", and the legacy CSV export carried it through as
  the literal text FALSE, which R turned into NA anyway.

  The sheet is anchored at A1 (not at UsedRange's top-left) so row and column positions
  match what a full-sheet export would give, and callers can keep skipping a fixed
  number of banner rows.

  Macros are force-disabled and the workbook is opened read-only, so nothing in the
  vendor file runs and the source workbook is never modified.

.PARAMETER Path
  Path to the .xlsb (or any Excel-readable workbook).

.PARAMETER Sheet
  Worksheet name to export.

.PARAMETER Out
  Destination .csv path.

.EXAMPLE
  powershell -File scripts/utils/convert_xlsb_sheet.ps1 `
    -Path "data/raw/2026-08-11 - LCOE Data.xlsb" `
    -Sheet "Raw LCOE data" `
    -Out "data/interim/2026-08-11 - LCOE Data__Raw LCOE data.csv"
#>
[CmdletBinding()]
param(
  [Parameter(Mandatory = $true)][string]$Path,
  [Parameter(Mandatory = $true)][string]$Sheet,
  [Parameter(Mandatory = $true)][string]$Out
)

$ErrorActionPreference = 'Stop'

$srcPath = (Resolve-Path -LiteralPath $Path).Path

$outPath = if ([System.IO.Path]::IsPathRooted($Out)) {
  $Out
} else {
  Join-Path (Get-Location).ProviderPath $Out
}
$outDir = Split-Path -Parent $outPath
if ($outDir -and -not (Test-Path -LiteralPath $outDir)) {
  New-Item -ItemType Directory -Path $outDir -Force | Out-Null
}

$inv = [System.Globalization.CultureInfo]::InvariantCulture

function Format-Cell($v) {
  if ($null -eq $v) { return '' }
  if ($v -is [bool]) { return '' }
  if ($v -is [double] -or $v -is [single]) { return ([double]$v).ToString('R', $inv) }
  if ($v -is [decimal]) { return ([decimal]$v).ToString($inv) }
  if ($v -is [int] -or $v -is [long]) { return $v.ToString($inv) }
  $s = [string]$v
  if ($s -match '[",\r\n]') { return '"' + $s.Replace('"', '""') + '"' }
  return $s
}

$excel = $null
$book = $null
try {
  $excel = New-Object -ComObject Excel.Application
  $excel.Visible = $false
  $excel.DisplayAlerts = $false
  # 3 = msoAutomationSecurityForceDisable: never run macros in a vendor workbook.
  $excel.AutomationSecurity = 3
  $excel.AskToUpdateLinks = $false

  # UpdateLinks:=0, ReadOnly:=$true - do not touch the staged source file.
  $book = $excel.Workbooks.Open($srcPath, 0, $true)

  $ws = $null
  foreach ($candidate in $book.Worksheets) {
    if ($candidate.Name -eq $Sheet) { $ws = $candidate; break }
  }
  if ($null -eq $ws) {
    $names = ($book.Worksheets | ForEach-Object { $_.Name }) -join ', '
    throw "Sheet '$Sheet' not found in $srcPath. Available sheets: $names"
  }

  $used = $ws.UsedRange
  $lastRow = $used.Row + $used.Rows.Count - 1
  $lastCol = $used.Column + $used.Columns.Count - 1
  if ($lastRow -lt 1 -or $lastCol -lt 1) { throw "Sheet '$Sheet' is empty." }

  $values = $ws.Range($ws.Cells(1, 1), $ws.Cells($lastRow, $lastCol)).Value2

  $writer = New-Object System.IO.StreamWriter($outPath, $false, (New-Object System.Text.UTF8Encoding($false)))
  try {
    $line = New-Object System.Text.StringBuilder
    for ($r = 1; $r -le $lastRow; $r++) {
      [void]$line.Clear()
      for ($c = 1; $c -le $lastCol; $c++) {
        if ($c -gt 1) { [void]$line.Append(',') }
        [void]$line.Append((Format-Cell $values.GetValue($r, $c)))
      }
      $writer.WriteLine($line.ToString())
    }
  }
  finally {
    $writer.Dispose()
  }

  $book.Close($false)
  Write-Output "Wrote $outPath ($lastRow rows x $lastCol cols)"
}
finally {
  if ($null -ne $excel) {
    $excel.Quit()
    [System.Runtime.InteropServices.Marshal]::ReleaseComObject($excel) | Out-Null
  }
  [GC]::Collect()
  [GC]::WaitForPendingFinalizers()
}
