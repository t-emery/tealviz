Write-Host 'Starting font uninstallation...'
$LocalFontFolder = "$env:LOCALAPPDATA\Microsoft\Windows\Fonts"

# Font files will be passed as arguments
$FontFiles = $args
$SuccessCount = 0
$SkippedCount = 0

foreach ($FontName in $FontFiles) {
    # Get just the filename if full path is provided
    $FontName = Split-Path $FontName -Leaf
    Write-Host "Processing font: $FontName"

    If (-not (Test-Path "$LocalFontFolder\$FontName")) {
        Write-Host "Font not found: $FontName"
        $SkippedCount++
        continue
    }

    try {
        # Remove from registry first
        $RegPath = "HKCU:\Software\Microsoft\Windows NT\CurrentVersion\Fonts"
        $FontType = switch ([System.IO.Path]::GetExtension($FontName)) {
            ".ttf"  { " (TrueType)" }
            ".otf"  { " (OpenType)" }
            default { "" }
        }
        $RegName = [System.IO.Path]::GetFileNameWithoutExtension($FontName) + $FontType
        Remove-ItemProperty -Path $RegPath -Name $RegName -ErrorAction Stop

        # Delete the font file
        Remove-Item "$LocalFontFolder\$FontName" -Force -ErrorAction Stop

        Write-Host "Successfully uninstalled: $FontName"
        $SuccessCount++
    }
    catch {
        Write-Host "Error uninstalling font $FontName`: $_"
        continue
    }
}

Write-Host "Uninstallation complete: $SuccessCount removed, $SkippedCount skipped"