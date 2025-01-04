Write-Host 'Starting font installation...'
$TempFolder = Join-Path $env:TEMP 'FontInstall'
$LocalFontFolder = "$env:LOCALAPPDATA\Microsoft\Windows\Fonts"
Write-Host "Using temporary folder: $TempFolder"
New-Item -Path $TempFolder -Type Directory -Force | Out-Null

# Font files will be passed as arguments
$FontFiles = $args
$SuccessCount = 0
$SkippedCount = 0

foreach ($FontPath in $FontFiles) {
    $FontName = Split-Path $FontPath -Leaf
    Write-Host "Processing font: $FontName"

    If (Test-Path "$LocalFontFolder\$FontName") {
        Write-Host "Font already installed: $FontName"
        $SkippedCount++
        continue
    }

    try {
        # Copy font to local fonts folder
        Write-Host "Copying to local fonts folder..."
        Copy-Item $FontPath -Destination "$LocalFontFolder\$FontName" -ErrorAction Stop

        # Register in user's registry
        $RegPath = "HKCU:\Software\Microsoft\Windows NT\CurrentVersion\Fonts"
        $FontType = switch ([System.IO.Path]::GetExtension($FontName)) {
            ".ttf"  { " (TrueType)" }
            ".otf"  { " (OpenType)" }
            default { "" }
        }
        $RegName = [System.IO.Path]::GetFileNameWithoutExtension($FontName) + $FontType
        New-ItemProperty -Path $RegPath -Name $RegName -Value "$LocalFontFolder\$FontName" -PropertyType String -Force | Out-Null

        Write-Host "Successfully installed: $FontName"
        $SuccessCount++
    }
    catch {
        Write-Host "Error installing font $FontName`: $_"
        continue
    }
}

Write-Host "Installation complete: $SuccessCount installed, $SkippedCount skipped"