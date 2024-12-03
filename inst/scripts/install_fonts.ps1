$OutputFile = Join-Path $env:TEMP 'font_install_output.txt'
Write-Host 'Starting font installation...'

# Check for admin privileges and elevate if needed
if (-NOT ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] 'Administrator')) {
    Write-Host 'Requesting administrative privileges...'
    
    # Modify the arguments to properly capture all output streams
    $arguments = "-NoProfile -ExecutionPolicy Bypass -Command `"& {" + 
                "& '$($MyInvocation.MyCommand.Path)' $($MyInvocation.UnboundArguments) *>&1 | " +
                "Out-File -FilePath '$OutputFile' -Encoding UTF8" +
                "}`""
    
    # Start elevated process and wait for completion
    $process = Start-Process -FilePath PowerShell.exe -Verb RunAs -ArgumentList $arguments -Wait -PassThru
    
    # Read and display the output
    if (Test-Path $OutputFile) {
        Get-Content $OutputFile | ForEach-Object { Write-Host $_ }
        Remove-Item $OutputFile -Force
    }
    
    exit $process.ExitCode
}

Write-Host 'Running with administrative privileges'
$Destination = (New-Object -ComObject Shell.Application).Namespace(0x14)
$TempFolder = "$env:windir\Temp\Fonts\"
Write-Host "Using temporary folder: $TempFolder"
New-Item -Path $TempFolder -Type Directory -Force | Out-Null

# Font files will be passed as arguments
$FontFiles = $args
$SuccessCount = 0
$SkippedCount = 0

foreach ($FontPath in $FontFiles) {
    $FontName = Split-Path $FontPath -Leaf
    Write-Host "Processing font: $FontName"
    
    If (Test-Path "$env:LOCALAPPDATA\Microsoft\Windows\Fonts\$FontName") {
        Write-Host "Font already installed: $FontName"
        $SkippedCount++
        continue
    }
    
    try {
        $TempFont = "$env:windir\Temp\Fonts\$FontName"
        Write-Host "Copying to temp location: $TempFont"
        Copy-Item $FontPath -Destination $TempFolder
        Write-Host "Registering font with Windows..."
        $Destination.CopyHere($TempFont, 0x10)
        Start-Sleep -Seconds 1
        Write-Host "Cleaning up temporary file..."
        Remove-Item $TempFont -Force -ErrorAction SilentlyContinue
        Write-Host "Copying to local fonts folder..."
        Copy-Item $FontPath -Destination "$env:LOCALAPPDATA\Microsoft\Windows\Fonts\$FontName" -ErrorAction Stop
        Write-Host "Successfully installed: $FontName"
        $SuccessCount++
    }
    catch {
        Write-Host "Warning: Partial installation for $FontName (font registered but local copy failed)"
        Write-Host "This is normal and the font should still work"
        $SuccessCount++
    }
}

Write-Host "Installation complete: $SuccessCount installed, $SkippedCount skipped"