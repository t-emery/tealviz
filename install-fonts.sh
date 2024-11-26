#!/bin/bash

# To run, first make executable with: chmod +x install-fonts.sh

# Create temporary directory for downloads
TEMP_DIR=$(mktemp -d)

# Determine OS and set fonts directory accordingly
case "$(uname -s)" in
    Darwin*)    # macOS
        FONTS_DIR="$HOME/Library/Fonts"
        ;;
    Linux*)     # Linux
        FONTS_DIR="$HOME/.local/share/fonts"
        ;;
    CYGWIN*|MINGW32*|MSYS*|MINGW*)    # Windows
        FONTS_DIR="$APPDATA/Microsoft/Windows/Fonts"
        ;;
    *)
        echo "Unsupported operating system"
        exit 1
        ;;
esac

# Function to clean up temporary files
cleanup() {
    echo "Cleaning up temporary files..."
    rm -rf "$TEMP_DIR"
}

# Set up trap to clean up on script exit
trap cleanup EXIT

# Function to install a single font family
install_font() {
    local font_name="$1"
    local font_url="$2"
    
    echo "Downloading $font_name..."
    curl -L "$font_url" -o "$TEMP_DIR/$font_name.zip"
    
    if [ $? -eq 0 ]; then
        echo "Installing $font_name..."
        unzip -q "$TEMP_DIR/$font_name.zip" -d "$TEMP_DIR/$font_name"
        
        # Move all .ttf and .otf files to Fonts directory
        find "$TEMP_DIR/$font_name" -type f \( -name "*.ttf" -o -name "*.otf" \) -exec cp {} "$FONTS_DIR/" \;
        
        echo "✓ $font_name installed successfully"
    else
        echo "✗ Failed to download $font_name"
    fi
}

# Make sure Fonts directory exists
mkdir -p "$FONTS_DIR"

# Add your desired Google Fonts here
# Format: install_font "Font Name" "Download URL"
install_font "Roboto" "https://fonts.google.com/download?family=Roboto"
install_font "Roboto+Condensed" "https://fonts.google.com/download?family=Roboto+Condensed"
install_font "Lora" "https://fonts.google.com/download?family=Lora"
install_font "Inconsolata" "https://fonts.google.com/download?family=Inconsolata"

echo "Font installation completed!"

# Clear font cache based on OS
clear_font_cache() {
    case "$(uname -s)" in
        Darwin*)    # macOS
            sudo atsutil databases -remove
            ;;
        Linux*)     # Linux
            fc-cache -f -v
            ;;
        CYGWIN*|MINGW32*|MSYS*|MINGW*)    # Windows
            # Windows requires logging out/in to refresh font cache
            echo "Please log out and log back in to complete font installation"
            ;;
    esac
}

clear_font_cache