<# 
.SYNOPSIS 
    This script marks up a raw ASCII test string with extended characters 
    
.DESCRIPTION 
    This script marks up a raw ASCII test string with extended characters
    so it is obvious when a string has not been localized 
        
.NOTES 
    File Name  : Apply-Accents.ps1 
    Requires   : PowerShell Version 2.0
    
.PARAMETER RawString

The string to transform

.PARAMETER DocumentPath

The path to the document being transformed

#> 
param ( 
    [Parameter(Mandatory = $true)] [string] $RawString
    )
    

$lower = "āƀćďēƒĝĥĩĵķłɱňōƥʠřšŧŭɅŵˣŷž"
$upper = "ĀƁĈĎĒƑĢĤĬĴĶĹɯŊŐƤꝘŘŠŤŨƲŴ˟ŸŽ"

$ascii = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
$accents = $lower+$upper

$table = @{}
For ($i=0; $i -le 51; $i++) {
    $key = $ascii[$i]
	$value = $accents[$i]
    $table[$key] = $value
    }

$result = ""
$RawString.ToCharArray() | % { 
if ($table.ContainsKey($_)) {
    $result += $table[$_]
    } else {
    $result += $_ 
    }
    } 
Write-Host $result