[cmdletbinding()]
param (
    [Parameter(Mandatory = $true, ValueFromPipeline=$true)]
    [string[]] $InputArray
)

foreach ($i in $input) {
    Write-Host $i
    putty.exe ec2-user@$i -i C:\Users\qaz\Desktop\SourceTree_DataScience\AWS\Keys\key.ppk -t
}
