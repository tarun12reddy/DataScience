[cmdletbinding()]
param (
	[Parameter(Mandatory = $true, ValueFromPipeline=$true)]
	[string[]] $InputArray,
	[Parameter(Mandatory = $true)]
	[string] $AWSPath,
	[Parameter(Mandatory = $true)]
	[string] $RetrieveFolder
)

$procs = @()
$csvExports = "demo.csv"
$retrBatchFile = "cd validation
mget $csvExports"

"$retrBatchFile" | set-content retr-batchfile.ftp -Encoding Ascii

foreach ($i in $input) {
    $procs = $procs + @(Start-Process -Passthru powershell "C:\Users\qaz\Desktop\psftp.exe $i -i C:\Users\qaz\Desktop\SourceTree_DataScience\AWS\Keys\key.ppk -l ec2-user -b retr-batchfile.ftp -bc")
}

$procs | Wait-Process

rmdir -force -recurse $AWSPath\$RetrieveFolder
mkdir $AWSPath\$RetrieveFolder
copy $AWSPath\$csvExports $AWSPath\$RetrieveFolder
rm $csvExports
