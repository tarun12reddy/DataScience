[cmdletbinding()]
param (
<<<<<<< HEAD
    [Parameter(Mandatory = $true, ValueFromPipeline=$true)]
    [string[]] $InputArray,
    [Parameter(Mandatory = $true)]
    [string] $AWSPath,
    [Parameter(Mandatory = $true)]
    [string] $RetrieveFolder
=======
	[Parameter(Mandatory = $true, ValueFromPipeline=$true)]
	[string[]] $InputArray,
	[Parameter(Mandatory = $true)]
	[string] $AWSPath,
	[Parameter(Mandatory = $true)]
	[string] $RetrieveFolder
>>>>>>> 4cd6c9069c6c04d3d4c2551e591d58bdaf1b2b04
)

$procs = @()
$csvExports = "demo.csv"
$retrBatchFile = "cd validation
mget $csvExports"

"$retrBatchFile" | set-content retr-batchfile.ftp -Encoding Ascii

foreach ($i in $input) {
<<<<<<< HEAD
    $procs = $procs + @(Start-Process -Passthru powershell "C:\Users\qaz\Desktop\psftp.exe $i -i C:\Users\qaz\Desktop\SourceTree_DataScience\AWS\Keys\key.ppk -l ec2-user -b retr-batchfile.ftp -bc")
=======
	$procs = $procs + @(Start-Process -Passthru powershell "C:\Users\qaz\Desktop\psftp.exe $i -i C:\Users\qaz\Desktop\SourceTree_DataScience\AWS\Keys\key.ppk -l ec2-user -b retr-batchfile.ftp -bc")
>>>>>>> 4cd6c9069c6c04d3d4c2551e591d58bdaf1b2b04
}

$procs | Wait-Process

rmdir -force -recurse $AWSPath\$RetrieveFolder
mkdir $AWSPath\$RetrieveFolder
copy $AWSPath\$csvExports $AWSPath\$RetrieveFolder
<<<<<<< HEAD
rm $csvExports
=======
rm $csvExports
>>>>>>> 4cd6c9069c6c04d3d4c2551e591d58bdaf1b2b04
