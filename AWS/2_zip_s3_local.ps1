[cmdletbinding()]
param (
<<<<<<< HEAD
    [Parameter(Mandatory = $true)]
    [string] $AWSPath,
    [Parameter(Mandatory = $true)]
    [string] $rPath,
    [Parameter(Mandatory = $true)]
    [string] $LocalFolder = "S3_Bucket",
    [Parameter(Mandatory = $true)]
    [string] $FileName = "S3_Zip"
=======
	[Parameter(Mandatory = $true)]
	[string] $AWSPath,
	[Parameter(Mandatory = $true)]
	[string] $rPath,
	[Parameter(Mandatory = $true)]
	[string] $LocalFolder = "S3_Bucket",
	[Parameter(Mandatory = $true)]
	[string] $FileName = "S3_Zip"
>>>>>>> 4cd6c9069c6c04d3d4c2551e591d58bdaf1b2b04
)

$ExecutionPolicy = "Set-ExecutionPolicy RemoteSigned -Scope CurrentUser"
$Rscript = "demo.R"

rmdir -force -recurse $AWSPath\$LocalFolder
mkdir $AWSPath\$LocalFolder
copy $rPath\$Rscript $AWSPath\$LocalFolder
<<<<<<< HEAD
7z.exe a $AWSPath\$LocalFolder\$FileName.zip $AWSPath\$LocalFolder\*
=======
7z.exe a $AWSPath\$LocalFolder\$FileName.zip $AWSPath\$LocalFolder\*
>>>>>>> 4cd6c9069c6c04d3d4c2551e591d58bdaf1b2b04
