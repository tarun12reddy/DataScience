[cmdletbinding()]
param (
	[Parameter(Mandatory = $true)]
	[string] $AWSPath,
	[Parameter(Mandatory = $true)]
	[string] $rPath,
	[Parameter(Mandatory = $true)]
	[string] $LocalFolder = "S3_Bucket",
	[Parameter(Mandatory = $true)]
	[string] $FileName = "S3_Zip"
)

$ExecutionPolicy = "Set-ExecutionPolicy RemoteSigned -Scope CurrentUser"
$Rscript = "demo.R"

rmdir -force -recurse $AWSPath\$LocalFolder
mkdir $AWSPath\$LocalFolder
copy $rPath\$Rscript $AWSPath\$LocalFolder
7z.exe a $AWSPath\$LocalFolder\$FileName.zip $AWSPath\$LocalFolder\*