[cmdletbinding()]
param (
	[Parameter(Mandatory = $true)]
	[string] $AWSPath,
	[Parameter(Mandatory = $true)]
	[string] $rPath,
	[Parameter(Mandatory = $true)]
	[string] $Folder = "S3_Bucket",
	[Parameter(Mandatory = $true)]
	[string] $FileName = "S3_Zip"
)

$ExecutionPolicy = "Set-ExecutionPolicy RemoteSigned -Scope CurrentUser"
$Rscript = "demo.R"

rmdir -force -recurse $AWSPath\$Folder
mkdir $AWSPath\$Folder
copy $rPath\$Rscript $AWSPath\$Folder
7z.exe a $AWSPath\$Folder\$FileName.zip $AWSPath\$Folder\*