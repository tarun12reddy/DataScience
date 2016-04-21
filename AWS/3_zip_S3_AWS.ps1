[cmdletbinding()]
param (
	[Parameter(Mandatory = $true)]
	[string] $AWSPath,
	[Parameter(Mandatory = $true)]
	[string] $S3Folder,
	[Parameter(Mandatory = $true)]
	[string] $LocalFolder,
	[Parameter(Mandatory = $true)]
	[string] $FileName = "S3_Zip"
)

aws s3 rm --recursive s3://datasciencetarun/$S3Folder
aws s3 mb s3://datasciencetarun/$S3Folder
aws s3 cp $AWSPath\$LocalFolder\$FileName.zip s3://datasciencetarun/$S3Folder/$FileName.zip --acl public-read

