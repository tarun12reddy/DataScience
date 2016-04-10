[cmdletbinding()]
param (
	[Parameter(Mandatory = $true)]
	[string] $AWSPath,
	[Parameter(Mandatory = $true)]
	[string] $Folder = "colaberry",
	[Parameter(Mandatory = $true)]
	[string] $FileName = "S3_Zip"
)

aws s3 rm --recursive s3://datasciencetarun/$Folder
aws s3 mb s3://datasciencetarun/$Folder
aws s3 cp $AWSPath\$Folder\$FileName.zip s3://datasciencetarun/$Folder/$FileName.zip --acl public-read

