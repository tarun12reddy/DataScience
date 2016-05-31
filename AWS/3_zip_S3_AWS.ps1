[cmdletbinding()]
param (
<<<<<<< HEAD
    [Parameter(Mandatory = $true)]
    [string] $AWSPath,
    [Parameter(Mandatory = $true)]
    [string] $S3Folder,
    [Parameter(Mandatory = $true)]
    [string] $LocalFolder,
    [Parameter(Mandatory = $true)]
    [string] $FileName = "S3_Zip"
=======
	[Parameter(Mandatory = $true)]
	[string] $AWSPath,
	[Parameter(Mandatory = $true)]
	[string] $S3Folder,
	[Parameter(Mandatory = $true)]
	[string] $LocalFolder,
	[Parameter(Mandatory = $true)]
	[string] $FileName = "S3_Zip"
>>>>>>> 4cd6c9069c6c04d3d4c2551e591d58bdaf1b2b04
)

aws s3 rm --recursive s3://datasciencetarun/$S3Folder
aws s3 mb s3://datasciencetarun/$S3Folder
aws s3 cp $AWSPath\$LocalFolder\$FileName.zip s3://datasciencetarun/$S3Folder/$FileName.zip --acl public-read
<<<<<<< HEAD
=======

>>>>>>> 4cd6c9069c6c04d3d4c2551e591d58bdaf1b2b04
