<<<<<<< HEAD
 [cmdletbinding()]
 param (
     [Parameter(Mandatory = $true, ValueFromPipeline=$true)]
     [string] $InputArray,
     [Parameter(Mandatory = $true)]
     [string] $S3Folder,
     [Parameter(Mandatory = $true)]
     [string] $FileName
 )
 
 $procs = @()
 
 del cmd*.sh
 foreach ($i in $input) {
     $commandText = "pkill screen
         rm -rf validation
         mkdir validation
         cd validation
         wget https://s3-us-west-1.amazonaws.com/datasciencetarun/" + $S3Folder + "/" + $FileName + ".zip
         unzip -o " + $FileName + ".zip
         echo 'Rscript demo.R' > shell.sh
         chmod +x shell.sh
         screen -q -s ./shell.sh"
         $name = "cmd" + $i.Replace(' ', '') + ".sh"
         [string]$outputFile = [System.IO.Path]::Combine($name)
         Add-Content $outputFile "$commandText"
         Write-Host "wrote $i "
         putty -i C:\Users\qaz\Desktop\SourceTree_DataScience\AWS\Keys\key.ppk ec2-user@$i -m $name -t
 }
 
 $procs | Wait-Process
=======
[cmdletbinding()]
param (
	[Parameter(Mandatory = $true, ValueFromPipeline=$true)]
	[string] $InputArray,
	[Parameter(Mandatory = $true)]
	[string] $S3Folder,
	[Parameter(Mandatory = $true)]
	[string] $FileName
)

$procs = @()

del cmd*.sh
foreach ($i in $input) {
	$commandText = "pkill screen
		rm -rf validation
		mkdir validation
		cd validation
		wget https://s3-us-west-1.amazonaws.com/datasciencetarun/" + $S3Folder + "/" + $FileName + ".zip
		unzip -o " + $FileName + ".zip
		echo 'Rscript demo.R' > shell.sh
		chmod +x shell.sh
		screen -q -s ./shell.sh"
		$name = "cmd" + $i.Replace(' ', '') + ".sh"
		[string]$outputFile = [System.IO.Path]::Combine($name)
		Add-Content $outputFile "$commandText"
		Write-Host "wrote $i "
		putty -i C:\Users\qaz\Desktop\SourceTree_DataScience\AWS\Keys\key.ppk ec2-user@$i -m $name -t
}

$procs | Wait-Process
>>>>>>> 4cd6c9069c6c04d3d4c2551e591d58bdaf1b2b04
