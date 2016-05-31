[cmdletbinding()]
param (
<<<<<<< HEAD
    [Parameter(Mandatory = $true, ValueFromPipeline=$true)]
    [string[]] $InputArray
)

foreach ($i in $input) {
    Write-Host $i
    putty.exe ec2-user@$i -i C:\Users\qaz\Desktop\SourceTree_DataScience\AWS\Keys\key.ppk -t
}
=======
	[Parameter(Mandatory = $true, ValueFromPipeline=$true)]
	[string[]] $InputArray
)

foreach ($i in $input) {
	Write-Host $i
    putty.exe ec2-user@$i -i C:\Users\qaz\Desktop\SourceTree_DataScience\AWS\Keys\key.ppk -t
}
>>>>>>> 4cd6c9069c6c04d3d4c2551e591d58bdaf1b2b04
