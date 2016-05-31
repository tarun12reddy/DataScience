[cmdletbinding()]
param (
<<<<<<< HEAD
    [Parameter(Mandatory = $true, ValueFromPipeline=$true)]
    [string[]] $InputArray
)

foreach ($i in $input){
    aws ec2 terminate-instances --instance-ids $i --region us-east-1

}
=======
	[Parameter(Mandatory = $true, ValueFromPipeline=$true)]
	[string[]] $InputArray
)

foreach ($i in $input){
	aws ec2 terminate-instances --instance-ids $i --region us-east-1

}
>>>>>>> 4cd6c9069c6c04d3d4c2551e591d58bdaf1b2b04
