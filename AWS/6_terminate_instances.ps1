[cmdletbinding()]
param (
	[Parameter(Mandatory = $true, ValueFromPipeline=$true)]
	[string[]] $InputArray
)

foreach ($i in $input){
	aws ec2 terminate-instances --instance-ids $i --region us-east-1

}