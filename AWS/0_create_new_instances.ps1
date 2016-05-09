ec2-run-instances ami-183a2b72 -n 1 --instance-type t2.micro -k key
ec2-describe-instances > describe_instances.csv
ec2-describe-spot-instance-requests > spot_instances.csv