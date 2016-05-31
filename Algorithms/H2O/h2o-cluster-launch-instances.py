# Launch EC2 instances with an IAM role
# --------------------------------------
# Change either but not both the IAM Profile Name.
iam_profile_resource_name = None
iam_profile_name = 'testing_role'

# SSH key pair name.
keyName = 'testing_key'
securityGroupName = 'SecurityDisabled'

numInstancesToLaunch = 2
instanceType = 't1.micro'
instanceNameRoot = 'amy_is_testing'

regionName = 'us-east-1'
amiId = 'ami-ed550784'