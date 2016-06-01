.\0_create_new_instances.ps1
cat IPs.txt | .\1_putty_init.ps1
.\2_zip_S3_local.ps1
aws configure
.\3_zip_S3_AWS.ps1
