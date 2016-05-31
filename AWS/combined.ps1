.\0_create_new_instances.ps1
cat IPs.txt | .\1_putty_init.ps1
.\2_zip_S3_local.ps1
aws configure
<<<<<<< HEAD
.\3_zip_S3_AWS.ps1
=======
.\3_zip_S3_AWS.ps1
>>>>>>> 4cd6c9069c6c04d3d4c2551e591d58bdaf1b2b04
