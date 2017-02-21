#!/bin/bash
#
# Fix tomcat file permissions following update.
# If successful, restart should print 'OK'
sudo chmod 644 /var/lib/tomcat7/conf/{context,tomcat-users,server,web}.xml
sudo /etc/init.d/tomcat7 restart
# End fix-tomcat.sh

