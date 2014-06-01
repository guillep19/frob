#!/bin/sh
echo "Copying files.."
mkdir -p /var/www/kits
cp -rf ./* /var/www/kits
service apache2 restart
