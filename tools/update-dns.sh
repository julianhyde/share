#!/bin/bash
# Updates DNS, if public IP address has changed.

# Set ZONEEDIT_USER, ZONEEDIT_PASSWORD, ZONEEDIT_HOSTS.
. ~/.cron-env

prev_ip=
if [ -f /tmp/ip.txt ]; then
    prev_ip=$(cat /tmp/ip.txt)
fi
ip=$(dig +short myip.opendns.com @resolver1.opendns.com)
if [ "$ip" != "$prev_ip" -o $(date +%H) -eq 22 ]; then
  for h in $(echo ${ZONEEDIT_HOSTS} | sed -e 's/,/ /g'); do
    echo update $h to $ip
    sleep 1
    curl -s -u "${ZONEEDIT_USER}:${ZONEEDIT_PASSWORD}" "https://dynamic.zoneedit.com/auth/dynamic.html?host=${h}&dnsto="
  done
  echo $ip > /tmp/ip.txt
  echo "$(date -Is) ${ip}" >> ~/ip_history.txt
fi

# End update-dns.sh
