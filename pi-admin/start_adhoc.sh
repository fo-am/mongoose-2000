#!/bin/bash
sudo ifconfig wlan0 up
sleep 5
sudo /usr/sbin/hostapd /etc/hostapd/hostapd.conf &
sleep 5
sudo ifconfig wlan0 up
sleep 5
sudo /usr/sbin/hostapd /etc/hostapd/hostapd.conf &

