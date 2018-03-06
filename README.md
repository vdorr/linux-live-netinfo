![Build Status](https://travis-ci.org/vdorr/linux-live-netinfo.svg?branch=master)

linux-live-netinfo library and program

Provides real time updates on state of network neighbourhood (NICs, IPs, ARP table) of Linux system.

This is work in progress and quite experimental.

to send ping on linux (that is, to use raw sock), certain capabilities are required:
    sudo setcap cap_net_raw+pe FILE
or run as superuser, the former is strongly advised

