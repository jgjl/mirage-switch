#!/bin/bash

ip link set dev nic0 up
ip link set dev nic1 up
ip link set dev nic2 up

ip addr add 10.0.0.1/24 dev nic0
ip addr add 10.1.0.1/24 dev nic1
ip addr add 10.2.0.1/24 dev nic2

ip neigh add 10.0.0.100 lladdr 56:c0:71:c0:2f:ee nud permanent dev nic0
