#!/bin/bash

ip link set dev tap0 up
ip addr add 10.0.0.1/24 dev tap0
ip neigh add 10.0.0.100 lladdr 56:c0:71:c0:2f:ee nud permanent dev tap0

ip netns add tap1
ip netns add tap2

ip link set netns tap1 dev tap1
ip netns exec tap1 ip link set dev lo up
ip netns exec tap1 ip link set dev tap1 up
ip netns exec tap1 ip addr add 10.0.0.2/24 dev tap1

ip link set netns tap2 dev tap2
ip netns exec tap2 ip link set dev lo up
ip netns exec tap2 ip link set dev tap2 up
ip netns exec tap2 ip addr add 10.0.0.3/24 dev tap2

