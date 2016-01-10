#!/bin/bash

createns () {
  NIC=$1
  INDEX=$2

  ip netns add $NIC
  ip link set netns $NIC dev $NIC
  ip netns exec $NIC ip link set dev lo up
  ip netns exec $NIC ip link set dev $NIC up
  ip netns exec $NIC ip addr add 10.0.0.$INDEX/24 dev $NIC
}

ip link set dev tap0 up
ip addr add 10.0.0.1/24 dev tap0
ip neigh add 10.0.0.100 lladdr 56:c0:71:c0:2f:ee nud permanent dev tap0

createns tap1 2
createns tap2 3
createns tap3 4
