#!/bin/bash

ip netns | grep tap | xargs -n 1 ip netns delete 

