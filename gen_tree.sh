#!/bin/bash

ind_dir=$1
bin=$2
out=$3

ls $ind_dir|awk '{n=split($1,subfd,"_");m=split(subfd[n],out,".");print out[1]}'|xargs $bin >$3
