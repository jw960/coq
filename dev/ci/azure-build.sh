#!/bin/bash

set -e -x

cd $(dirname $0)/../..

make coq coqide-server
