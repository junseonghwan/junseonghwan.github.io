#!/bin/bash -l

echo "$(date) Running on: $(hostname)"

wget $1 -O $2

echo "$(date) File download complete."
