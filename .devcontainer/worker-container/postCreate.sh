#!/bin/sh

set -ex

# create virtualenv inside project
poetry config virtualenvs.in-project true

# install python packages
poetry install
