#!/usr/bin/env bash

# We don't want to execute if we have a build error
set -eu

this_dir=$(west topdir)/lhost/support

# Build controller image
pushd ${this_dir}/firmware/hci_sim
west build -b nrf52_bsim
popd

# Build peripheral image
pushd ${this_dir}/firmware/peripheral
west build -b nrf52_bsim
popd

# Build central image
pushd ${this_dir}/firmware/central
west build -b nrf52_bsim
popd
