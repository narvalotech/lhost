#!/usr/bin/env bash

# We don't want to execute if we have a build error

this_dir=$(west topdir)/lhost/support

image_name=$1

if [[ -n "$image_name" ]]; then
    set -eu

    # Build specified image
    pushd ${this_dir}/firmware/$image_name
    west build -b nrf52_bsim
    popd
else
    set -eu
    # Build all images

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
fi
