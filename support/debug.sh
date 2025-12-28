#!/usr/bin/env bash
# It's ok if the FIFO already exists
set +eu

# The FIFO pair the lisp program uses to communicate with the Bluetooth
# controller device.
uart_h2c=/tmp/lhost/uart.h2c
uart_c2h=/tmp/lhost/uart.c2h

mkdir -p $(dirname ${uart_h2c})
mkfifo ${uart_h2c}
mkfifo ${uart_c2h}
# We don't want to execute if we have a build error
set -eu

this_dir=$(west topdir)/lhost/support
central="${this_dir}/firmware/peripheral/build/zephyr/zephyr.exe"

# Cleanup all existing sims
${BSIM_COMPONENTS_PATH}/common/stop_bsim.sh

echo "Start PHY"
pushd "${BSIM_OUT_PATH}/bin"
./bs_2G4_phy_v1 -s=lisp-id -D=3 -dump_imm &

echo "Slow down sim"
pushd "${BSIM_COMPONENTS_PATH}/device_handbrake"
./bs_device_handbrake -s=lisp-id -d=2 -r=1 &

# This talks to the lisp program
hci_uart="${this_dir}/firmware/hci_sim/build/zephyr/zephyr.exe"
$hci_uart \
    -s=lisp-id -d=1 -RealEncryption=0 -rs=70 \
    -fifo_0_rx=${uart_h2c} \
    -fifo_0_tx=${uart_c2h} &

echo "Start debug server on central device"
gdbserver :2345 $central -s=lisp-id -d=0 &

# Give some time for server to start up
sleep 0.5
