# SPDX-License-Identifier: Apache-2.0

cmake_minimum_required(VERSION 3.20.0)

set(EXTRA_DTC_OVERLAY_FILE app.overlay)

find_package(Zephyr REQUIRED HINTS $ENV{ZEPHYR_BASE})

if (!CONFIG_NATIVE_BUILD)
    message(FATAL_ERROR "Only supported on bsim platforms, building natively. For now.")
endif()

project(hci_sim)

set(ORIG_DIR ${ZEPHYR_BASE}/samples/bluetooth/hci_uart_async)

target_sources(app PRIVATE
        ${ORIG_DIR}/src/hci_uart_async.c
        ${ORIG_DIR}/src/main.c
)

