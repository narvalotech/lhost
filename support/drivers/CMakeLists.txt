# SPDX-License-Identifier: Apache-2.0

if (CONFIG_SERIAL)
    zephyr_library()
    zephyr_library_sources_ifdef(CONFIG_UART_POSIX_PIPE uart_posix_pipe.c)
endif()
