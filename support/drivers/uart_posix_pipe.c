/*
 * Copyright (c) 2024 Nordic Semiconductor ASA
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#define DT_DRV_COMPAT zephyr_posix_pipe_uart

#include <stdbool.h>

#include <zephyr/drivers/uart.h>
#include <zephyr/kernel.h>

#include "cmdline.h" /* native_posix command line options header */
#include "posix_native_task.h"

#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>

#include <stdio.h>
#include <stdlib.h>

#include <zephyr/logging/log.h>
LOG_MODULE_REGISTER(uart_pipe, LOG_LEVEL_INF);

#define RETRY_DELAY K_MSEC(CONFIG_UART_POSIX_PIPE_RETRY_MS)

/*
 * UART driver for POSIX ARCH based boards.
 *
 * It communicates using UNIX named pipes (ie. FIFOs).
 */

struct nu_async_ep {
    uint8_t *buf;
    size_t len;
    size_t pos;
    struct k_timer expiry;
};

struct nu_isr_ep {
    bool enabled;
    /* FIXME: this is _really_ confusing. Use a proper state machine dammit. */
    /* For TX:
     * - 1: driver has not yet sent `c`
     * - 0: driver is waiting for data
     *
     * For RX:
     * - 1: driver has not yet rx'd into c
     * - 0: driver is waiting for app to read c
     * */
    bool pending;
    uint8_t c;
};

struct nu_isr {
    struct nu_isr_ep tx;
    struct nu_isr_ep rx;
    struct k_timer timer;
    uart_irq_callback_user_data_t cb;
    void *ud;
};

struct nu_state {
    const struct device *dev;    /* pointer to parent */
    char *tx_fifo_path;
    char *rx_fifo_path;
    int tx_fd;
    int rx_fd;
#ifdef CONFIG_UART_ASYNC_API
    struct nu_async_ep tx;
    struct nu_async_ep rx;
    struct k_timer timer;
    uart_callback_t cb;
    void *ud;
#endif
#ifdef CONFIG_UART_INTERRUPT_DRIVEN
    struct nu_isr isr;
#endif /* CONFIG_UART_INTERRUPT_DRIVEN */
};
#ifdef CONFIG_UART_INTERRUPT_DRIVEN
static void nu_isr_timer_work(struct k_timer *timer);
#endif /* CONFIG_UART_INTERRUPT_DRIVEN */

static int open_busywait(char *path, int flags)
{
    int ret = -1;
    while (ret < 0) {
        ret = open(path, flags);
        if (ret < 0) {
            k_busy_wait(1);
        }
    }

    return ret;
}

static int open_fifo(char *path, int flags)
{
    /* Wait for other side to connect */
    int fd = open_busywait(path, flags | O_NONBLOCK);

    if (fd < 0) {
        LOG_ERR("Failed to open pipe %s: err %d",
            path, errno);
        k_oops();
    }

    LOG_DBG("Opened FIFO %s", path);

    return fd;
}

static int nu_init(const struct device *dev)
{
    struct nu_state *s = (struct nu_state *)dev->data;

    s->rx_fd = open_fifo(s->rx_fifo_path, O_RDONLY);
    s->tx_fd = open_fifo(s->tx_fifo_path, O_WRONLY);

    if (s->tx_fd < 0 || s->rx_fd < 0) {
        return -1;
    }

#ifdef CONFIG_UART_INTERRUPT_DRIVEN
    memset(&s->isr, 0, sizeof(struct nu_isr));

    k_timer_init(&s->isr.timer, nu_isr_timer_work, NULL);
    k_timer_user_data_set(&s->isr.timer, s);
#endif    /* CONFIG_UART_INTERRUPT_DRIVEN */

    LOG_DBG("Initialized FIFO UART\n");

    return 0;
}

/* FIXME: panic if fifo peer has disconnected */
static void nu_poll_out(const struct device *dev, unsigned char c)
{
    struct nu_state *s = (struct nu_state *)dev->data;
    int ret = -1;

    LOG_DBG("%c", c);

    while (ret < 0) {
        ret = write(s->tx_fd, (uint8_t *)&c, sizeof(c));
        if (ret < 0) {
            /* printk("write: %d, waiting...\n", ret); */
            k_msleep(1);
        }
    }
}

static int nu_poll_in(const struct device *dev, unsigned char *c)
{
    struct nu_state *s = (struct nu_state *)dev->data;
    int ret = -1;

    LOG_DBG("");

    ret = read(s->rx_fd, (uint8_t *)c, sizeof(*c));
    if (ret < 0) {
        return -errno;
    }

    return 0;
}

#ifdef CONFIG_UART_ASYNC_API

static void handle_rx_done(struct nu_state *s, bool complete)
{
    struct uart_event evt;
    uint8_t *buf = s->rx.buf;
    size_t len = s->rx.len;

    LOG_DBG("%s", complete ? "complete" : "not-complete");

    /* allow rxing more from callback */
    s->rx.buf = NULL;
    s->rx.len = 0;
    s->rx.pos = 0;
    k_timer_stop(&s->rx.expiry);

    memset(&evt, 0, sizeof(evt));

    evt.type = UART_RX_RDY;
    evt.data.rx.buf = buf;
    evt.data.rx.offset = 0;
    if (complete) {
        evt.data.rx.len = len;
    } else {
        evt.data.rx.len = 0;
    }
    s->cb(s->dev, &evt, s->ud);

    memset(&evt, 0, sizeof(evt));

    evt.type = UART_RX_BUF_RELEASED;
    evt.data.rx_buf.buf = buf;
    s->cb(s->dev, &evt, s->ud);

    memset(&evt, 0, sizeof(evt));

    evt.type = UART_RX_DISABLED;
    s->cb(s->dev, &evt, s->ud);
}

static void handle_tx_done(struct nu_state *s, bool complete)
{
    struct uart_event evt;

    LOG_DBG("%s", complete ? "complete" : "not-complete");

    memset(&evt, 0, sizeof(evt));

    if (complete) {
        evt.type = UART_TX_DONE;
        evt.data.tx.buf = s->tx.buf;
        evt.data.tx.len = s->tx.len;
    } else {
        evt.type = UART_TX_ABORTED;
        evt.data.tx.buf = s->tx.buf;
        evt.data.tx.len = 0;
    }

    /* allow txing more from callback */
    s->tx.buf = NULL;
    s->tx.len = 0;
    s->tx.pos = 0;
    k_timer_stop(&s->tx.expiry);

    s->cb(s->dev, &evt, s->ud);
}

static void nu_expiry_work(struct k_timer *timer)
{
    struct nu_state *s = (struct nu_state *)k_timer_user_data_get(timer);

    __ASSERT_NO_MSG(s);

    LOG_DBG("");

    /* Figure out if we timed out on RX or TX */
    if (timer == &s->rx.expiry) {
        handle_rx_done(s, false);
    } else if (timer == &s->tx.expiry) {
        handle_tx_done(s, false);
    } else {
        __ASSERT(0, "Spanish inquisition timer");
    }
}

static void nu_timer_work(struct k_timer *timer)
{
    struct nu_state *s = (struct nu_state *)k_timer_user_data_get(timer);
    int ret;

    __ASSERT_NO_MSG(s);

    LOG_DBG("rx %p %d tx %p %d",
        s->rx.buf, s->rx.len, s->tx.buf, s->tx.len);

    /* TODO: handle disconnect from pipe */

    /* Try to RX first */
    if (s->rx.buf) {
        ret = read(s->rx_fd,
               s->rx.buf + s->rx.pos,
               s->rx.len - s->rx.pos);

        if (ret >= 0) {
            s->rx.pos += ret;
            LOG_DBG("read %d out of %d", s->rx.pos, s->rx.len);
        }

        if (s->rx.pos == s->rx.len) {
            handle_rx_done(s, true);
        } else {
            LOG_DBG("rx buf %p len %d ret %d", s->rx.buf, s->rx.len, ret);
            k_timer_start(timer, RETRY_DELAY, K_FOREVER);
        }
    }

    /* Then try to TX */
    if (s->tx.buf) {
        ret = write(s->tx_fd,
                s->tx.buf + s->tx.pos,
                s->tx.len - s->tx.pos);

        if (ret >= 0) {
            s->tx.pos += ret;
            LOG_DBG("wrote %d out of %d", s->tx.pos, s->tx.len);
        }

        if (s->tx.pos == s->tx.len) {
            handle_tx_done(s, true);
        } else {
            LOG_DBG("tx buf %p len %d ret %d", s->tx.buf, s->tx.len, ret);
            k_timer_start(timer, RETRY_DELAY, K_FOREVER);
        }
    }
}

static int nu_callback_set(const struct device *dev, uart_callback_t callback, void *user_data)
{
    struct nu_state *s = (struct nu_state *)dev->data;

    s->dev = dev;
    s->cb = callback;
    s->ud = user_data;

    LOG_DBG("cb %p ud %p", callback, user_data);

    /* TODO: move this to DTS macro */
    memset(&s->tx, 0, sizeof(struct nu_async_ep));
    memset(&s->rx, 0, sizeof(struct nu_async_ep));

    k_timer_init(&s->timer, nu_timer_work, NULL);
    k_timer_user_data_set(&s->timer, s);

    k_timer_init(&s->rx.expiry, nu_expiry_work, NULL);
    k_timer_user_data_set(&s->rx.expiry, s);

    k_timer_init(&s->tx.expiry, nu_expiry_work, NULL);
    k_timer_user_data_set(&s->tx.expiry, s);

    return 0;
}

static int nu_tx(const struct device *dev, const uint8_t *buf, size_t len, int32_t timeout)
{
    struct nu_state *s = (struct nu_state *)dev->data;

    LOG_DBG("buf %p len %p to %dus", buf, len, timeout);

    LOG_HEXDUMP_WRN(buf, len, "---------->");

    if (s->tx.buf) {
        LOG_DBG("already: buf %p len %d", s->tx.buf, s->tx.len);

        /* only one transaction supported at a time */
        return -EALREADY;
    }

    s->tx.buf = (uint8_t *)buf;
    s->tx.len = len;
    s->tx.pos = 0;

    /* Always TX from ISR context */
    k_timer_start(&s->timer, K_NO_WAIT, K_NO_WAIT);
    if (timeout != SYS_FOREVER_US) {
        k_timer_start(&s->tx.expiry, K_USEC(timeout), K_NO_WAIT);
    }

    return 0;
}

static int nu_tx_abort(const struct device *dev)
{
    /* 'sup */
    return -ENOTSUP;
}

static int nu_rx_enable(const struct device *dev, uint8_t *buf, size_t len, int32_t timeout)
{
    struct nu_state *s = (struct nu_state *)dev->data;

    LOG_DBG("buf %p len %p to %dus", buf, len, timeout);

    if (s->rx.buf) {
        LOG_DBG("already: buf %p len %d", s->rx.buf, s->rx.len);

        /* only one transaction supported at a time */
        return -EALREADY;
    }

    s->rx.buf = buf;
    s->rx.len = len;
    s->rx.pos = 0;

    /* Always RX from ISR context */
    k_timer_start(&s->timer, K_NO_WAIT, K_NO_WAIT);
    if (timeout != SYS_FOREVER_US) {
        k_timer_start(&s->rx.expiry, K_USEC(timeout), K_NO_WAIT);
    }

    return 0;
}

static int nu_rx_buf_rsp(const struct device *dev, uint8_t *buf, size_t len)
{
    /* 'sup */
    return -ENOTSUP;
}

static int nu_rx_disable(const struct device *dev)
{
    /* 'sup */
    return -ENOTSUP;
}
#endif    /* CONFIG_UART_ASYNC_API */

#ifdef CONFIG_UART_INTERRUPT_DRIVEN
static void nu_isr_timer_work(struct k_timer *timer)
{
    struct nu_state *s = (struct nu_state *)k_timer_user_data_get(timer);
    int ret;

    LOG_DBG("");

    __ASSERT_NO_MSG(s);
    __ASSERT_NO_MSG(s->isr.cb);

    if (s->isr.rx.pending) {
        LOG_DBG("rx-pending");
        ret = read(s->rx_fd, &s->isr.rx.c, 1);
        if (ret == 1) {
            s->isr.rx.pending = false;
            /* FIXME: that probably drops data */
            if (s->isr.rx.enabled) {
                s->isr.cb(s->dev, s->isr.ud);
            } else {
                s->isr.rx.pending = true;
            }
        } else {
            LOG_DBG("restart RX timer");
            k_timer_start(timer, RETRY_DELAY, K_FOREVER);
        }
    }

    if (s->isr.tx.pending) {
        LOG_DBG("tx-pending");
        ret = write(s->tx_fd, &s->isr.tx.c, 1);
        if (ret == 1) {
            s->isr.tx.pending = false;
            if (s->isr.tx.enabled) {
                s->isr.cb(s->dev, s->isr.ud);
                s->isr.tx.pending = true;
            }
        } else {
            LOG_DBG("restart RX timer");
            k_timer_start(timer, RETRY_DELAY, K_FOREVER);
        }
    }

    /* If none are pending, do nothing? Application should then call
     * irq_enable, which should trigger a callback into the app
     * immediately for getting/setting data.
     */
}

static void nu_irq_callback_set(const struct device *dev,
                uart_irq_callback_user_data_t cb,
                void *user_data)
{
    struct nu_state *s = (struct nu_state *)dev->data;

    LOG_DBG("cb %p ud %p", cb, user_data);

    s->isr.cb = cb;
    s->isr.ud = user_data;
}

static int nu_irq_fifo_read(const struct device *dev, uint8_t *rx_data, const int len)
{
    struct nu_state *s = (struct nu_state *)dev->data;

    LOG_DBG("data %p len %d", rx_data, len);

    if (s->isr.rx.pending) {
        LOG_DBG("pending");
        /* We haven't yet RXd the last byte */
        return 0;
    }

    __ASSERT_NO_MSG(len);
    rx_data[0] = s->isr.rx.c;
    s->isr.rx.pending = true;

    /* trigger RX loop */
    k_timer_start(&s->isr.timer, K_NO_WAIT, K_NO_WAIT);

    return 1;
}

static int nu_irq_fifo_fill(const struct device *dev, const uint8_t *tx_data, int len)
{
    struct nu_state *s = (struct nu_state *)dev->data;

    LOG_HEXDUMP_DBG(tx_data, len, "");

    if (s->isr.tx.pending) {
        LOG_DBG("pending");
        /* We haven't yet TXd the last byte */
        return 0;
    }

    __ASSERT_NO_MSG(len);
    s->isr.tx.c = tx_data[0];
    s->isr.tx.pending = true;

    /* trigger TX loop */
    k_timer_start(&s->isr.timer, K_NO_WAIT, K_NO_WAIT);

    return 1;
}

static void nu_irq_tx_enable(const struct device *dev)
{
    struct nu_state *s = (struct nu_state *)dev->data;

    LOG_DBG("");

    s->isr.tx.enabled = true;

    bool want_data = !s->isr.tx.pending;
    if (want_data) {
        s->isr.cb(dev, s->isr.ud);
    }
}

static void nu_irq_tx_disable(const struct device *dev)
{
    struct nu_state *s = (struct nu_state *)dev->data;

    LOG_DBG("");

    s->isr.tx.enabled = false;
}

static int nu_irq_tx_ready(const struct device *dev)
{
    struct nu_state *s = (struct nu_state *)dev->data;

    /* will this result in a dummy byte on start? */

    LOG_DBG("pending %d enabled %d", !s->isr.tx.pending, s->isr.tx.enabled);

    return !s->isr.tx.pending && s->isr.tx.enabled;
}

static int nu_irq_tx_complete(const struct device *dev)
{
    LOG_DBG("");

    return -ENOSYS;
}

static void nu_irq_rx_enable(const struct device *dev)
{
    struct nu_state *s = (struct nu_state *)dev->data;

    LOG_DBG("");

    s->isr.rx.enabled = true;

    bool have_data = !s->isr.rx.pending;
    if (have_data) {
        s->isr.cb(dev, s->isr.ud);
    }
}

static void nu_irq_rx_disable(const struct device *dev)
{
    struct nu_state *s = (struct nu_state *)dev->data;

    LOG_DBG("");

    s->isr.rx.enabled = false;
}

static int nu_irq_rx_ready(const struct device *dev)
{
    struct nu_state *s = (struct nu_state *)dev->data;

    /* will this result in a dummy byte on start? */

    LOG_DBG("pending %d enabled %d", !s->isr.rx.pending, s->isr.rx.enabled);

    return !s->isr.rx.pending && s->isr.rx.enabled;
}

static void nu_irq_err_enable(const struct device *dev)
{
    LOG_DBG("");

    /* no errors here */
}

static void nu_irq_err_disable(const struct device *dev)
{
    LOG_DBG("");

    /* no errors here */
}

static int nu_irq_is_pending(const struct device *dev)
{
    struct nu_state *s = (struct nu_state *)dev->data;

    LOG_DBG("%d", !s->isr.tx.pending || !s->isr.rx.pending);

    return (!s->isr.tx.pending && s->isr.tx.enabled) || (!s->isr.rx.pending && s->isr.rx.enabled);
}

static int nu_irq_update(const struct device *dev)
{
    /* clear the isr flags in fifo fns */
    LOG_DBG("");

    return 1;
}
#endif    /* CONFIG_UART_INTERRUPT_DRIVEN */

static struct uart_driver_api nu_api = {
    .poll_out = nu_poll_out,
    .poll_in = nu_poll_in,
#ifdef CONFIG_UART_ASYNC_API
    .callback_set = nu_callback_set,
    .tx = nu_tx,
    .tx_abort = nu_tx_abort,
    .rx_enable = nu_rx_enable,
    .rx_buf_rsp = nu_rx_buf_rsp,
    .rx_disable = nu_rx_disable,
#endif    /* CONFIG_UART_ASYNC_API */
#ifdef CONFIG_UART_INTERRUPT_DRIVEN
    .fifo_fill = nu_irq_fifo_fill,
    .irq_tx_enable = nu_irq_tx_enable,
    .irq_tx_disable = nu_irq_tx_disable,
    .irq_tx_ready = nu_irq_tx_ready,
    .irq_tx_complete = nu_irq_tx_complete,

    .fifo_read = nu_irq_fifo_read,
    .irq_rx_enable = nu_irq_rx_enable,
    .irq_rx_disable = nu_irq_rx_disable,
    .irq_rx_ready = nu_irq_rx_ready,

    .irq_err_enable = nu_irq_err_enable,
    .irq_err_disable = nu_irq_err_disable,

    .irq_is_pending = nu_irq_is_pending,
    .irq_update = nu_irq_update,
    .irq_callback_set = nu_irq_callback_set,
#endif    /* CONFIG_UART_INTERRUPT_DRIVEN */
};

#define UART_NATIVE_CMDLINE_ADD(n)                                      \
    static void nu_##n##_extra_cmdline_opts(void)                       \
    {                                                                   \
        static struct args_struct_t nu_##n##_opts[] = {                 \
        {                                                               \
        .option = "fifo_" #n "_rx",                                     \
        .name = "\"path\"",                                             \
        .type = 's',                                                    \
        .dest = (void *)&nu_##n##_state.rx_fifo_path,                   \
        .descript = "Full path to FIFO used for serial driver: host -> device" \
    },                                                                  \
        {                                                               \
        .option = "fifo_" #n "_tx",                                     \
        .name = "\"path\"",                                             \
        .type = 's',                                                    \
        .dest = (void *)&nu_##n##_state.tx_fifo_path,                   \
        .descript = "Full path to FIFO used for serial driver: device -> host" \
    },                                                                  \
        ARG_TABLE_ENDMARKER                                             \
    };                                                                  \
                                                                        \
        native_add_command_line_opts(nu_##n##_opts);                    \
    }                                                                   \
                                                                        \
    static void nu_##n##_cleanup(void)                                  \
    {                                                                   \
        close(nu_##n##_state.rx_fd);                                    \
        close(nu_##n##_state.tx_fd);                                    \
    }                                                                   \
                                                                        \
    NATIVE_TASK(nu_##n##_extra_cmdline_opts, PRE_BOOT_1, 11);           \
    NATIVE_TASK(nu_##n##_cleanup, ON_EXIT, 99);                         \


#define UART_NATIVE_PIPE_DEFINE(n)                      \
                                                        \
    static struct nu_state nu_##n##_state;              \
                                                        \
    UART_NATIVE_CMDLINE_ADD(n);                         \
                                                        \
    DEVICE_DT_INST_DEFINE(n,                            \
                          &nu_init,                     \
                          NULL,                         \
                          &nu_##n##_state,              \
                          NULL,                         \
                          POST_KERNEL,                  \
                          CONFIG_SERIAL_INIT_PRIORITY,  \
                          &nu_api);

DT_INST_FOREACH_STATUS_OKAY(UART_NATIVE_PIPE_DEFINE)
