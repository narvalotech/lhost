# LHost: Toy bluetooth host in common lisp

## What is it?

A toy implementation of the Bluetooth LE host in common lisp.
It's very buggy, use at your own risk.

For now, it talks to a virtualized controller that uses the [Babblesim](https://babblesim.github.io/#content) simulator.

## Why???

- I like image-based development
- Dead languages don't have painful updates
- I wanted to quickly experiment ideas for [my old job](https://github.com/jori-nordic)
- I now want custom tooling for [my current job](https://www.garmin.com/en-CA/)

## How to run

- Open vscode in the repo root
- Trigger "open in container" (usually a popup)
- In a new vscode terminal: `./support/build.sh && ./support/run-mitm.sh`
- Eval `host.lisp` in emacs
- Eval `script.lisp` in emacs
- Check the REPL for output

## How it works

- Babblesim runs fully inside the container
- Bsim has two devices:
  - a peripheral
  - a controller with a home-rolled UNIX FIFO uart driver
- `/tmp` is mounted in the container, the fifo is created there
- `host.lisp` connects to that FIFO and talks HCI-H4 protocol

## Features

- role-agnostic gatt server and client
- central bonding (JW + LESC only)
- peripheral bonding (ditto)
- MITM of a just-works LESC link
  - tested with phone + HRM sensor among others
- hcisnoop logs
- basic UI
- pretty much no restrictions / api checks
  - very hackable
  - also very brittle

## Connecting to the real world

See [host.lisp](./host.lisp) there's a socat command somewhere.
Basically you flash a nRF dongle with the zephyr controller and off you go.
