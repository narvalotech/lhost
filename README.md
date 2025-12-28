# LHost: Toy bluetooth host in common lisp

## What is it?

A toy implementation of the Bluetooth LE host in common lisp.
It's really broken, please don't use it.

For now, it talks to a virtualized controller that uses the [Babblesim](https://babblesim.github.io/#content) simulator.

## Why???

- I like image-based development
- I want to quickly experiment ideas for [my day job](https://github.com/jori-nordic)
- Dead languages don't have painful updates
- I want a job at google, they seem to love rewriting BT hosts

## How to run

- Open vscode in the repo root
- Trigger "open in container" (usually a popup)
- In a new vscode terminal: `./support/brun.sh`
- Eval `host.lisp` in emacs
- Check the REPL for output

## How it works

- Babblesim runs fully inside the container
- Bsim has two devices:
  - a peripheral
  - a controller with a home-rolled UNIX FIFO uart driver
- `/tmp` is mounted in the container, the fifo is created there
- `host.lisp` connects to that FIFO and talks HCI-H4 protocol

## How about a real device

Who needs real devices when you got bsim 😁
