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

links

jrpc schema
https://playground.open-rpc.org/
https://github.com/MauDagos/cl-jschema

iced
https://github.com/iced-rs/iced/tree/latest/examples
https://docs.rs/iced/latest/iced/
https://docs.rs/iced/latest/iced/#the-pocket-guide
https://docs.rs/iced/latest/iced/widget/index.html
https://github.com/fogarecious/iced_tutorial/blob/main/README.md
https://github.com/iced-rs/awesome-iced?tab=readme-ov-file#Resources

## iced development

Element::explain
-> apply this to any Element to highlight

e.g.
iced::Element::new(button("Start scan").on_press(UIEvent::Command(UICommand::StartScan))).explain(iced::Color::BLACK),

``` rust
fn make_button<'a>( (label, msg): (&'a str, UICommand) ) -> iced::widget::Button<'a, UICommand> {
    button(label)
    .width(iced::Length::Fill) // Makes the button fill its share of the Row
    .on_press(msg)
}
macro_rules! btn {
    (($label:expr, $msg:expr)) => {
        button(
            $label
        )
        .width(iced::Length::Fill) // This forces the buttons to expand equally
        .on_press($msg)
    };
}
```

slint
https://docs.slint.dev/latest/docs/rust/slint/
https://docs.slint.dev/latest/docs/slint/guide/development/debugging_techniques/
https://slintpad.com/?gz=H4sIAAAAAAAACq1UsW6DMBDd-QqLOaladWmTKWkqtVLVoUTpkGZwsAOW4A7ZF0ET8e-1AwxVgKKKJwbje7y7sx-n0gw1sbPHLBZ7PFKQKKDJ5X15JEKo1hupSYU8WWJRbQTEQXAt1nyfyI2SebW9lgU9C2UVSnbQmDLfkJjmSkSSzI1x4v7c82RxyRuizQ8SiC2y7FOBwLyupcn3xr9tUfWmwwtqdUKgllDvdw0yLoSCaMbubrNifhU2GQ97wikvXDMUO0Irozq0lsQOZE9nxnx7drZ5E3LwrwVK75-amI0s-YQAMqTxBJcIYjy1lTJhT4U9Gf40yZW5OyqprfDQbgWHWKoottXed1NCTI4pmBnbtoYdzowUJdI2vRBCS2N8Vk4GsD-C4HUg9Z2nciB1xYl3UnftTWrMezvc7gbLtdilmTod15QqmDYX0fHbOhzsWJkadbI9PnZxau9RrAyzD2cJRsxONfkF1jKMA1Is9QBD_l6VXun9AK5TvmSMBQAA


    VerticalLayout / HorizontalLayout: The children are placed along the vertical or horizontal axis.
    GridLayout: The children are placed in a grid of columns and rows.

    min-width
    min-height
    max-width
    max-height
    preferred-width
    preferred-height

    spacing: This controls the spacing between the children.
    padding: This specifies the padding within the layout, the space between the elements and the border of the layout.

https://docs.slint.dev/latest/docs/slint/guide/language/coding/positioning-and-layouts/

https://github.com/slint-ui/slint/tree/master/examples/todo

;; (defun gattc-print (table)
;;   (with-output-to-string (os)
;;     (format os "| Handle .~%")
;;     (format os "| ~%")
;;   (loop for attribute in table
;;         do (format os "~A" table)

how to get char end?
- attempt to read next list handle
-> need a NEXT-HANDLE function that takes a handle + the whole list
-> or we could flatten the list first?
  -> but then we need to flatten it _again_ after descs
  -> orrr we have a "splice" function

;; This needs to
(defun group-by-uuid-size (attributes)
  (sort attributes (lambda (a b)
                     (not (eql
                           (type-of (getf a :uuid))
                           (type-of (getf b :uuid)))))))

(group-by-uuid-size
 (list
  (gatts-make-char-value +gatt-uuid-heart-rate-measurement+
                         (list :read #'read-spy :write #'write-spy)
                         "1")
  (gatts-make-char-value '(1 2 3 4 5 6)
                         (list :read #'read-spy :write #'write-spy)
                         "2")
  (gatts-make-char-value '(1 2 3 4 5 7)
                         (list :read #'read-spy :write #'write-spy)
                         "3")
  (gatts-make-char-value +gatt-uuid-heart-rate-measurement+
                         (list :read #'read-spy :write #'write-spy)
                         "4")
  (gatts-make-char-value '(1 2 3 4 5 9)
                         (list :read #'read-spy :write #'write-spy)
                         "5")))

;; FIXME: dispatch instead of expecting
(defun wait-for-discovery-by-peer (hci conn)
  (wait-for-service-discovery hci conn)
  (wait-for-characteristic-discovery hci conn)
  (wait-for-cccd-discovery hci conn)
  (wait-for-cccd-write hci conn)
  )


    "initializeCommand": "mkdir -p ${localWorkspaceFolder}/.cache && touch ${localWorkspaceFolder}/.cache/.bash_history",
    "onCreateCommand": "git config --global --add safe.directory '*'",
    "updateContentCommand": "/workspaces/zephyr/.devcontainer/scripts/setup-env.sh && /workspaces/zephyr/.devcontainer/scripts/setup-bsim.sh",

    "mounts": [
        "source=${localWorkspaceFolder}/.cache/.bash_history,target=/home/user/.bash_history,type=bind,consistency=cached",
        "source=/tmp,target=/tmp,type=bind,consistency=cached"
    ]

    "image": "ghcr.io/jori-nordic/zephyr-bsim-cache:2024-09-12",

(defun smp-cmac-spy (key &rest texts)
  (format t "SMP-CMAC: key ~X text ~X~%"
          (reverse key)
          (apply #'append
                 (mapcar (lambda (x) (reverse x)) texts))))

(setq sly-lisp-implementations
      '((sbcl ("sbcl"))))

(defun get-selected-tab (nb)
  (let ((path (ng:with-read-data (nil)
                (ng:format-wish "senddata [~a select]" (ng:widget-path nb))
                (ng:read-data))))
    (when (and path (string/= path ""))
      path)))

(defun print-addr-without-making-ltk-freak-out (address)
  "ltk is afraid of too many colons. true story."
  ;; Imma switch to a better toolkit soon anyways.
  ;;
  ;; Some resources on string quoting just in case:
  ;; https://stackoverflow.com/questions/5302120/general-string-quoting-for-tcl
  ;; https://wiki.tcl-lang.org/page/Tcl+Quoting
  ;; https://www.tutorialspoint.com/tcl-tk/tcl_strings.htm
  (format nil "~{~2,'0X~}"
