{ pkgs ? import <nixpkgs> { } }:

let
  dlopenLibraries = with pkgs; [
    libxkbcommon

    # GPU backend
    vulkan-loader
    # libGL

    # Window system
    wayland
    # xorg.libX11
    # xorg.libXcursor
    # xorg.libXi
  ];

in pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    cargo
    rustc
    rustup
    rust-analyzer
  ];

  # additional libraries that your project
  # links to at build time, e.g. OpenSSL
  buildInputs = [];

  env.RUSTFLAGS = "-C link-arg=-Wl,-rpath,${pkgs.lib.makeLibraryPath dlopenLibraries}";
}
