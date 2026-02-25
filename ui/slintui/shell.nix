{ pkgs ? import <nixpkgs> { } }:

let
  # These are the libraries that Slint/Winit try to load at runtime
  runtimeLibs = with pkgs; [
    libxkbcommon
    wayland
    vulkan-loader
    libGL
    # If you use X11, you might also need these:
    # xorg.libX11
    # xorg.libXcursor
    # xorg.libXi
    # xorg.libXrandr
  ];

in pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    pkg-config
    cargo
    rustc
    rustup
    rustfmt
    rust-analyzer
  ];

  buildInputs = with pkgs; [
    fontconfig
    freetype
    expat
  ] ++ runtimeLibs;

  # This makes the libraries available to the linker
  env.RUSTFLAGS = "-C link-arg=-Wl,-rpath,${pkgs.lib.makeLibraryPath runtimeLibs}";

  # This makes the libraries available at RUNTIME (critical for dlopen)
  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath runtimeLibs}:$LD_LIBRARY_PATH"
  '';
}
