{ pkgs ? import <nixpkgs> { } }:

let
  runtimeLibs = with pkgs; [
    libxkbcommon
    wayland
    vulkan-loader
    libGL
    stdenv.cc.cc.lib

    # Technically runtime, but using mold forces us
    # to also have them at build-time.
    fontconfig
    freetype
    expat
  ];

in pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    pkg-config
    cargo
    rustc
    rustfmt
    rust-analyzer
    mold
  ];

  buildInputs = runtimeLibs;

  env.RUSTFLAGS = (
    "-C link-arg=-fuse-ld=mold " +
    "-C link-arg=-Wl,-rpath,${pkgs.lib.makeLibraryPath runtimeLibs}"
  );

  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath runtimeLibs}:$LD_LIBRARY_PATH"
  '';
}
