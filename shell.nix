{ pkgs ? import <nixpkgs> {} }:

let
  libraryPaths = with pkgs; [
    openssl
    sqlite
    libffi
    zlib
    libjpeg_turbo  # Provides libturbojpeg.so
    tcl
    tk
    xorg.libX11
    libGL
    libGLU
    SDL2
    SDL2_image
    SDL2_ttf
  ];

  sbclrcFile = pkgs.writeText "temp-sbclrc" ''
    (require :asdf)
    (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
      (when (probe-file quicklisp-init)
        (load quicklisp-init)))
  '';

  # This creates a 'fixed' SBCL that always knows where your Nix libraries are
  sbcl-wrapped = pkgs.stdenv.mkDerivation {
    name = "sbcl-wrapped";
    buildInputs = [ pkgs.makeWrapper ];
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out/bin
      makeWrapper ${pkgs.sbcl}/bin/sbcl $out/bin/sbcl \
        --add-flags "--load ${sbclrcFile}" \
        --set LD_LIBRARY_PATH "${pkgs.lib.makeLibraryPath libraryPaths}" \
        --set CPATH "${pkgs.lib.makeSearchPathOutput "dev" "include" libraryPaths}" \
        --set SBCL_HOME "${pkgs.sbcl}/lib/sbcl"
    '';
  };

in
pkgs.mkShell {
  buildInputs = with pkgs; [
    sbcl-wrapped
    pkg-config
    gcc
  ] ++ libraryPaths;

  shellHook = ''
    echo "Wrapped SBCL ready with libturbojpeg baked in!"
  '';
}
