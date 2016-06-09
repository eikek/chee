{ pkgs ? import <nixpkgs> {} }:

with pkgs.lib;

let
  versionsbt =  last (splitString ":=" (builtins.readFile ./version.sbt));
  version = builtins.replaceStrings [" " "\n" ''"'' ] ["" "" ""] versionsbt;
  cask = pkgs.stdenv.mkDerivation rec {
    version = "0.7.4";
    name = "cask-${version}";
    src = pkgs.fetchurl {
      url = "https://github.com/cask/cask/archive/v${version}.tar.gz";
      name = "cask-src-git-${version}.tar.gz";
      sha256 = "0za3in46qf02fd5gsficphgr0df3xicbf0pl8285q8gwa0ffm0xi";
    };
    buildPhase = "true";
    patchPhase = ''
      sed -i 's,/usr/bin/env python,${pkgs.python}/bin/python,g' bin/cask
    '';
    installPhase = ''
      mkdir -p $out
      cp -r * $out/
      mv $out/bin/cask $out/bin/_cask
      cat > $out/bin/cask <<-"EOF"
      #!${pkgs.bash}/bin/bash
      if ! ${pkgs.which}/bin/which emacs > /dev/null 2>&1 && [ -z "$EMACS" ];
      then
          export EMACS=${pkgs.emacs}/bin/emacs
      fi
      $(dirname $0)/_cask "$@"
      EOF
      chmod 755 $out/bin/cask
    '';
  };
in

pkgs.stdenv.mkDerivation rec {
  name = "chee-${version}";

  src = ./.;

  buildInputs = with pkgs; [ jdk git cask ];

  patchPhase = ''
    echo "" >> build.sbt
    echo "javaBin in script := \"${pkgs.jre}/bin/java\"" >> build.sbt
  '';

  buildPhase = ''
    export HOME=.
    mkdir -p {_tmp,_home,_sbt}
    CHEE_OPTS="-Dchee.workingdir=$(pwd)/_home/chee -Dchee.configdir=$(pwd)/_home/chee"
    export SBT_OPTS="-Dfile.encoding=UTF-8 -Dsbt.boot.directory=$(pwd)/_sbt/boot/ -Dsbt.ivy.home=$(pwd)/_sbt/ivy2/ -Dsbt.global.base=$(pwd)/_sbt/ $CHEE_OPTS"
    ${pkgs.sbt}/bin/sbt make-zip

    cd emacs
    cask
    cask build
    ./run-tests.sh
    cd ..
  '';

  installPhase = ''
    mkdir -p $out/{bin,program}
    cp -r target/${name}/* $out/program #*/
    ln -s $out/program/chee $out/bin/chee

    mkdir -p $out/share/emacs/site-lisp
    cp -R emacs/{*.el,*.elc} $out/share/emacs/site-lisp/
  '';

  meta = {
    description = "Chee is a command line tool for managing photos.";
    homepage = https://github.com/eikek/chee;
    license = licenses.gpl3;
  };
}
