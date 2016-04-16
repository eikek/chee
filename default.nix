{ pkgs ? import <nixpkgs> {} }:

with pkgs;
with lib;

let
  buildsbt = map (splitString " := ") (splitString "\n\n" (builtins.readFile ./build.sbt));
  version = builtins.replaceStrings ["\""] [""] (last (findFirst (p: (builtins.head p) == "version") "" buildsbt));
  sbtVersion = last (splitString "=" (builtins.readFile ./project/build.properties));
  sbt = fetchurl {
    url = "http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/${sbtVersion}/sbt-launch.jar";
    sha256 = "1h8b23v891dqzb36pgzs7h44bc3rrr9j6z6w7f70vam7lwsxwfn6";
  };
  cask = stdenv.mkDerivation rec {
    version = "0.7.4";
    name = "cask-${version}";
    src = fetchurl {
      url = "https://github.com/cask/cask/archive/v${version}.tar.gz";
      name = "cask-src-git-${version}.tar.gz";
      sha256 = "0za3in46qf02fd5gsficphgr0df3xicbf0pl8285q8gwa0ffm0xi";
    };
    buildPhase = "true";
    patchPhase = ''
      sed -i 's,/usr/bin/env python,${python}/bin/python,g' bin/cask
    '';
    installPhase = ''
      mkdir -p $out
      cp -r * $out/
      mv $out/bin/cask $out/bin/_cask
      cat > $out/bin/cask <<-"EOF"
      #!${bash}/bin/bash
      if ! ${which}/bin/which emacs > /dev/null 2>&1 && [ -z "$EMACS" ];
      then
          export EMACS=${emacs}/bin/emacs
      fi
      $(dirname $0)/_cask "$@"
      EOF
      chmod 755 $out/bin/cask
    '';
  };
in

stdenv.mkDerivation rec {
  name = "chee-${version}";

  src = ./.;

  buildInputs = [ jdk git cask ];

  patchPhase = ''
    echo "" >> build.sbt
    echo "javaBin in script := \"${jdk}/bin/java\"" >> build.sbt
    echo "" >> build.sbt
    echo "assemblyDir in script := \"program\"" >> build.sbt
  '';

  buildPhase = ''
    mkdir {_tmp,_home}
    export SBT_OPTS="-XX:PermSize=190m -Dsbt.boot.directory=_sbt/boot/ -Dsbt.ivy.home=_sbt/ivy2/ -Dsbt.global.base=_sbt/"
    export CHEE_OPTS="-Dchee.workingdir=_home/chee -Dchee.configdir=_home/chee"
    ${jdk}/bin/java $SBT_OPTS $CHEE_OPTS -Duser.home=_home -jar ${sbt} make-zip
    cd emacs
    export HOME=.
    ${cask}/bin/cask build
    cd ..
  '';

  installPhase = ''
    mkdir -p $out/{bin,program}
    cp -r target/${name}/* $out/program #*/
    ln -s $out/program/chee $out/bin/chee

    mkdir -p $out/share/emacs/site-lisp
    cp -R emacs/{*.el,*.elc} $out/share/emacs/site-lisp/
  '';

  meta = with stdenv.lib; {
    description = "Chee is a command line tool for managing photos.";
    homepage = https://github.com/eikek/chee;
    license = licenses.gpl3;
  };
}
