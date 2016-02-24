{ pkgs ? import <nixpkgs> {} }:

with pkgs;
with lib;

let
  buildsbt = map (splitString " := ") (splitString "\n\n" (builtins.readFile ./build.sbt));
  version = builtins.replaceStrings ["\""] [""] (last (findFirst (p: (builtins.head p) == "version") "" buildsbt));
  sbtVersion = last (splitString "=" (builtins.readFile ./project/build.properties));
  sbt = fetchurl {
    url = "http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/${sbtVersion}/sbt-launch.jar";
    sha256 = "04k411gcrq35ayd2xj79bcshczslyqkicwvhkf07hkyr4j3blxda";
  };
in

stdenv.mkDerivation rec {
  name = "chee-${version}";

  src = ./.;

  buildInputs = [ jdk git ];

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
  '';

  installPhase = ''
    mkdir -p $out/{bin,program}
    cp -R target/${name}/* $out/ #*/
    sed -i "s,\$(dirname \$0),$out,g" $out/chee
    mv $out/chee $out/bin/chee
  '';

  meta = with stdenv.lib; {
    description = "Chee is a command line tool for managing photos.";
    homepage = https://github.com/eikek/chee;
    license = licenses.gpl3;
  };
}
