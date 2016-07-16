#!/bin/sh

cd $(dirname "$0")
if [[ ! -e "fomus.asd" ]]; then
    echo "ERROR: FOMUS install directory not found"
    exit 1
fi

if [[ -e "install.cfg" ]]; then
    source install.cfg
    BINDIRCFG=1
    LIBDIRCFG=1
fi

for A in $*
  do
  case $A in
      --uninstall)
	  UNINST=1
	  ;;
      --bindir=)
	  BINDIR=`echo $A | sed -e "s/^--bindir=//"`
	  BINDIRCFG=0
	  ;;
      --libdir=)
	  LIBDIR=`echo $A | sed -e "s/^--libdir=//"`
	  LIBDIRCFG=0
	  ;;
      --sbcl)
	  LISP=sbcl
	  ;;
      --cmucl)
	  LISP=cmucl
	  ;;
      --clisp)
	  LISP=clisp
	  ;;
      --openmcl)
	  LISP=openmcl
	  ;;
      --cm=*)
	  CMDIR=`echo $A | sed -e "s/^--cm=//"`
	  ;;
      --cmn=*)
	  CMNDIR=`echo $A | sed -e "s/^--cmn=//"`
	  ;;
      --prefix=*)
	  PREFIX=`echo $A | sed -e "s/^--prefix=//"`
	  if [[ "$BINDIRCFG" = '1' ]]; then
	      BINDIRCFG=0
	      BINDIR=''
	  fi
	  if [[ "$LIBDIRCFG" = '1' ]]; then
	      LIBDIRCFG=0
	      LIBDIR=''
	  fi
	  ;;
      *)
	  cat <<EOF
This installs FOMUS as a command-line executable.

Usage Examples:
./install.sh --sbcl                                                   --install using SBCL into /usr/local (you need to be root)
./install.sh --sbcl --cm=/mylispdir/cm                                --install using SBCL and include CM
./install.sh --sbcl --cm=                                             --install using SBCL and exclude CM
./install.sh --sbcl --cmn=/mylispdir/cmn                              --install using SBCL and include CMN
./install.sh --cmucl --prefix=/mybasedir                              --install using CMUCL into /mybaseinstalldir
./install.sh --cmucl --prefix=/mybasedir --bindir=/mybasedir/mybin    --install using CMUCL with special bin directory
./install.sh --openmcl --prefix=/mybasedir --libdir=/mybasedir/mylib  --install using OpenMCL with special lib directory
./install.sh --uninstall                                              --uninstall

Lisp options are --sbcl, --cmucl, --openmcl and --clisp.  Directories need to be specified as full pathnames.

Install options are stored in install.cfg and recalled automatically when install.sh is run again--they only need to be
specified once.

If you get stuck in Lisp while compiling, try the following command:
(cl-user::quit)
EOF
	  exit 2
	  ;;
  esac
done

saveopts () {
    (
	cat<<EOF
PREFIX="$PREFIX"
BINDIR="$BINDIR"
LIBDIR="$LIBDIR"
LISP="$LISP"
CMDIR="$CMDIR"
CMNDIR="$CMNDIR"
EOF
    ) > install.cfg
}

if [[ -z "$PREFIX" ]]; then
    PREFIX="/usr/local"
fi
if [[ -z "$BINDIR" ]]; then
    BINDIR="$PREFIX/bin"
fi
if [[ -z "$LIBDIR" ]]; then
    LIBDIR="$PREFIX/lib"
fi

if [[ "$UNINST" = '1' ]]; then
    echo "Uninstalling..."
    echo "rm -f $LIBDIR/fomus.img"
    rm -f $LIBDIR/fomus.img
    echo "rm -f $BINDIR/fomus"
    rm -f $BINDIR/fomus
    
    saveopts
    
    echo
    echo "Done!"
    exit 0
fi

if [[ -z "$LISP" ]]; then
    echo "ERROR: No Lisp was specified (use --sbcl, --cmucl or --openmcl, or type --help for more options)"
    exit 1
fi

LOADCM="src/cm.lisp"
LOADCMN="cmn-all.lisp"

if [[ ! -d '/tmp' ]]; then
    echo "ERROR: Can't find /tmp directory"
fi

case "$LISP" in
    sbcl)
	LISPEXE=$(which sbcl)
	LOADARG="--load"
	EVALARG="--eval"
	EXITCMD="(quit)"
	COREARG="--core"
	EXTRAARG="--noinform"
	DUMPCMD='(sb-ext:save-lisp-and-die "/tmp/fomus/fomus.img" :purify t)'
	;;
    cmucl)
	LISPEXE=$(which lisp)
	LOADARG="-load"
	EVALARG="-eval"
	EXITCMD="(quit)"
	COREARG="-core"
	EXTRAARG="-quiet"
	DUMPCMD='(ext:save-lisp "/tmp/fomus/fomus.img" :purify t)'
	;;
    openmcl)
	LISPEXE=$(which openmcl)
	LOADARG="-l"
	EVALARG="-e"
	EXITCMD="(quit)"
	COREARG="-I"
	DUMPCMD='(ccl:save-application "/tmp/fomus/fomus.img" :purify t)'
	;;
    clisp)
	LISPEXE=$(which clisp)
	EVALARG="-x"
	EXITCMD="(quit)"
	COREARG="-M"
	EXTRAARG="-q"
	DUMPCMD='(ext:saveinitmem "/tmp/fomus/fomus.img")'
	;;
esac

INSTFLAG='(intern "+FOMUS-INSTALL+" :common-lisp-user)'
CMKLUDGE='(intern "KEYSIG" :fomus)'
if [[ -e '/tmp/fomus' ]]; then rm -rf /tmp/fomus; fi
cp -pr . /tmp/fomus
find /tmp/fomus -regex '^.*\.\(fasl\|fas\|lib\|x86f\)$' -exec rm -f {} \;
if [[ "$LISP" = 'clisp' ]]; then
    $LISPEXE $EXTRAARG $EVALARG "(progn $INSTFLAG (load \"/tmp/fomus/load.lisp\") $EXITCMD)"
else
    $LISPEXE $EXTRAARG $EVALARG "$INSTFLAG" $LOADARG "/tmp/fomus/load.lisp" $EVALARG $EXITCMD
fi
if [[ -n "$CMNDIR" ]]; then
    if [[ -e '/tmp/fomus_cmn' ]]; then rm -rf /tmp/fomus_cmn; fi
    cp -pr "$CMNDIR" /tmp/fomus_cmn
    find /tmp/fomus_cmn -regex '^.*\.\(fasl\|fas\|lib\|x86f\)$' -exec rm -f {} \;
    if [[ "$LISP" = 'clisp' ]]; then
	$LISPEXE $EXTRAARG $EVALARG "(progn (load \"/tmp/fomus_cmn/$LOADCMN\") $EXITCMD)"
	INCCMN="(load \"/tmp/fomus_cmn/$LOADCMN\")"
    else
	$LISPEXE $EXTRAARG $LOADARG "/tmp/fomus_cmn/$LOADCMN" $EVALARG $EXITCMD
	INCCMN1=$LOADARG
	INCCMN2="/tmp/fomus_cmn/$LOADCMN"
    fi
fi
if [[ -n "$CMDIR" ]]; then
    if [[ -e '/tmp/fomus_cm' ]]; then rm -rf /tmp/fomus_cm; fi
    cp -pr "$CMDIR" /tmp/fomus_cm
    find /tmp/fomus_cm -regex '^.*\.\(fasl\|fas\|lib\|x86f\)$' -exec rm -f {} \;
    if [[ "$LISP" = 'clisp' ]]; then
	$LISPEXE $EXTRAARG $EVALARG "(progn $INCCMN $INSTFLAG (load \"/tmp/fomus/load.lisp\") $CMKLUDGE (load \"/tmp/fomus_cm/$LOADCM\") $EXITCMD)"
	INCCM="(load \"/tmp/fomus_cm/$LOADCM\")"
    else
	$LISPEXE $EXTRAARG $INCCMN1 $INCCMN2 $EVALARG "$INSTFLAG" $LOADARG "/tmp/fomus/load.lisp" $EVALARG "$CMKLUDGE" $LOADARG "/tmp/fomus_cm/$LOADCM" $EVALARG $EXITCMD
	INCCM1=$LOADARG
	INCCM2="/tmp/fomus_cm/$LOADCM"
    fi
fi
if [[ "$LISP" = 'clisp' ]]; then
    $LISPEXE $EXTRAARG $EVALARG "(progn $INCCMN $INSTFLAG (load \"/tmp/fomus/load.lisp\") $CMKLUDGE $INCCM $DUMPCMD $EXITCMD)"
else
    $LISPEXE $EXTRAARG $INCCMN1 $INCCMN2 $EVALARG "$INSTFLAG" $LOADARG "/tmp/fomus/load.lisp" $EVALARG "$CMKLUDGE" $INCCM1 $INCCM2 $EVALARG "$DUMPCMD"
fi

if [[ -e '/tmp/fomus_cmn' ]]; then rm -rf /tmp/fomus_cmn; fi
if [[ -e '/tmp/fomus_cm' ]]; then rm -rf /tmp/fomus_cm; fi
if [[ ! -e "/tmp/fomus/fomus.img" ]]; then
    echo "ERROR: Couldn't create FOMUS Lisp image :("
    exit 1
fi

(
    cat <<EOF
#!/bin/sh
usage () {
    echo "Usage: fomus [-lxfscmw] [-o basefilename] [-v value] [-q value] filename [filename]..."
    echo
    echo "  -l        Output to LilyPond"
    echo "  -x        Output to MusicXML"
    echo "  -f        Output to MusicXML for Finale"
    echo "  -s        Output to MusicXML for Sibelius"
    echo "  -c        Output to CMN"
    echo "  -m        Output to FOMUS input file"
    echo
    echo "  -w        View output"
    echo "  -o        Base filename (w/o extension)"
    echo "  -v        Verbosity level (0, 1 or 2, default = 1 or value in .fomus file)"
    echo "  -q        Quality value (real number, default = 1 or value in .fomus file)"
    echo
    echo "Report bugs to <fomus-devel@common-lisp.net>."
}
while getopts 'lxfscmwo:v:q:' opt; do
    case \$opt in
        [lxfscmw]) o="\$opt\$o";;
        o) n="\$OPTARG";;
        v) v="\$OPTARG";;
        q) q="\$OPTARG";;
        ?) usage; exit 2;;
    esac
done
shift \$((\$OPTIND - 1))
if [[ \$# -ne 1 ]]; then usage; exit 2; fi
fls="\"\$1\""
while [[ -n "\$2" ]]; do fls="\$fls \"\$2\""; shift; done
$LISPEXE $COREARG "$LIBDIR/fomus.img" $EXTRAARG $EVALARG "(fm::fomus-exe \"\$HOME/.fomus\" \"\$o\" \"\$n\" \"\$q\" \"\$v\" \$fls)" 2>/dev/null
EOF
) > /tmp/fomus/fomus.sh

echo "Installing..."
echo install -d $BINDIR
install -d $BINDIR
echo install -m 755 /tmp/fomus/fomus.sh $BINDIR/fomus
install -m 755 /tmp/fomus/fomus.sh $BINDIR/fomus
echo install -d $LIBDIR
install -d $LIBDIR
echo install -m 644 /tmp/fomus/fomus.img $LIBDIR
install -m 644 /tmp/fomus/fomus.img $LIBDIR

rm -rf /tmp/fomus
saveopts

echo
echo "Done!"
