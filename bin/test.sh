#!/usr/bin/env bash

SCRIPT_DIR=$(dirname $0)

STREAMLY_VERSION=0.8.0
BENCH_REPORT_DIR=benchmark/bench-report
source $SCRIPT_DIR/targets.sh

#------------------------------------------------------------------------------
# Script
#------------------------------------------------------------------------------

print_help () {
  echo "Usage: $0 "
  echo "       [--targets <"target1 target2 ..." | help>]"
  echo "       [--with-compiler <compiler exe name>]"
  echo "       [--cabal-build-options <option>]"
  echo "       [--dev-build]"
  echo "       [--coverage]"
  echo "       [--hpc-report-options <option>]"
  echo "       [--no-measure]"
  echo "       [--raw]"
  echo "       [--rtsopts <option>]"
  echo "       -- <hspec options or test names>"
  echo
  echo "--targets: targets to run, use 'help' for list of targets"
  echo "--cabal-build-options: Pass any cabal build options to be used for build"
  echo "--dev-build: runs some additional tests"
  echo "--coverage: enable coverage and report coverage info"
  echo "--hpc-report-options: option for 'hpc report'"
  echo "--no-measure: with --coverage, do not run tests, only show coverage info"
  echo "--raw: with --coverage, do not run hpc"
  echo "--rtsopts: pass GHC RTS options to the test executable"
  echo
  echo "Any arguments after a '--' are passed directly to hspec"
  exit
}

#-----------------------------------------------------------------------------
# Read command line
#-----------------------------------------------------------------------------

RUNNING_TESTS=y
source $BENCH_REPORT_DIR/bin/build-lib.sh

USE_GIT_CABAL=1
set_common_vars
COVERAGE=0
MEASURE=1
HPC_REPORT_OPTIONS=
RAW=0

# XXX add a bisect option
while test -n "$1"
do
  case $1 in
    -h|--help|help) print_help ;;
    # options with arguments
    --targets) shift; TARGETS=$1; shift ;;
    --with-compiler) shift; CABAL_WITH_COMPILER=$1; shift ;;
    --cabal-build-options) shift; CABAL_BUILD_OPTIONS=$1; shift ;;
    --hpc-report-options) shift; HPC_REPORT_OPTIONS=$1; shift ;;
    --rtsopts) shift; RTS_OPTIONS=$1; shift ;;
    # flags
    --raw) RAW=1; shift ;;
    #--slow) SLOW=1; shift ;; # not implemented
    #--quick) QUICK_MODE=1; shift ;; # not implemented
    --dev-build) RUNNING_DEVBUILD=1; shift ;;
    --coverage) COVERAGE=1; shift ;;
    --no-measure) MEASURE=0; shift ;;
    --) shift; break ;;
    -*|--*) echo "Unknown flags: $*"; echo; print_help ;;
    *) break ;;
  esac
done
TARGET_EXE_ARGS=$*

set_derived_vars

#-----------------------------------------------------------------------------
# Determine targets
#-----------------------------------------------------------------------------

# Defined in targets.sh
targets

if test "$(has_item "$TARGETS" help)" = "help"
then
  list_target_groups
  list_targets
  exit
fi

DEFAULT_TARGETS="$(all_grp)"
TARGETS=$(set_targets)

echo "Using targets [$TARGETS]"

#-----------------------------------------------------------------------------
# Build targets
#-----------------------------------------------------------------------------

test_exe_rts_opts () {
  case "$1" in
    # XXX Data.Array.* heap requirement increased for GHC-8.10
    Data.Array.Foreign) echo -n "-M128M" ;;
    Data.Array.Prim) echo -n "-M128M" ;;
    Data.Array.Prim.Pinned) echo -n "-M128M" ;;
    # For -O0 case writeChunks test fails, maybe we should have a separate flag
    # for O0 case?
    FileSystem.Handle) echo -n "-K16M" ;;
    *) if test "$COVERAGE" -eq "1"
       then
          echo -n "-K8M -M1024M"
        else
          echo -n "-K8M -M64M"
       fi ;;
  esac
}

# $1: bench name
# $2: bench executable
target_exe_extra_args () {
  local bench_name=$1
  local bench_prog=$2

  echo "+RTS \
    $(test_exe_rts_opts $(basename $bench_prog)) \
    $RTS_OPTIONS \
    -RTS"
}

if test "$COVERAGE" -eq "1"
then
  # Used to determine the hpc tix dir
  PACKAGE_FULL_NAME=streamly-$STREAMLY_VERSION
  case `uname` in
    Linux) SYSTEM=x86_64-linux ;;
    *) echo "Unsupported system"; exit 1 ;;
  esac

  # With the --enable-coverage option the tests as well as the library get
  # compiled with -fhpc, and we get coverage for tests as well. But we want to
  # exclude that, so a project file is needed.
  CABAL_BUILD_OPTIONS+=" --project-file cabal.project.coverage"
  mkdir -p $BUILD_DIR/hpc
fi
BUILD_TEST="$CABAL_EXECUTABLE v2-build $CABAL_BUILD_OPTIONS --enable-tests"

if test "$MEASURE" -eq "1"
then
run_build "$BUILD_TEST" streamly-tests test "$TARGETS"
fi

#-----------------------------------------------------------------------------
# Run targets
#-----------------------------------------------------------------------------

# $1: target name
get_tix_file () {
  echo $BUILD_DIR/build/$SYSTEM/ghc-${GHC_VERSION}/$PACKAGE_FULL_NAME/hpc/vanilla/tix/$1/$1.tix
}

# $1: package name
# $2: component
# $3: target
# $4: args generator func
run_target () {
  local package_name=$1
  local component=$2
  local target_name=$3
  local extra_args=$4

  local target_prog
  target_prog=$(cabal_target_prog $package_name $component $target_name) || \
    die "Cannot find executable for target $target_name"

  echo "Running executable $target_name ..."

  # Needed by bench-exec-one.sh
  export BENCH_EXEC_PATH=$target_prog
  mkdir -p $(dirname $(get_tix_file $target_name))
  export HPCTIXFILE=$(get_tix_file $target_name)

  run_verbose $target_prog $($extra_args $target_name $target_prog) \
    || die "Target exe failed"

  # hpc-coveralls fails if there is an empty dir and no .tix file generated
  rmdir $(dirname $(get_tix_file $target_name)) 2>/dev/null || true
}

# $1: package name with version
# $2: component
# $3: targets
# $4: args generator func
run_targets() {
    for i in $3
    do
      run_target $1 $2 $i $4
    done
}

if test "$MEASURE" -eq "1"
then
run_targets "streamly-tests-0.0.0" t "$TARGETS" target_exe_extra_args
fi

#-----------------------------------------------------------------------------
# Run coverage reports
#-----------------------------------------------------------------------------

PACKAGE_NAME=streamly-$STREAMLY_VERSION
MIX_DIR=$BUILD_DIR/build/$SYSTEM/ghc-${GHC_VERSION}/$PACKAGE_NAME/hpc/vanilla/mix/$PACKAGE_NAME/
ALLTIX=$BUILD_DIR/hpc/all.tix

if test "$COVERAGE" -eq "1" -a "$RAW" -eq 0
then
  TIXFILES=
  for i in $TARGETS
  do
    tixfile="$(get_tix_file ${i})"
    if test -f "$tixfile"
    then
      TIXFILES+="$tixfile "
    fi
  done

  hpc sum --output=$ALLTIX $TIXFILES
  run_verbose hpc markup $ALLTIX --hpcdir $MIX_DIR
  run_verbose hpc report $ALLTIX $HPC_REPORT_OPTIONS --hpcdir $MIX_DIR
fi
