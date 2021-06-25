#!/usr/bin/env bash

# See dev/ci-tests.md

# TODO: (1) Detect if nix is available otherwise run with plain cabal,
# (2) Detect the platform and run tests applicable to the platform, (3)
# add a test for windows/msys

SCRIPT_DIR=$(cd `dirname $0`; pwd)
source $SCRIPT_DIR/build-lib.sh

#------------------------------------------------------------------------------
# Prime version (GHC 8.10)
#------------------------------------------------------------------------------

GHC_PRIME=ghc8104
GHC_PRIME_VER="8.10"
JOBS=1

# Without cabal project file, it will not run any tests
ghc_prime_dist () {
  nix-shell \
    --argstr compiler "$GHC_PRIME" \
    --run "\
      packcheck.sh cabal-v2 \
      GHCVER=$GHC_PRIME_VER \
      CABAL_BUILD_OPTIONS="--jobs=$JOBS" \
      CABAL_DISABLE_DEPS=y \
      CABAL_CHECK_RELAX=y"
}

# With cabal project file, it will run the tests
ghc_prime_dist_tests () {
  nix-shell \
    --argstr compiler "$GHC_PRIME" \
    --run "\
      packcheck.sh cabal-v2 \
      GHCVER=$GHC_PRIME_VER \
      CABAL_BUILD_OPTIONS="--jobs=$JOBS" \
      CABAL_PROJECT=cabal.project \
      CABAL_DISABLE_DEPS=y \
      CABAL_CHECK_RELAX=y"
}

# build-all, Werror, test, inspection, fusion-plugin. Note, inspection
# requires fusion-plugin.

PERF_FLAGS="--flag inspection --flag fusion-plugin"

ghc_prime_perf () {
  nix-shell \
    --argstr compiler "$GHC_PRIME" \
    --argstr c2nix "--flag inspection" \
    --run "\
      bin/bench.sh --cabal-build-options \
        \"--jobs=$JOBS --project-file cabal.project.Werror $PERF_FLAGS\" --quick --raw;\
      bin/test.sh --cabal-build-options \
        \"--jobs=$JOBS --project-file cabal.project.Werror $PERF_FLAGS\";"
}

ghc_prime_O0 () {
  nix-shell \
    --argstr compiler "$GHC_PRIME" \
    --run "\
      bin/test.sh --cabal-build-options \
        \"--jobs=$JOBS --project-file cabal.project.O0\""
}

#------------------------------------------------------------------------------
# Check warnings, docs
#------------------------------------------------------------------------------

# XXX avoid rebuilding if nothing has changed in the source
# XXX run hlint only on changed files
lint () {
  packcheck cabal \
    HLINT_OPTIONS="lint --cpp-include=src --cpp-include=test"  \
    HLINT_TARGETS="src test benchmark"
}

werror () {
  nix-shell \
    --argstr compiler "$GHC_PRIME" \
    --run "cabal build --project-file cabal.project.Werror-nocode all"
}

ghc_prime_Werror () {
  nix-shell \
    --argstr compiler "$GHC_PRIME" \
    --run "cabal build --jobs=$JOBS --project-file cabal.project.Werror all"
}

ghc_prime_doctests () {
  nix-shell \
    --argstr compiler "$GHC_PRIME" \
    --run "cabal build --jobs=$JOBS --project-file cabal.project.doctest all"
  cabal-docspec --timeout 60
}

#------------------------------------------------------------------------------
# coverage
#------------------------------------------------------------------------------

# To upload the results to coveralls.io using hpc-coveralls
# hpc-coveralls --repo-token="$REPO_TOKEN" --coverage-mode=StrictlyFullLines

ghc_prime_coverage () {
  nix-shell \
    --argstr compiler "$GHC_PRIME" \
    --run "bin/test.sh --cabal-build-options \"--jobs=$JOBS\" --coverage"
}

#------------------------------------------------------------------------------
# Flags
#------------------------------------------------------------------------------

ghc_prime_dev () {
  nix-shell \
    --argstr compiler "$GHC_PRIME" \
    --run "bin/test.sh --cabal-build-options \"--jobs=$JOBS --flag dev\""
}

ghc_prime_c_malloc () {
  nix-shell \
    --argstr compiler "$GHC_PRIME" \
    --run "bin/test.sh --cabal-build-options \"--jobs=$JOBS --flag use-c-malloc\""
}

ghc_prime_debug () {
  nix-shell \
    --argstr compiler "$GHC_PRIME" \
    --run "bin/test.sh --cabal-build-options \"--jobs=$JOBS --flag debug\""
}

ghc_prime_streamk () {
  nix-shell \
    --argstr compiler "$GHC_PRIME" \
    --run "bin/test.sh --cabal-build-options \"--jobs=$JOBS --flag streamk\""
}

#------------------------------------------------------------------------------
# Other GHC versions
#------------------------------------------------------------------------------

# build-all only
# $1 883
# $2 8.8.3
ghc () {
  nix-shell \
    --argstr compiler "ghc$1" \
    --run "\
      packcheck.sh cabal-v2 \
      GHCVER=$2 \
      CABAL_BUILD_OPTIONS="--jobs=$JOBS" \
      CABAL_DISABLE_DEPS=y \
      CABAL_CHECK_RELAX=y \
      DISABLE_SDIST_BUILD=y \
      DISABLE_TEST=y \
      DISABLE_DOCS=y"
}

ghc901 () { ghc 901 9.0.1; }
ghc884 () { ghc 884 8.8.4; }

#------------------------------------------------------------------------------
# GHC Head
#------------------------------------------------------------------------------

ghcHEAD () {
  nix-shell \
    --argstr compiler "ghcHEAD" \
    --run "\
      packcheck.sh cabal-v2 \
      GHCVER=9.3 \
      CABAL_BUILD_OPTIONS="--jobs=$JOBS" \
      CABAL_CHECK_RELAX=y \
      DISABLE_SDIST_BUILD=y \
      CABAL_BUILD_OPTIONS=\"--allow-newer --project-file cabal.project.ghc-head"
}

#------------------------------------------------------------------------------
# ghcjs
#------------------------------------------------------------------------------

ghcjs () {
      export PATH=~/.local/bin:/opt/ghc/bin:/opt/ghcjs/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH
      packcheck.sh cabal-v2 \
        GHCVER=8.4.0 \
        CABAL_BUILD_OPTIONS="--jobs=$JOBS" \
        CABAL_CHECK_RELAX=y \
        DISABLE_SDIST_BUILD=y \
        DISABLE_TEST=y \
        DISABLE_DOCS=y \
        ENABLE_GHCJS=y
}

#-----------------------------------------------------------------------------
# Read command line
#-----------------------------------------------------------------------------

RELEASE_CI_TARGETS="\
ghc_prime_dist \
ghc_prime_dist_tests \
ghc_prime_perf \
ghc_prime_O0 \
ghc_prime_Werror \
ghc_prime_doctests \
ghc_prime_coverage \
ghc_prime_c_malloc \
ghc_prime_debug \
ghc_prime_streamk \
ghc901 \
ghc884 \
ghcjs \
lint"

ALL_TARGETS="\
$RELEASE_CI_TARGETS \
ghc_prime_dev \
ghcHEAD \
werror"

print_targets () {
  echo "Available targets: all $ALL_TARGETS"
}

print_help () {
  echo "Usage: $0 --targets <space separated target names>"
  print_targets
  exit
}

RELEASE_CI=0

while test -n "$1"
do
  case $1 in
    # flags
    -h|--help|help) print_help ;;
    --release) RELEASE_CI=1; shift; break ;;
    # options with arguments
    --targets) shift; TARGETS=$1; shift ;;
    --) shift; break ;;
    -*|--*) echo "Unknown flags: $*"; echo; print_help ;;
    *) break ;;
  esac
done

if test -z "$TARGETS"
then
  if test "$RELEASE_CI" -eq 1
  then
    TARGETS="$RELEASE_CI_TARGETS"
  else
    print_help
  fi
else
  if test "$RELEASE_CI" -eq 1
  then
    print_help
  fi
fi

for i in $TARGETS
do
  if test "$(has_item "$ALL_TARGETS" $i)" != "$i"
  then
    echo "Unrecognized target: $i"
    print_help
  fi
done

if test "$(has_item "$TARGETS" help)" = "help"
then
  print_targets
  exit
fi

if test "$(has_item "$TARGETS" all)" = "all"
then
  TARGETS="$ALL_TARGETS"
fi

for i in $TARGETS
do
    $i || { echo "$i failed."; exit 1; }
done
