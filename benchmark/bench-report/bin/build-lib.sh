# $1: message
die () {
  >&2 echo -e "Error: $1"
  exit 1
}

warn () {
  >&2 echo -e "Warning: $1"
}

# $1: command
function run_verbose() {
  echo "$*"
  bash -c "$*"
}

has_item () {
  for i in $1
  do
    if test "$i" = "$2"
    then
      echo "$i"
      break
    fi
  done
}

#------------------------------------------------------------------------------
# target groups
#------------------------------------------------------------------------------

test_only () {
  if test -n "$RUNNING_TESTS"
  then
    echo $1
  fi
}

bench_only () {
  if test -n "$RUNNING_BENCHMARKS"
  then
    echo $1
  fi
}

dev_build () {
  if test -n "$RUNNING_DEVBUILD"
  then
    echo $1
  fi
}

# A group consisting of all known individual targets
all_grp () {
  { for i in $GROUP_TARGETS
    do
      for j in $(eval "echo \$$i")
      do
        echo $j
      done
    done
    for i in $INDIVIDUAL_TARGETS
    do
      echo $i
    done
  } | sort | uniq
}

# All groups
all_target_groups () {
  echo $GROUP_TARGETS
}

# XXX pass as arg
list_targets ()  {
  echo "Individual targets:"
  for i in $(all_grp)
  do
    echo "$i"
  done
  echo
}

# XXX pass as arg
list_target_groups ()  {
  echo "All Targets: all_grp"
  echo "Target groups:"
  for i in $(all_target_groups)
  do
    echo -n "$i ["
    eval "echo -n \$$i"
    echo "]"
  done
  echo
}

# XXX pass as arg
set_targets() {
  if test -z "$TARGETS"
  then
    echo $DEFAULT_TARGETS
  else
    for i in $(echo $TARGETS)
    do
        case $i in
          *_grp) eval "echo -n \$${i}" ;;
          *_cmp) eval "echo -n \$${i} $i" ;;
          *) echo -n $i ;;
        esac
        echo -n " "
    done
  fi
}

# XXX cabal issue "cabal v2-exec which" cannot find benchmark/test executables

# $1: builddir
# $2: package name with version
# $3: component ("" (lib), t (test), b (benchmark), x (executable))
# $4: command to find
cabal_which_builddir() {
  local path=$(echo $1/build/*/ghc-${GHC_VERSION}/${2}/$3/$4/build/$4/$4)
  test -f "$path" && echo $path
}

# $1: package name with version
# $2: component
# $3: command to find
cabal_which() {
  cabal_which_builddir $BUILD_DIR $1 $2 $3
}

# We run the benchmarks in isolation in a separate process so that different
# benchmarks do not interfere with other. To enable that we need to pass the
# benchmark exe path to gauge as an argument. Unfortunately it cannot find its
# own path currently.

# The path is dependent on the architecture and cabal version.

# $1: package name with version
# $2: component
# $3: target
cabal_target_prog () {
  local target_prog=`cabal_which $1 $2 $3`
  if test -x "$target_prog"
  then
    echo $target_prog
  else
    return 1
  fi
}

set_common_vars () {
  SLOW=0
  QUICK_MODE=0

  RUNNING_DEVBUILD=

  TARGET_EXE_ARGS=
  RTS_OPTIONS=

  CABAL_BUILD_OPTIONS=""
  CABAL_EXECUTABLE=cabal

  # Use branch specific builds if git-cabal is present in PATH
  BUILD_DIR=dist-newstyle
  if test "$USE_GIT_CABAL" -eq 1
  then
    if which git-cabal 2>/dev/null
    then
        echo "Using git-cabal for branch specific builds"
        CABAL_EXECUTABLE=git-cabal
        BUILD_DIR=$(git-cabal show-builddir)
      fi
  fi
}

# To be called after parsing CLI arguments
set_derived_vars () {
  if test -n "$CABAL_WITH_COMPILER"
  then
    CABAL_BUILD_OPTIONS+=" --with-compiler $CABAL_WITH_COMPILER"
  else
    CABAL_WITH_COMPILER=ghc
  fi

  GHC_VERSION=$($CABAL_WITH_COMPILER --numeric-version)
}

# $1: build program
# $2: package name
# $3: component prefix
# $4: targets
run_build () {
  local build_prog=$1
  local package=$2
  local component_prefix=$3
  local COMPONENTS
  local c

  for c in $4
  do
    COMPONENTS+="$package:$component_prefix:$c "
  done
  run_verbose $build_prog $COMPONENTS || die "build failed"
}
