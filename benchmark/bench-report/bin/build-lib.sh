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

# We run the benchmarks in isolation in a separate process so that different
# benchmarks do not interfere with other. To enable that we need to pass the
# benchmark exe path to gauge as an argument. Unfortunately it cannot find its
# own path currently.

# The path is dependent on the architecture and cabal version.

# $1: package name
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

# XXX cabal issue "cabal v2-exec which" cannot find benchmark/test executables

# $1: builddir
# $2: package name
# $3: component ("" (lib), t (test), b (benchmark), x (executable))
# $4: command to find
cabal_which_builddir() {
  local path=$(echo $1/build/*/ghc-${GHC_VERSION}/${2}-0.0.0/$3/$4/build/$4/$4)
  test -f "$path" && echo $path
}

# $1: package name
# $2: component
# $3: command to find
cabal_which() {
  cabal_which_builddir $BUILD_DIR $1 $2 $3
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

# $1: package name
# $2: component
# $3: targets
# $4: args generator func
run_targets() {
    for i in $3
    do
      run_target $1 $2 $i $4
    done
}
