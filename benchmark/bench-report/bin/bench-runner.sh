#!/usr/bin/env bash

# Note that this script is used in the "streamly" package as well as
# in "streaming-benchmarks" package. Any changes to the script should be
# generic enough so that it works in both the cases.

#------------------------------------------------------------------------------

set -o pipefail

SCRIPT_DIR=$(cd `dirname $0`; pwd)

RUNNING_BENCHMARKS=y
source $SCRIPT_DIR/build-lib.sh

print_help () {
  echo "Usage: $0 "
  echo "       [--benchmarks <"bench1 bench2 ..." | help>]"
  echo "       [--prefix <benchmark name prefix to match>"
  echo "       [--fields <"field1 field2 ..." | help>]"
  echo "       [--sort-by-name]"
  echo "       [--compare]"
  echo "       [--diff-style <absolute|percent|multiples>]"
  echo "       [--cutoff-percent <percent-value>]"
  echo "       [--graphs]"
  echo "       [--no-measure]"
  echo "       [--append]"
  echo "       [--long]"
  echo "       [--slow]"
  echo "       [--quick]"
  echo "       [--raw]"
  echo "       [--dev-build]"
  echo "       [--use-nix]"
  echo "       [--with-compiler <compiler exe name>]"
  echo "       [--cabal-build-options <options>]"
  echo "       [--rtsopts <opts>]"
  echo "       [--commit-compare] [--base <commit>] [--candidate <commit>]"
  #echo "       -- <gauge options or benchmarks>"
  echo
  echo "--benchmarks: benchmarks to run, use 'help' for list of benchmarks"
  echo "--compare: compare the specified benchmarks with each other"
  echo "--fields: measurement fields to report, use 'help' for a list"
  echo "--graphs: Generate graphical reports"
  echo "--no-measure: Don't run benchmarks, run reports from previous results"
  echo "--append: Don't overwrite previous results, append for comparison"
  echo "--long: Use much longer stream size for infinite stream benchmarks"
  echo "--slow: Slightly more accurate results at the expense of speed"
  echo "--quick: Faster results, useful for longer benchmarks"
  echo "--raw: Run the benchmarks but don't report them. This is useful when"
  echo "       you only want to work with the csv files generated."
  echo "--cabal-build-options: Pass any cabal build options to be used for build"
  echo "       e.g. --cabal-build-options \"--flag dev\""
  echo
  echo "When specific space complexity group is chosen then (and only then) "
  echo "RTS memory restrictions are used accordingly. For example, "
  echo "bench.sh --benchmarks Data.Parser -- Data.Parser/o-1-space "
  echo "restricts Heap/Stack space for O(1) characterstics"
  echo
  echo "When using --commit-compare, by default comparative chart of HEAD^ vs HEAD"
  echo "commit is generated, in the 'charts' directory."
  echo "Use --base and --candidate to select the commits to compare."
  echo
  #echo "Any arguments after a '--' are passed directly to gauge"
  exit
}

#-----------------------------------------------------------------------------
# Reporting utility functions
#-----------------------------------------------------------------------------

list_comparisons ()  {
  echo "Comparison groups:"
  for i in $COMPARISONS
  do
    echo -n "$i ["
    eval "echo -n \$$i"
    echo "]"
  done
  echo
}

build_report_prog() {
    local prog_path=$BENCH_REPORT_DIR/bin/bench-report

    hash -r
    if test ! -x "$prog_path" -a "$BUILD_ONCE" = "0"
    then
      echo "Building bench-report executables"
      BUILD_ONCE=1
      pushd $BENCH_REPORT_DIR
      local cmd
      cmd="$CABAL_EXECUTABLE install --installdir bin bench-report"
      if test "$USE_NIX" -eq 0
      then
        $cmd || die "bench-report build failed"
      else
        nix-shell --run "$cmd" || die "bench-report build failed"
      fi
      popd
    elif test ! -x "$prog_path"
    then
      return 1
    fi
    return 0
}

build_report_progs() {
  if test "$RAW" = "0"
  then
      build_report_prog || exit 1
      local prog
      prog=$BENCH_REPORT_DIR/bin/bench-report
      test -x $prog || die "Cannot find bench-report executable"
      echo "Using bench-report executable [$prog]"
  fi
}

# We run the benchmarks in isolation in a separate process so that different
# benchmarks do not interfere with other. To enable that we need to pass the
# benchmark exe path to gauge as an argument. Unfortunately it cannot find its
# own path currently.

# The path is dependent on the architecture and cabal version.

bench_output_file() {
    local bench_name=$1
    echo "charts/$bench_name/results.csv"
}

#------------------------------------------------------------------------------
# Speed options
#------------------------------------------------------------------------------

# tasty-bench does not like an option set twice
set_super_quick_mode () {
    echo -n super_quick
}

# For certain long benchmarks if the user has not requested super quick
# mode we anyway use a slightly quicker mode.
use_quicker_mode () {
  if test "$QUICK_MODE" -eq 0
  then
    echo quicker
  fi
}

#------------------------------------------------------------------------------
# Determine options from benchmark name
#------------------------------------------------------------------------------

# Global environment passed:
# BENCH_EXEC_PATH: the benchmark executable
# RTS_OPTIONS: additional RTS options
# QUICK_MODE: whether we are in quick mode
# USE_GAUGE: whether to use gauge or tasty-bench
# LONG: whether to use a large stream size

# $1: bench name
bench_exec_one() {
  local BENCH_NAME_ORIG
  BENCH_NAME_ORIG="$1"
  shift

  local SUPER_QUICK_OPTIONS
  local QUICKER_OPTIONS
  if test "$USE_GAUGE" -eq 0
  then
    SUPER_QUICK_OPTIONS="--stdev 1000000"
    QUICKER_OPTIONS="--stdev 100"
  else
    # Do not keep time limit as 0 otherwise GC stats may remain 0 in some cases.
    SUPER_QUICK_OPTIONS="--quick --min-duration 0 --time-limit 0.01 --include-first-iter"
    QUICKER_OPTIONS="--min-samples 3 --time-limit 1"
  fi

  local BENCH_NAME0
  local BENCH_NAME1
  local BENCH_NAME2
  local BENCH_NAME
  # XXX this is a hack to make the "/" separated names used in the functions
  # determining options based on benchmark name. For tasty-bench the benchmark
  # names are separated by "." instead of "/".
  if test "$USE_GAUGE" -eq 0
  then
    # Remove the prefix "All."
    BENCH_NAME0=$(echo $BENCH_NAME_ORIG | sed -e s/^All\.//)

    # Module names could contain dots e.g. "Prelude.Serial". So we insert
    # an explicit "/" to separate the module name part and the rest of
    # the benchmark name. For example, Prelude.Serial/elimination.drain
    BENCH_NAME1=$(echo $BENCH_NAME0 | cut -f1 -d '/')

    if test "$BENCH_NAME1" = "$BENCH_NAME0"
    then
      # There is no "/" separator
      BENCH_NAME1=$(echo $BENCH_NAME0 | sed -e 's/\./\//g')
      BENCH_NAME2=""
    else
      BENCH_NAME2=/$(echo $BENCH_NAME0 | cut -f2- -d '/' | sed -e 's/\./\//g')
    fi
    BENCH_NAME="${BENCH_NAME1}${BENCH_NAME2}"
  else
    BENCH_NAME=$BENCH_NAME_ORIG
  fi

  local RTS_OPTIONS1
  RTS_OPTIONS1="\
+RTS -T \
$(bench_rts_options $(basename $BENCH_EXEC_PATH) $BENCH_NAME) \
$RTS_OPTIONS \
-RTS"

  local QUICK_MODE_TYPE
  QUICK_MODE_TYPE="\
  $(if test "$QUICK_MODE" -ne 0; then set_super_quick_mode; fi) \
  $(bench_speed_options $(basename $BENCH_EXEC_PATH) $BENCH_NAME)"

  local QUICK_BENCH_OPTIONS
  for i in $QUICK_MODE_TYPE
  do
    case "$i" in
      super_quick) QUICK_BENCH_OPTIONS="$SUPER_QUICK_OPTIONS"; break ;;
      quicker) QUICK_BENCH_OPTIONS="$QUICKER_OPTIONS"; break ;;
    esac
  done

  local STREAM_SIZE
  local STREAM_LEN
  local STREAM_SIZE_OPT
  if test "$LONG" -ne 0
  then
    STREAM_SIZE=10000000
    STREAM_LEN=$(env LC_ALL=en_US.UTF-8 printf "--stream-size %'.f\n" $STREAM_SIZE)
    STREAM_SIZE_OPT="--stream-size $STREAM_SIZE"
  fi

  echo "$BENCH_NAME_ORIG: \
  $RTS_OPTIONS1 \
  $STREAM_LEN \
  $QUICK_BENCH_OPTIONS" \
  "$@"

  #------------------------------------------------------------------------------
  # Run benchmark with options and collect results
  #------------------------------------------------------------------------------

  local output_file
  output_file=$(bench_output_file $(basename $BENCH_EXEC_PATH))
  mkdir -p `dirname $output_file`
  rm -f ${output_file}.tmp

  local BENCH_NAME_ESC
  if test $USE_GAUGE -eq 0
  then
    # Escape "\" and double quotes in benchmark names
    BENCH_NAME_ESC=$(echo "$BENCH_NAME_ORIG" | sed -e 's/\\/\\\\/g' | sed -e 's/"/\\"/g')
    $BENCH_EXEC_PATH \
      -j 1 \
      $RTS_OPTIONS1 \
      $STREAM_SIZE_OPT \
      $QUICK_BENCH_OPTIONS \
      "$@" \
      --csv=${output_file}.tmp \
      -p '$0 == "'"$BENCH_NAME_ESC"'"' || die "Benchmark execution failed."

    # Convert cpuTime field from picoseconds to seconds
    awk --version 2>&1 | grep -q "GNU Awk" \
      || die "Need GNU awk. [$(which awk)] is not GNU awk."
    tail -n +2 ${output_file}.tmp | \
      awk 'BEGIN {FPAT = "([^,]+)|(\"[^\"]+\")";OFS=","} {$2=$2/1000000000000;print}' \
      >> $output_file
  else
    $BENCH_EXEC_PATH \
      $RTS_OPTIONS1 \
      $STREAM_SIZE_OPT \
      $QUICK_BENCH_OPTIONS \
      "$@" \
      --csvraw=${output_file}.tmp \
      -m exact "$BENCH_NAME" || die "Benchmark execution failed."
    tail -n +2 ${output_file}.tmp \
      >> $output_file
  fi
}

invoke_gauge () {
    local target_prog="$1"
    local target_name="$2"
    local output_file="$3"

    local MATCH=""
    if test "$LONG" -ne 0
    then
      MATCH="$target_name/o-1-space"
    else
      MATCH="$BENCH_PREFIX"
    fi
    echo "name,iters,time,cycles,cpuTime,utime,stime,maxrss,minflt,majflt,nvcsw,nivcsw,allocated,numGcs,bytesCopied,mutatorWallSeconds,mutatorCpuSeconds,gcWallSeconds,gcCpuSeconds" >> $output_file
    # keep only benchmark names with shortest prefix e.g. "a/b/c" and "a/b", we
    # should only keep "a/b" otherwise benchmarks will run multiple times. why?
    $target_prog -l \
      | grep "^$MATCH" \
      | while read -r name; \
  do bench_exec_one "$name" "${GAUGE_ARGS[@]}" || exit 1; done \
      || die "Benchmark execution failed."
}

invoke_tasty_bench () {
    local target_prog="$1"
    local target_name="$2"
    local output_file="$3"

    local MATCH=""
    if test "$LONG" -ne 0
    then
      MATCH="-p /$target_name\/o-1-space/"
    else
        if test -n "$BENCH_PREFIX"
        then
          # escape "/"
          local escaped_name=$(echo "$BENCH_PREFIX" | sed -e 's/\//\\\//g')
          MATCH="-p /$escaped_name/"
        fi
    fi
    echo "Name,cpuTime,2*Stdev (ps),Allocated,bytesCopied" >> $output_file
    $target_prog -l $MATCH \
      | grep "^All" \
      | while read -r name; \
          do bench_exec_one "$name" "${GAUGE_ARGS[@]}" || exit 1; done \
      || die "Benchmark execution failed."
}

run_bench_target () {
  local package_name=$1
  local component=$2
  local target_name=$3

  local target_prog
  if test -z "$BENCHMARK_PACKAGE_VERSION"
  then
    echo "Please set BENCHMARK_PACKAGE_VERSION in bench_config"
    exit 1
  fi
  target_prog=$(cabal_target_prog $package_name-$BENCHMARK_PACKAGE_VERSION $component $target_name) || \
    die "Cannot find executable for target $target_name"

  echo "Running executable $target_name ..."

  # Needed by bench_exec_one
  BENCH_EXEC_PATH=$target_prog

  local output_file=$(bench_output_file $target_name)
  mkdir -p `dirname $output_file`
  if test "$USE_GAUGE" -eq 0
  then invoke_tasty_bench "$target_prog" "$target_name" "$output_file"
  else invoke_gauge "$target_prog" "$target_name" "$output_file"
  fi
}

# $1: package name
# $2: component
# $3: targets
run_bench_targets() {
    for i in $3
    do
      run_bench_target $1 $2 $i
    done
}

run_benches_comparing() {
    local bench_list=$1

    if test -z "$CANDIDATE"
    then
      CANDIDATE=$(git rev-parse HEAD)
    fi
    if test -z "$BASE"
    then
      # XXX Should be where the current branch is forked from master
      BASE="$CANDIDATE^"
    fi
    echo "Comparing baseline commit [$BASE] with candidate [$CANDIDATE]"
    echo "Checking out base commit [$BASE] for benchmarking"
    git checkout "$BASE" || die "Checkout of base commit [$BASE] failed"

    $BUILD_BENCH || die "build failed"
    run_bench_targets $BENCHMARK_PACKAGE_NAME b "$bench_list" target_exe_extra_args

    echo "Checking out candidate commit [$CANDIDATE] for benchmarking"
    git checkout "$CANDIDATE" || \
        die "Checkout of candidate [$CANDIDATE] commit failed"

    $BUILD_BENCH || die "build failed"
    run_bench_targets $BENCHMARK_PACKAGE_NAME b "$bench_list" target_exe_extra_args
    # XXX reset back to the original commit
}

backup_output_file() {
  local bench_name=$1
  local output_file=$(bench_output_file $bench_name)

  if test -e $output_file -a "$APPEND" != 1
  then
      mv -f -v $output_file ${output_file}.prev
  fi
}

run_measurements() {
  local bench_list=$1

  for i in $bench_list
  do
      backup_output_file $i
  done

  if test "$COMMIT_COMPARE" = "0"
  then
    run_bench_targets $BENCHMARK_PACKAGE_NAME b "$bench_list" target_exe_extra_args
  else
    run_benches_comparing "$bench_list"
  fi
}

run_reports() {
    local prog
    prog=$BENCH_REPORT_DIR/bin/bench-report
    test -x $prog || die "Cannot find bench-report executable"
    echo

    for i in $1
    do
        echo "Generating reports for ${i}..."
        $prog \
            --benchmark $i \
            $(test "$USE_GAUGE" = 1 && echo "--use-gauge") \
            $(test "$GRAPH" = 1 && echo "--graphs") \
            $(test "$SORT_BY_NAME" = 1 && echo "--sort-by-name") \
            $(test -n "$BENCH_DIFF_STYLE" && echo "--diff-style $BENCH_DIFF_STYLE") \
            $(test -n "$BENCH_CUTOFF_PERCENT" && echo "--diff-cutoff-percent $BENCH_CUTOFF_PERCENT") \
            --fields "$FIELDS"
    done
}

#-----------------------------------------------------------------------------
# Execution starts here
#-----------------------------------------------------------------------------

USE_GIT_CABAL=1 # This is used by set_common_vars
set_common_vars

USE_NIX=0
COMPARE=0
COMMIT_COMPARE=0
BASE=
CANDIDATE=

APPEND=0
LONG=0
RAW=0
SORT_BY_NAME=0
GRAPH=0
MEASURE=1

GAUGE_ARGS=
BUILD_ONCE=0

CABAL_BUILD_OPTIONS="--flag fusion-plugin "

#-----------------------------------------------------------------------------
# Read command line
#-----------------------------------------------------------------------------

while test -n "$1"
do
  case $1 in
    -h|--help|help) print_help ;;
    # options with arguments
    --benchmarks) shift; TARGETS=$1; shift ;;
    --targets) shift; TARGETS=$1; shift ;;
    --prefix) shift; BENCH_PREFIX="$1"; shift ;;
    --fields) shift; FIELDS=$1; shift ;;
    --base) shift; BASE=$1; shift ;;
    --candidate) shift; CANDIDATE=$1; shift ;;
    --with-compiler) shift; CABAL_WITH_COMPILER=$1; shift ;;
    --cabal-build-flags) shift; CABAL_BUILD_OPTIONS+=$1; shift ;;
    --cabal-build-options) shift; CABAL_BUILD_OPTIONS+=$1; shift ;;
    --rtsopts) shift; RTS_OPTIONS=$1; shift ;;
    --config) shift; BENCH_CONFIG_FILE=$1; shift ;;
    --diff-style) shift; BENCH_DIFF_STYLE=$1; shift ;;
    --diff-cutoff-percent) shift; BENCH_CUTOFF_PERCENT=$1; shift ;;
    # flags
    --slow) SLOW=1; shift ;;
    --quick) QUICK_MODE=1; shift ;;
    --compare) COMPARE=1; shift ;;
    --commit-compare) COMMIT_COMPARE=1; shift ;;
    --raw) RAW=1; shift ;;
    --append) APPEND=1; shift ;;
    --long) LONG=1; shift ;;
    --sort-by-name) SORT_BY_NAME=1; shift ;;
    --graphs) GRAPH=1; shift ;;
    --no-measure) MEASURE=0; shift ;;
    --dev-build) RUNNING_DEVBUILD=1; shift ;;
    --use-nix) USE_NIX=1; shift ;;
    --use-gauge) USE_GAUGE=1; shift ;;
    --) shift; break ;;
    *) echo "Unknown flags: $*"; echo; print_help ;;
  esac
done
GAUGE_ARGS=("$@")

if test -z "$BENCH_CONFIG_FILE"
then
  die "Please use --config to specify config file"
fi

source "$BENCH_CONFIG_FILE" || \
  die "Failed to source config file $BENCH_CONFIG_FILE"

# Defined in $BENCH_CONFIG_FILE
bench_config

if test -z "$FIELDS"
then
  FIELDS=$DEFAULT_FIELDS
fi

set_derived_vars

#-----------------------------------------------------------------------------
# Determine targets
#-----------------------------------------------------------------------------

only_real_benchmarks () {
  for i in $TARGETS
  do
    local SKIP=0
    for j in $COMPARISONS
    do
      if test $i == $j
      then
        SKIP=1
      fi
    done
    if test "$SKIP" -eq 0
    then
      echo -n "$i "
    fi
  done
}

# defined in $BENCH_CONFIG_FILE
bench_targets

if test "$(has_item "$TARGETS" help)" = "help"
then
  list_target_groups
  list_comparisons
  list_targets
  exit
fi

COMMON_FIELDS="allocated bytescopied cputime maxrss"
if test "$USE_GAUGE" -eq 1
then
  ALL_FIELDS="$COMMON_FIELDS time cycles utime stime minflt majflt nvcsw nivcsw"
else
  ALL_FIELDS="$COMMON_FIELDS"
fi
if test "$(has_item "$FIELDS" help)" = "help"
then
  echo "Supported fields: $ALL_FIELDS"
  echo "Default fields: $DEFAULT_FIELDS"
  exit
fi

if test "$LONG" -ne 0
then
  if test -n "$TARGETS"
  then
    echo "Cannot specify benchmarks [$TARGETS] with --long"
    exit
  fi
  TARGETS=$infinite_grp
fi

DEFAULT_TARGETS="$(all_grp)"
TARGETS=$(set_targets)

TARGETS_ORIG=$TARGETS
TARGETS=$(only_real_benchmarks)

echo "Using benchmark suites [$TARGETS]"

#-----------------------------------------------------------------------------
# Build reporting utility
#-----------------------------------------------------------------------------

# We need to build the report progs first at the current (latest) commit before
# checking out any other commit for benchmarking.
build_report_progs

#-----------------------------------------------------------------------------
# Build and run targets
#-----------------------------------------------------------------------------

if test "$USE_GAUGE" -eq 1
then
  BUILD_FLAGS="--flag use-gauge"
fi

BUILD_BENCH="$CABAL_EXECUTABLE v2-build $BUILD_FLAGS $CABAL_BUILD_OPTIONS --enable-benchmarks"
if test "$MEASURE" = "1"
then
  run_build "$BUILD_BENCH" $BENCHMARK_PACKAGE_NAME bench "$TARGETS"
  run_measurements "$TARGETS"
fi

#-----------------------------------------------------------------------------
# Run reports
#-----------------------------------------------------------------------------

# $1: var name
build_comparison_results () {
    local name
    local constituents

    name=$1
    constituents=$(eval "echo -n \$${name}")
    mkdir -p "charts/$name"
    dest_file="charts/$name/results.csv"
    : > $dest_file
    for j in $constituents
    do
      cat "charts/$j/results.csv" >> $dest_file
    done
}

if test "$COMPARE" -eq 1
then
  DYN_CMP_GRP="$(echo "$TARGETS" | sed -e 's/ /_/g')_cmp"
  eval "$DYN_CMP_GRP=\"$TARGETS\""
  COMPARISON_REPORTS=$DYN_CMP_GRP
  build_comparison_results $DYN_CMP_GRP
else
  COMPARISON_REPORTS=""
fi

for i in $COMPARISONS
do
  if test "$(has_item "$TARGETS_ORIG" $i)" = $i
  then
    COMPARISON_REPORTS="$COMPARISON_REPORTS $i"
    build_comparison_results $i
  fi
done

if test "$RAW" = "0"
then
  run_reports "$TARGETS"
  run_reports "$COMPARISON_REPORTS"
  if test -n "$DYN_CMP_GRP"
  then
    rm -rf "charts/$DYN_CMP_GRP"
  fi
fi
