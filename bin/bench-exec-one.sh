#!/usr/bin/env bash

# Environment passed:
# BENCH_EXEC_PATH: the benchmark executable
# RTS_OPTIONS: additional RTS options
# QUICK_MODE: whether we are in quick mode
# USE_GAUGE: whether to use gauge or tasty-bench
# LONG: whether to use a large stream size

set -e
set -o pipefail

SCRIPT_DIR=$(cd `dirname $0`; pwd)
source $SCRIPT_DIR/bench-exec-options.sh

# $1: message
die () {
  >&2 echo -e "Error: $1"
  exit 1
}

warn () {
  >&2 echo -e "Warning: $1"
}

test -n "$BENCH_EXEC_PATH" || die "BENCH_EXEC_PATH env var must be set"
test -n "$QUICK_MODE" || warn "QUICK_MODE env var not set (to 0 or 1)"

#------------------------------------------------------------------------------
# Speed options
#------------------------------------------------------------------------------

if test "$USE_GAUGE" -eq 0
then
  SUPER_QUICK_OPTIONS="--stdev 1000000"
  QUICKER_OPTIONS="--stdev 100"
else
  # Do not keep time limit as 0 otherwise GC stats may remain 0 in some cases.
  SUPER_QUICK_OPTIONS="--quick --min-duration 0 --time-limit 0.01 --include-first-iter"
  QUICKER_OPTIONS="--min-samples 3 --time-limit 1"
fi

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

bench_output_file() {
    local bench_name=$1
    echo "charts/$bench_name/results.csv"
}

#------------------------------------------------------------------------------
# Determine options from benchmark name
#------------------------------------------------------------------------------

BENCH_NAME_ORIG="$1"
shift

if test "$USE_GAUGE" -eq 0
then
  # XXX this is a hack to make the "/" separated names used in the functions
  # determining options based on benchmark name. For tasty-bench the benchmark
  # names are separated by "." instead of "/".
  BENCH_NAME0=$(echo $BENCH_NAME_ORIG | sed -e s/^All\.//)
  BENCH_NAME1=$(echo $BENCH_NAME0 | cut -f1 -d '/')
  BENCH_NAME2=$(echo $BENCH_NAME0 | cut -f2- -d '/' | sed -e 's/\./\//g')
  BENCH_NAME="$BENCH_NAME1/$BENCH_NAME2"
else
  BENCH_NAME=$BENCH_NAME_ORIG
fi

RTS_OPTIONS=\
"+RTS -T \
$(bench_rts_options $(basename $BENCH_EXEC_PATH) $BENCH_NAME) \
$RTS_OPTIONS \
-RTS"

QUICK_MODE_TYPE="\
$(if test "$QUICK_MODE" -ne 0; then set_super_quick_mode; fi) \
$(bench_speed_options $(basename $BENCH_EXEC_PATH) $BENCH_NAME)"

for i in $QUICK_MODE_TYPE
do
  case "$i" in
    super_quick) QUICK_BENCH_OPTIONS="$SUPER_QUICK_OPTIONS"; break ;;
    quicker) QUICK_BENCH_OPTIONS="$QUICKER_OPTIONS"; break ;;
  esac
done

if test "$LONG" -ne 0
then
  STREAM_SIZE=10000000
  STREAM_LEN=$(env LC_ALL=en_US.UTF-8 printf "--stream-size %'.f\n" $STREAM_SIZE)
  STREAM_SIZE_OPT="--stream-size $STREAM_SIZE"
fi

echo "$BENCH_NAME_ORIG: \
$RTS_OPTIONS \
$STREAM_LEN \
$QUICK_BENCH_OPTIONS" \
"$@"

#------------------------------------------------------------------------------
# Run benchmark with options and collect results
#------------------------------------------------------------------------------

output_file=$(bench_output_file $(basename $BENCH_EXEC_PATH))
mkdir -p `dirname $output_file`
rm -f ${output_file}.tmp

if test $USE_GAUGE -eq 0
then
  # Escape "\" and double quotes in benchmark names
  BENCH_NAME_ESC=$(echo "$BENCH_NAME_ORIG" | sed -e 's/\\/\\\\/g' | sed -e 's/"/\\"/g')
  $BENCH_EXEC_PATH \
    -j 1 \
    $RTS_OPTIONS \
    $STREAM_SIZE_OPT \
    $QUICK_BENCH_OPTIONS \
    "$@" \
    --csv=${output_file}.tmp \
    -p '$0 == "'"$BENCH_NAME_ESC"'"'

  # Convert cpuTime field from picoseconds to seconds
  awk --version 2>&1 | grep -q "GNU Awk" \
    || die "Need GNU awk. [$(which awk)] is not GNU awk."
  tail -n +2 ${output_file}.tmp | \
    awk 'BEGIN {FPAT = "([^,]+)|(\"[^\"]+\")";OFS=","} {$2=$2/1000000000000;print}' \
    >> $output_file
else
  $BENCH_EXEC_PATH \
    $RTS_OPTIONS \
    $STREAM_SIZE_OPT \
    $QUICK_BENCH_OPTIONS \
    "$@" \
    --csvraw=${output_file}.tmp \
    -m exact "$BENCH_NAME"
  tail -n +2 ${output_file}.tmp \
    >> $output_file
fi
