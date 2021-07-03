#!/usr/bin/env bash

SCRIPT_DIR=$(cd `dirname $0`; pwd)
source $SCRIPT_DIR/bench-config.sh \
  || { echo "Cannot source $SCRIPT_DIR/bench-config.sh"; exit 1; }

bench_config
if test -z "$BENCH_REPORT_DIR"
then
  echo "BENCH_REPORT_DIR variable not defined by bench-config() in $SCRIPT_DIR/bench-config.sh"
  exit 1
fi

cd $SCRIPT_DIR/..
$BENCH_REPORT_DIR/bin/bench-runner.sh --config $SCRIPT_DIR/bench-config.sh "$@"
