#!/usr/bin/env bash

BENCH_CONFIG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
source $BENCH_CONFIG_DIR/targets.sh \
  || { echo "Cannot source $BENCH_CONFIG_DIR/targets.sh"; exit 1; }

# Customization options
bench_config () {
  BENCH_REPORT_DIR=benchmark/bench-report
  BENCHMARK_PACKAGE_NAME=streamly-benchmarks
  BENCHMARK_PACKAGE_VERSION=0.0.0

  USE_GAUGE=0
  DEFAULT_FIELDS="allocated cputime bytescopied"
}

#------------------------------------------------------------------------------
# benchmark groups
#------------------------------------------------------------------------------

bench_targets () {
  targets
}

#------------------------------------------------------------------------------
# RTS options based on the benchmark executable name and benchmark name
#------------------------------------------------------------------------------

bench_rts_options () {
  local exe_name
  local bench_name

  exe_name="$1"
  bench_name="$2"

  # Based on benchmark class
  case "$bench_name" in
    */o-1-sp*) echo -n "-K36K -M16M" ;;
    */o-n-h*) echo -n "-K36K -M32M" ;;
    */o-n-st*) echo -n "-K1M -M16M" ;;
    */o-n-sp*) echo -n "-K1M -M32M" ;;
    *) echo -n "" ;;
  esac

  echo " "

  # Based on specific benchmark
  # XXX Note: for tasty-bench we replace the "." separator in the benchmark names
  # with "/" for matching with this. It may not work reliably if the benchmark
  # name already contains ".".
  case "$bench_name" in
    Data.Stream.StreamD/o-n-space/elimination/toList) echo -n "-K2M" ;;
    Data.Stream.StreamK/o-n-space/elimination/toList) echo -n "-K2M" ;;

    Prelude.Parallel/o-n-heap/mapping/mapM) echo -n "-M256M" ;;
    Prelude.Parallel/o-n-heap/monad-outer-product/*) echo -n "-M256M" ;;
    Prelude.Parallel/o-n-space/monad-outer-product/*) echo -n "-K4M -M256M" ;;

    Prelude.Rate/o-1-space/*) echo -n "-K128K" ;;
    Prelude.Rate/o-1-space/asyncly/*) echo -n "-K128K" ;;

    # XXX For GHC-9.0
    Prelude.Serial/o-1-space/mixed/sum-product-fold) echo -n "-K64M" ;;

    # XXX These should be moved to o-n-space?
    Prelude.Serial/o-n-heap/grouping/classifySessionsOf) echo -n "-K1M -M32M" ;;
    Prelude.Serial/o-n-heap/Functor/*) echo -n "-K4M -M32M" ;;
    Prelude.Serial/o-n-heap/transformer/*) echo -n "-K8M -M64M" ;;

    Prelude.Serial/o-n-space/Functor/*) echo -n "-K4M -M64M" ;;
    Prelude.Serial/o-n-space/Applicative/*) echo -n "-K8M -M128M" ;;
    Prelude.Serial/o-n-space/Monad/*) echo -n "-K8M -M64M" ;;

    # Use -K4M for o-n-space except for grouping
    Prelude.Serial/o-n-space/grouping/*) echo -n "" ;;
    Prelude.Serial/o-n-space/*) echo -n "-K4M" ;;

    Prelude.WSerial/o-n-space/*) echo -n "-K4M" ;;

    Prelude.Async/o-n-space/monad-outer-product/*) echo -n "-K4M" ;;
    Prelude.Ahead/o-n-space/monad-outer-product/*) echo -n "-K4M" ;;
    Prelude.Ahead/o-1-space/*) echo -n "-K128K" ;;

    Prelude.WAsync/o-n-heap/monad-outer-product/toNull3) echo -n "-M64M" ;;
    Prelude.WAsync/o-n-space/monad-outer-product/*) echo -n "-K4M" ;;

    # XXX need to investigate these, taking too much stack
    Data.Parser.ParserD/o-1-space/some) echo -n "-K8M" ;;
    Data.Parser/o-1-space/some) echo -n "-K8M" ;;
    Data.Parser.ParserD/o-1-space/manyTill) echo -n "-K4M" ;;
    Data.Parser/o-1-space/manyTill) echo -n "-K4M" ;;
    Data.Parser/o-n-heap/manyAlt) echo -n "-K4M -M128M" ;;
    Data.Parser/o-n-heap/someAlt) echo -n "-K4M -M128M" ;;
    Data.Parser.ParserK/o-n-heap/manyAlt) echo -n "-K4M -M128M" ;;
    Data.Parser.ParserK/o-n-heap/someAlt) echo -n "-K4M -M128M" ;;
    Data.Parser.ParserK/o-n-heap/sequence) echo -n "-M64M";;
    Data.Parser.ParserK/o-n-heap/sequenceA) echo -n "-M64M";;

    Data.SmallArray/o-1-sp*) echo -n "-K128K" ;;
    # For tasty-bench
    Data.Array*/o-1-space/generation/show) echo -n "-M32M" ;;
    # XXX For GHC-8.10
    Data.Array/o-1-space/transformationX4/map) echo -n "-M32M" ;;
    # DEVBUILD only benchmarks - array foldable instance
    Data.Array.Foreign/o-1-space/elimination/foldable/foldl*) echo -n "-K8M" ;;
    Data.Array.Foreign/o-1-space/elimination/foldable/sum) echo -n "-K8M" ;;
    *) echo -n "" ;;
  esac

  case "$exe_name" in
    Prelude.Concurrent*) echo -n " -K256K -M384M" ;;
    *) echo -n "" ;;
  esac
}

#------------------------------------------------------------------------------
# Speed options
#------------------------------------------------------------------------------

bench_speed_options () {
  local exe_name
  local bench_name

  exe_name="$1"
  bench_name="$2"

  case "$exe_name" in
    Prelude.Concurrent) set_super_quick_mode ;;
    Prelude.Rate) set_super_quick_mode ;;
    Prelude.Adaptive) set_super_quick_mode;;
    *) echo -n "" ;;
  esac

  # XXX Note: for tasty-bench we replace the "." separator in the benchmark names
  # with "/" for matching with this. It may not work reliably if the benchmark
  # name already contains ".".

  # Use quick options for benchmarks that take too long
  case "$bench_name" in
    Prelude.Parallel/o-n-heap/mapping/mapM) set_super_quick_mode ;;
    Prelude.Parallel/o-n-heap/monad-outer-product/*) set_super_quick_mode ;;
    Prelude.Parallel/o-n-space/monad-outer-product/*) set_super_quick_mode ;;
    Prelude.Parallel/o-n-heap/generation/*) use_quicker_mode ;;
    Prelude.Parallel/o-n-heap/mapping/*) use_quicker_mode ;;
    Prelude.Parallel/o-n-heap/concat-foldable/*) use_quicker_mode ;;

    Prelude.Async/o-1-space/monad-outer-product/*) use_quicker_mode ;;
    Prelude.Async/o-n-space/monad-outer-product/*) use_quicker_mode ;;

    Prelude.Ahead/o-1-space/monad-outer-product/*) use_quicker_mode ;;
    Prelude.Ahead/o-n-space/monad-outer-product/*) use_quicker_mode ;;

    Prelude.WAsync/o-n-heap/monad-outer-product/*) use_quicker_mode ;;
    Prelude.WAsync/o-n-space/monad-outer-product/*) use_quicker_mode ;;

    FileSystem.Handle/*) use_quicker_mode ;;
    *) echo -n "" ;;
  esac
}
