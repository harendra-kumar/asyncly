#------------------------------------------------------------------------------
# test and benchmark groups
#------------------------------------------------------------------------------

# Depends on RUNNING_TESTS, RUNNING_BENCHMARKS, RUNNING_DEVBUILD variables
# being set for dev_build, test_only, bench_only functions to work. So the
# "targets" fucntion should be called only after these are set.

# IMPORTANT NOTE: the names "_grp" and "_cmp" suffixes are special, do
# not rename them to something else.

targets () {
  base_stream_grp="\
    `bench_only Data.Stream.StreamD` \
    `bench_only Data.Stream.StreamK` \
    `bench_only Data.Stream.StreamDK`"

  prelude_serial_grp="\
    Prelude.Serial \
    Prelude.WSerial \
    Prelude.ZipSerial"

  prelude_concurrent_grp="\
    Prelude.Async \
    Prelude.WAsync \
    Prelude.Ahead \
    Prelude.Parallel \
    Prelude.ZipAsync"

  prelude_other_grp="\
    `test_only Prelude` \
    $(test_only $(dev_build Prelude.Rate)) \
    `bench_only Prelude.Rate` \
    `test_only Prelude.Fold` \
    `test_only Prelude.Concurrent` \
    $(bench_only $(dev_build Prelude.Concurrent)) \
    `bench_only Prelude.Adaptive`"

  array_grp="\
    Data.Array \
    Data.Array.Foreign \
    Data.Array.Prim \
    Data.SmallArray \
    Data.Array.Prim.Pinned"

  base_parser_grp="\
    Data.Parser.ParserD \
    `bench_only Data.Parser.ParserK`"

  parser_grp="\
    Data.Fold \
    Data.Parser"

  list_grp="\
    `test_only Data.List.Base` \
    `test_only Data.List`"

  #------------------------------------------------------------------------------
  # Streaming vs non-streaming
  #------------------------------------------------------------------------------
  # The "o-1-space" groups of these benchmarks are run with long stream
  # sizes when --long option is used.

  infinite_grp="\
    $prelude_serial_grp \
    $prelude_concurrent_grp \
    `bench_only Prelude.Rate`"

  #------------------------------------------------------------------------------
  # Benchmark comparison groups
  #------------------------------------------------------------------------------

  # *_cmp denotes a comparison benchmark, the benchmarks provided in *_cmp
  # variables are compared with each other
  base_stream_cmp="Data.Stream.StreamD Data.Stream.StreamK"
  serial_wserial_cmp="Prelude.Serial Prelude.WSerial"
  serial_async_cmp="Prelude.Serial Prelude.Async"
  concurrent_cmp="Prelude.Async Prelude.WAsync Prelude.Ahead Prelude.Parallel"
  array_cmp="Memory.Array Data.Array.Prim Data.Array Data.Array.Prim.Pinned"
  pinned_array_cmp="Memory.Array Data.Array.Prim.Pinned"
  base_parser_cmp=$base_parser_grp

  COMPARISONS="\
    base_stream_cmp \
    serial_wserial_cmp \
    serial_async_cmp \
    concurrent_cmp \
    array_cmp \
    pinned_array_cmp \
    base_parser_cmp"

  #------------------------------------------------------------------------------
  # All test/benchmark modules must be in at least one of these
  #------------------------------------------------------------------------------

  # All groups
  GROUP_TARGETS="\
      base_stream_grp \
      prelude_serial_grp \
      prelude_concurrent_grp \
      prelude_other_grp \
      array_grp \
      base_parser_grp \
      parser_grp \
      list_grp  \
      infinite_grp"

  # Not in any groups
  INDIVIDUAL_TARGETS="\
      Data.Unfold \
      Unicode.Stream \
      FileSystem.Handle \
      `test_only FileSystem.Event` \
      `test_only Network.Socket` \
      `test_only Network.Inet.TCP` \
      `test_only version-bounds`"
}
