#!/bin/sh

run_bench() {
  ITER=$1
  rm -f $TIME_FILE
  for ((i=0;i<$ITER;i++)); do
    (time taskset -c $PROC ./$PROG) 2>> $TIME_FILE
  done
}

export TIMEFORMAT="%3R"
ulimit -t 10
mkdir -p timing
make all
sudo invoke-rc.d cpufreqd stop
sudo cpufreq-set -c 0 -g userspace
sudo cpufreq-set -c 1 -g userspace
sudo cpufreq-set -c 0 -f 2.0GHz
sudo cpufreq-set -c 1 -f 2.0GHz

PROC=0
MEAN_FILE="timing/b2.mean"
for PROG in b2 b2-cpc b2-manual-return b2-manual-return-cpc; do
  TIME_FILE="timing/$PROG.time"
  run_bench 10
  echo -n "$PROG " >> $MEAN_FILE
  awk '{t+=$0; n++} END{print (t/n);}' <$TIME_FILE >> $MEAN_FILE
done

for t in b3 b4 b5; do
  echo $t
  MEAN_FILE="timing/$t.mean"
  rm $MEAN_FILE
  for lib in nptl cpc st pth; do
    echo $lib
    # Use only first core
    PROG=$t-$lib
    TIME_FILE="timing/$t-$lib-mono.time"
    PROC=0
    run_bench 10
    echo -n "$lib-mono " >> $MEAN_FILE
    awk '{t+=$0; n++} END{print (t/n);}' <$TIME_FILE >> $MEAN_FILE
    # Use both cores
    TIME_FILE="timing/$t-$lib-multi.time"
    PROC=0,1
    run_bench 10
    echo -n "$lib-multi " >> $MEAN_FILE
    awk '{t+=$0; n++} END{print (t/n);}' <$TIME_FILE >> $MEAN_FILE
  done
done

EXTRA_DEFINES="-DJOIN" make clean b4-nptl b4-cpc b4-st b4-pth
t=b4-join
echo $t
MEAN_FILE="timing/$t.mean"
rm $MEAN_FILE
for lib in nptl cpc st pth; do
  echo $lib
  # Use only first core
  PROG=b4-$lib
  TIME_FILE="timing/$t-$lib-mono.time"
  PROC=0
  run_bench 10
  echo -n "$lib-mono " >> $MEAN_FILE
  awk '{t+=$0; n++} END{print (t/n);}' <$TIME_FILE >> $MEAN_FILE
  # Use both cores
  TIME_FILE="timing/$t-$lib-multi.time"
  PROC=0,1
  run_bench 10
  echo -n "$lib-multi " >> $MEAN_FILE
  awk '{t+=$0; n++} END{print (t/n);}' <$TIME_FILE >> $MEAN_FILE
done
