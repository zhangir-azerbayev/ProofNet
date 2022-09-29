CLEAN_BENCHMARK=benchmark_to_publish

mkdir $CLEAN_BENCHMARK
mkdir $CLEAN_BENCHMARK/formal
mkdir $CLEAN_BENCHMARK/informal

for f in formal/*lean; do
    if [ -s $f ]; then
        cp $f $CLEAN_BENCHMARK/formal
        sed 's/\-\-.*//g' -i $CLEAN_BENCHMARK/$f # remove comments
    fi
done



for f in $CLEAN_BENCHMARK/informal/*tex; do
    pdflatex $f
done
