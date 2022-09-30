CLEAN_BENCHMARK=benchmark_to_publish

mkdir -p $CLEAN_BENCHMARK/formal
mkdir -p $CLEAN_BENCHMARK/informal

for f in formal/*lean; do
    if [ -s $f ]; then
        cp $f $CLEAN_BENCHMARK/formal
        sed 's/\-\-.*//g' -i $CLEAN_BENCHMARK/$f # remove comments
        tex=informal/`basename -s .lean $f`.tex
        ./remove_not_formalized.py $CLEAN_BENCHMARK/$f $tex \
            > $CLEAN_BENCHMARK/$tex
        echo $f DONE
    fi
done

cd $CLEAN_BENCHMARK/informal
echo 'Making PDFs...'
for f in *tex; do
    pdflatex $f &> /dev/null
    rm *log
    rm *aux
done
echo DONE
