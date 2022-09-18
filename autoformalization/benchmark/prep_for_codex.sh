if [ ! -d tmp ]; then
    mkdir tmp
fi
for f in informal/*tex; do
    ff=`basename ${f%.*}`
    grep 'paragraph{Exercise ' $f | \
        sed "s/\\\paragraph{Exercise \([^}]*\)}/$ff.\1/g" \
        > tmp/$ff.1
done

cat tmp/*.1 > tmp/all
cut -d' ' -f1 tmp/all > tmp/names
cut -d' ' -f2- tmp/all > tmp/statements
seq `wc -l tmp/all | cut -d' ' -f1` > tmp/ids
paste -d' ' tmp/ids tmp/names > id_name_dict
paste -d', ' \
    <(sed 's/^/"nl_statement": "/g; s/$/"/g' tmp/statements) \
    <(sed 's/^/ "id": /g' tmp/ids) | \
    sed 's/^/{/g; s/$/}/g; s/\\/\\\\/g' \
    > informal.jsonl
paste -d', ' \
    <(sed 's/^/"id": /g' tmp/ids) \
    <(sed 's/^/ "name": "/g; s/$/"/g' tmp/names) | \
    sed 's/^/{/g; s/$/}/g' \
    > id_name_dict.jsonl
