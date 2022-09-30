
sed 's/    /  /g' -i formal/*lean
TMP=`mktemp`
for f in formal/*lean; do
    cat --squeeze-blank $f > $TMP
    mv $TMP $f
done
