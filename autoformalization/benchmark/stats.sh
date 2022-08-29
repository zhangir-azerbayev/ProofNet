for f in informal/*tex; do
    n=`grep 'Exercise ' $f | wc -l`
    echo $f: $n
done
echo
for f in formal/*lean; do
    n=`grep 'exercise_' $f | wc -l`
    echo $f: $n
done
echo
N_INFORMAL=`grep -r 'Exercise ' informal | wc -l`
N_FORMAL=`grep -r exercise formal | wc -l`
echo "Total number of informal exercices: $N_INFORMAL"
echo "Total number of formalized exercices: $N_FORMAL"

