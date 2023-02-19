for f in benchmark_to_publish/informal/*tex; do
    n=`grep 'Exercise ' $f | wc -l`
    n_proofs=`grep 'begin{proof}' $f | wc -l`
    echo "$f: $n statements, $n_proofs proof"
done
echo
for f in benchmark_to_publish/formal/*lean; do
    n=`grep 'exercise_' $f | wc -l`
    echo $f: $n
done
echo
N_INFORMAL=`grep -r 'Exercise ' benchmark_to_publish/informal/*tex | wc -l`
N_PROOFS=`grep -r 'begin{proof}' benchmark_to_publish/informal/*tex | wc -l`
N_FORMAL=`grep -r exercise benchmark_to_publish/formal/*lean | wc -l`
echo "Total number of informal exercices: $N_INFORMAL"
echo "Total number of informal proofs: $N_PROOFS"
echo "Total number of formalized exercices: $N_FORMAL"

