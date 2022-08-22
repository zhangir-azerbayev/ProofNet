N_INFORMAL=`grep -r exercise informal | wc -l`
N_FORMAL=`grep -r exercise formal | wc -l`

echo "Number of informal exercices: $N_INFORMAL"
echo "Number of formalized exercices: $N_FORMAL"
