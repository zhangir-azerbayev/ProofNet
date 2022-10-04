
for informal in informal/*tex; do
    formal=formal/`basename -s .tex $informal`.lean
    echo $informal
    ./list_not_formalized.py $formal $informal
    echo
done
