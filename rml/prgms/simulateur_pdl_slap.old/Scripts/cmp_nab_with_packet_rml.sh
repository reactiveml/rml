
n=2000
until [ $n -eq 6000 ]; do
    let $[ n += 100 ]
    ../simul.opt -N 100 -n $n -t 1000 -msg_proba 100 -D 20 -nox >> stat.100.data
done
