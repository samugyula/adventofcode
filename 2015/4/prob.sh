
i=0
while
    md5=`echo -n bgvyzdsv$i | md5sum`
    echo $i
#   echo $md5
[[ "$md5" != "000000"* ]]; do
    let "i=i+1"
done
