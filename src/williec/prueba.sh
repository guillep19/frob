
max=20
for i in `seq 1 $max`
do
  echo "curl http://www.ulala.com.uy/Application/Newsletter?email=hardbounce$i%40hotmail.com"
  curl http://www.ulala.com.uy/Application/Newsletter?email=hardbounce$i%40hotmail.com
done
