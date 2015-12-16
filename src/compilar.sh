
cd williec
make clean
make
cd ..
# Creo example5.bin
./williec/williec example5.willie example5
rm example5
echo "#include \"vmcode.h\"" > vmcode.cpp
echo " CODE code = {" >> vmcode.cpp
while read line
do
  echo -e "$line," >> vmcode.cpp;
done < example5.bin
rm example5.bin
echo "0};" >> vmcode.cpp

mv vmcode.cpp alfvm/vmcode.cpp
cd alfvm
make clean
make test
make
mv mbed_alfvm.bin ../mbed_alfvm.bin
make clean
cd ..
