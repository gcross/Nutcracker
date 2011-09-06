export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:../build/lib
export LD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:../build/lib

python test.py
python3 test.py