fi1=fi2=fi3=1 # FIB Russia rextester.com/FEEJ49204
for da in range(1, 4): # Danilin
    print("."*(20-len(str(fi3))), end=' ')
    print(fi3)
    fi3 = fi2+fi1
    fi1 = fi2
    fi2 = fi3
#................... 1
#................... 2
#................... 3 3lines with range 4
#
#. 1100087778366101931 30th w range 88

