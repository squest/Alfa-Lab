import time

def sqr (x) :
    return x * x

def is_div (a,b) :
  return 0 == a % b

def sqrt (x) : 
	return x**(0.5)

def is_prime (m) :
    status = True
    lim = sqrt (m)	
    i = 3
    while (i <= lim) and status:
        if (is_div (m,i)):
            status = False
        else :
            i = i + 2
    return status
	
def suma_prima (lim) :
    suma = 10
    i = 7
    cur = time.time ()
    while i < lim :
        if is_prime (i) :
            suma += i
        i += 2
    print time.time () - cur, "second"  
    return suma
        



		
		
		
		
		
