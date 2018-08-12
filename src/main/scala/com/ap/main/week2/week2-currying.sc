def sumInitial(f: Int => Int): (Int, Int) => Int = {
  def returnSum(a: Int, b: Int) : Int = {
    if(a>b) 0 else f(a) + returnSum(a+1,b)
  }
  returnSum
}
sumInitial(x=>x)(1,10)
sumInitial(x=>x*x)(1,10)
sumInitial(x=>x*x*x)(1,10)

def sum(f: Int => Int)(a: Int, b: Int) : Int = {
  if(a>b) 0 else f(a) + sum(f)(a+1,b)
}
sum(x=>x)(1,10)
sum(x=>x*x)(1,10)
sum(x=>x*x*x)(1,10)

def product(f: Int => Int)(a: Int, b: Int) : Int = {
  mapReduce(f, (x,y)=>x*y,1)(a,b)
}
product(x=>x)(1,4)
product(x=>x*x)(1,4)
product(x=>x*x*x)(1,4)

def factorial(a:Int):Int = product(x=>x)(1,a)
factorial(5)
factorial(6)

def mapReduce(f:Int=>Int, combine: (Int, Int)=>Int,
              baseVal: Int)(a: Int, b:Int): Int = {
  if(a>b) baseVal
  else combine(f(a),mapReduce(f,combine,baseVal)(a+1,b))
}
mapReduce(x=>x, (x,y)=>x+y,0)(1,4)
mapReduce(x=>x, (x,y)=>x*y,1)(1,4)
mapReduce(x=>x*x, (x,y)=>x*y,1)(1,4)


