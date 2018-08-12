class Rationals(x: Int, y: Int){
  private def gcd(x:Int, y:Int): Int = if(y==0) x else gcd(y, x%y)
  val numer = x / gcd(x,y)
  val denom = y / gcd(x,y)

  override def toString: String = numer + "/" + denom

  def neg: Rationals = new Rationals(-this.numer, this.denom)

  def add(that : Rationals): Rationals ={
    new Rationals(
      that.numer*this.denom + that.denom*this.numer, that.denom*this.denom
    )
  }

  def less(that : Rationals) = that.denom*this.numer<that.numer*this.denom

  def max(that:Rationals):Rationals = if(this.less(that)) that else this

  def subtract(that:Rationals) : Rationals = add(that.neg)
}

val r1 = new Rationals(1,2)
val r2 = new Rationals(2,3)
val r3 = new Rationals(3,4)
r1.add(new Rationals(1,2)).toString
r1.neg.toString
r1.add(r1)
r1.subtract(r2).subtract(r3).toString
r1.less(r2)
r1.max(r2)


