//object rationals{
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  x - y - z
  (y + y).m_denom
//}

class Rational(num: Int, denom: Int){
  private val gcd_num_denom = gcd(num, denom)
  def m_num = num // gcd_num_denom
  def m_denom = denom // gcd_num_denom

  private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
  def +(that: Rational) =
    new Rational(m_num * that.m_denom + that.m_num * m_denom, m_denom * that.m_denom)

  def unary_- : Rational = new Rational(-m_num, m_denom)

  def -(that: Rational): Rational = this + -that

  def <(that: Rational): Boolean = m_num * that.m_denom < that.m_num * m_denom

  def max(that: Rational): Rational = if(this < that) that else this

  override def toString: String = m_num / gcd_num_denom + " / " + m_denom / gcd_num_denom
}