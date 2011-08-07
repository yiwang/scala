trait Ord {
    def < (that: Any): Boolean
    def <=(that: Any): Boolean = (this < that) || (this == that)
    def > (that: Any): Boolean = !(this <= that)
    def >=(that: Any): Boolean = !(this < that)
}

class Date(y: Int, m: Int, d: Int) extends Ord {
    def year = y
    def month = m
    def day = d
    override def toString(): String = year + "" + month + "" + day
    override def equals(that: Any): Boolean =
        that.isInstanceOf[Date] && {
            val o = that.asInstanceOf[Date]
            o.day == day && o.month == month && o.year == year
        }
    def < (that: Any):Boolean = {
        if(!this.isInstanceOf[Date])
            sys.error("cannot compare " + that + " and a Date")
        val o = that.asInstanceOf[Date]
        (year < o.year) ||
        (year == o.year && (month < o.month ||
                            (month == o.month && day < o.day)))
    }
}

object DateTest{
    def main(args: Array[String]){
        val d1 = new Date(2011,1,1)
        val d2 = new Date(2011,1,2)
        println(d1<d2)
    }
}
