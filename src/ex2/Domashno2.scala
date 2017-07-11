sealed trait Shape
case class Triangle(a: Int, b: Int, c: Int, h: Int) extends Shape // h represent the height against the longest side of the triangle
case class Rectangle(a: Int, b: Int) extends Shape
case class Trapezoid(a: Int, b: Int, h: Int) extends Shape
case class Cube() extends Shape

object Domashno2 {
  //Zadacha 1
  def max(xs: List[Int]): Option[Int] = {
  def it(xs: List[Int],Max:Int): Option[Int] = {
  if(xs.isEmpty) Some(Max) 
  else {
  it(xs.tail,if (xs.head > Max) xs.head else Max);
  }
  }
  if(xs.isEmpty) None else it(xs.tail, xs.head);
  }
  //Zadacha 2
  def isTriangle(t:Triangle)={
  if(t.a + t.b > t.c && t.b + t.c > t.a && t.a + t.c > t.b) true else false;
  }
  def triangleType(t: Triangle): String =  {
  if(!isTriangle(t)) return "no";
  if (t.a != t.b && t.b != t.c && t.a!=t.c){
  val aSquare = t.a*t.a;
  val bSquare = t.b*t.b;
  val cSquare = t.c*t.c;
  if((aSquare + bSquare == cSquare) || (aSquare + cSquare == bSquare) || (bSquare + cSquare == aSquare)) "rect" else "random";
  }else if(t.a == t.b && t.b == t.c) "equilateral";
  else "isosceles";
  }
  //Zadacha 3
  def area(s: Shape): Double = s match{
  case t:Triangle => triangleType(t) match{
  case "rect" => max(List(t.a,t.b,t.c)).get match{
  case t.a => (t.b * t.c) / 2;
  case t.b => (t.a * t.c) / 2;
  case t.c => (t.a * t.b) / 2;
  }
  case "no" => -2;
  case _ => (max(List(t.a,t.b,t.c)).get * t.h) .toDouble / 2;
  }
  case Rectangle(a,b) => a*b;
  case Trapezoid(a,b,h) => (a + b).toDouble * h / 2;
  case Cube() =>  -1;
  }

 
  def findRectangulars(shapes: List[Shape]): Int = {
  def it(shapes: List[Shape], n: Int): Int = {
  if(shapes.isEmpty) n;
  else it(shapes.tail,shapes.head match{
  case t:Triangle => if(triangleType(t).equals("rect")) n+1 else n;
  case s:Shape => n; 
  });       
  }
  it(shapes, 0);
  }
  
 
}