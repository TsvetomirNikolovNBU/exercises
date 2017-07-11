

object Domashno1 {
  //Zadacha 1
  def length(data:List[Int]):Int={
  def lt(data:List[Int],res:Int):Int={
  if(data.isEmpty)res
  else lt(data.tail,res+1)
  }
  lt(data,0)
  }
  
  //Zadacha 2
  def ifelse(cond: Boolean, onTrue: => Int, onFalse: => Int) = {
  if (cond) onTrue else onFalse;
  }
  //Zadacha 3
  def balance(chars: List[Char]):Boolean={
  def bal(chars: List[Char],open:Int,close:Int): Boolean = {
  if(close > open){
  return false;
  }
  if(chars.isEmpty)
  if(open == close)true else false;
  else { 
  if(chars.head == ')') 
  bal(chars.tail, open, close+1); 
  else if(chars.head == '(')
  bal(chars.tail, open+1, close); 
  else bal(chars.tail, open, close); 
  }
  }
  if(chars.isEmpty){
  return true;
  }
  if(chars.head == ')') false;
  else bal(chars, 0, 0);
  }
  //Zadacha 4
  def map(chars: List[Char], f: (Char) => Any)={
  def im(chars: List[Char], f: (Char) => Any, mapp: List[Any]): List[Any] = {
  if (chars.isEmpty) mapp;
  else {
  im(chars.tail, f, mapp :+ f(chars.head));
  }
  }
  im(chars, f, List());
  }
  //Zadacha 5
  def toUpperCase(chars: List[Char]) = {   def upperCase(char: Char) = {
  if(char >= 97 && char <= 122) (char - 32).toChar
  else char
  }
  map(chars, upperCase);
  }
  //Zadacha 6
  def exists(data: List[Int], f: (Int) => Boolean): Boolean = {
  if (data.isEmpty) false;
  else if (f(data.head)) true; else exists(data.tail, f);
  }
  //Zadacha 7
  def filter(data: List[Int], f: (Int) => Boolean) = {
  def inf(data: List[Int], f: (Int) => Boolean, filt: List[Int]): List[Int] = {
  if (data.isEmpty) filt;
  else {
  if (f(data.head)) inf(data.tail, f, filt :+ data.head)
  else inf(data.tail, f, filt);
  }
  }
  inf(data, f, List());
  }
  //Zadahca 8
  def forall(data: List[Int], f: (Int) => Boolean): Boolean = {
  !exists(data, !f(_));
  }
  //Zadacha 9
  def pascal(a:Int,res:Int):Int = {
  def pi(a: Int, res: Int): Int = {   
  if(a == 0 || a == res) 1;
  else pi(a-1, res-1) + pi(a,res-1);
  }
  if(a>res || (a<0 || res<0)){
  return -1;
  }
  pi(a, res);
  }

  
}