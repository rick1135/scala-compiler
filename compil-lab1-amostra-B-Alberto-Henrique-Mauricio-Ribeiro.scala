case class ponto_t(x:Int, y:Int);
case class triangulo_t(a:ponto_t, b:ponto_t, c:ponto_t, cor:Int);

def func(v:Array[ponto_t], n:Int, T:triangulo_t):Double = {
  if (n <= 0) {
    return 1.0;
  } else if(n==1){
    return 1.01 + v(0).x / 1.0e2 + v(0).y / 0.1e-2 - T.a.x * T.a.x + T.b.y * T.c.x;
  }
  var res:Double = .25e-13;
  var i:Int = n-1;
  while(i >= 0 && v(i).x > 0){
    var temp:Double = v(i).y * v(i).x % 123;
    if(temp<0.0){
      res-=res*2.0e-2 +  func(v, n-1, T) * temp - T.a.y*T.cor;
    } else{
      res+=res*.3e3 +  func(v, n-2, T) * temp + T.c.x*T.cor;
      println("Estranho, ne?");
    }
    i -= 1;
  }
  return res;
}

def F2(T:triangulo_t):Int={
  var A:Int=0;
  var soma=new Array[Double](10);
  if ((T.a.x >= 10 || T.b.y > 20 || T.a.y < 30 || T.b.x <= 50) && ! (((T.c.x != 90) || (T.c.y == 0) ) ) ) {
    return 10 % 3;
  }
  else {
    A = 1;
  }
  while (A < 10) {
    var total:Int = 0;
    total += T.c.x * T.c.y;
    total += T.b.x * T.a.y;
    total += T.a.x * T.b.y;
    soma(A) = total % 100;
    A = A + 1;
  } 
  return A;
}