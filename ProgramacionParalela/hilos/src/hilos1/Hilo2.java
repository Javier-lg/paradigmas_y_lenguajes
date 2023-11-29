package hilos1;
public class Hilo2 extends Thread {
	int a;
	int b;
	public Hilo2(int a, int b) {
	this.a=a;
	this.b=b;
	}
	
	public void run () {
	//SECCION CRITICA
	a=a*3;
	b=b*2;
	System.out.println("a="+a);
	System.out.println("b="+b);
	//FIN DE SECCION CRITICA
	}
}
