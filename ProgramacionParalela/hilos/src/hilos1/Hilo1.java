package hilos1;
public class Hilo1 extends Thread {
	int a;
	int b;
	public Hilo1(int a, int b) {
	this.a=a;
	this.b=b;
	}
	public void run () {
	//SECCION CRITICA
	a=a+1;
	b=b+1;
	System.out.println("Hilo1: a = "+a);
	System.out.println("Hilo1: b = "+b);
	//FIN DE SECCION CRITICA
	}
	}
