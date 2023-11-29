package problemaConcurrencia;

public class HiloProd extends Thread{
	Bufer bufer;
	int gener,vueltas;
	public HiloProd(String s, Bufer b){
	super(s);
	bufer=b;
	}
	public void run(){
	for (vueltas=0;vueltas<20;vueltas++){
	gener=(int)(Math.random()*10);
	bufer.poneNum(gener);
	try {
	Thread.sleep((int)Math.random()*1000);
	}
	catch (InterruptedException ie){
	}
	} //fin del for
	}// fin del run()
	} // fin de la clase

