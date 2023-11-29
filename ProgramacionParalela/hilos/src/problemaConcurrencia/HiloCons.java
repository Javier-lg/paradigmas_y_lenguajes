package problemaConcurrencia;

public 	class HiloCons extends Thread{
		Bufer bufer;
		int consum,vueltas;
		public HiloCons(Bufer b){
		bufer=b;
		}
		public void run(){
		for (vueltas=0; vueltas<20; vueltas++){
		consum=bufer.sacaNum();
		try {
		Thread.sleep((int)Math.random()*1000);
		} catch (InterruptedException ie){
		}
		} //del for
		} //del run
		} // de la clase HiloCons

