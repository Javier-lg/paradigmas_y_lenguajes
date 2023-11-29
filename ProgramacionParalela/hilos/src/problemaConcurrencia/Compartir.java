package problemaConcurrencia;

public class Compartir {
	public static void main(String[] args) {
		Bufer bufer;
		bufer=new Bufer();
		HiloProd productor=new HiloProd("Productor",bufer);
		HiloCons consumidor=new HiloCons(bufer);
		productor.start();
		consumidor.start();
		}
}
