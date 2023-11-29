package problemaConcurrencia;

public class Bufer {
	private int valor;
	public Bufer(){
	valor=0;
	}
	public void poneNum(int p){
	valor=p;
	System.out.println("Mi valor es "+valor);
	}
	public int sacaNum(){
	System.out.println("Entregue un "+valor);
	return valor;
	}
	} // fin clase Bufer

