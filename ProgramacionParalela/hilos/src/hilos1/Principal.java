package hilos1;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

//De ejemplo de concurrencia entre dos procesos o hilos para el cual el resultado dependa del orden o la
//velocidad de la ejecución de los mismos. Indiqué qué parte del código constituiría la Sección Crítica de
//cada proceso.

public class Principal {
		public static void main(String[] args) throws IOException {
		int a;
		int b;
		BufferedReader entrada = new BufferedReader (new
		InputStreamReader (System.in));
		System.out.print("Ingrese el valor para a ");
		System.out.flush();
		String text1=entrada.readLine();
		a=Integer.parseInt(text1);
		System.out.print("Ingrese el valor para b ");
		System.out.flush();
		String text2=entrada.readLine();
		b=Integer.parseInt(text2);
		Hilo1 H1= new Hilo1(a,b);
		Hilo2 H2 = new Hilo2(a,b);
		H1.start();
		H2.start();
		}
		}
		
		

