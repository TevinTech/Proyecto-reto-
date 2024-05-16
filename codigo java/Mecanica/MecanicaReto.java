package Mecanica;

import java.util.ArrayList;
import java.util.Scanner;

public class MecanicaReto { 
    Metodos nt=new Metodos();
    public static void main(String[] args) {
        Scanner leer=new Scanner(System.in);
        MecanicaReto tmp=new MecanicaReto();
        int sw=0,st=0,sk=0;
        double vo,t,ang;
        System.out.println("---LANZAMIENTO DE PROYECTILES---");
                do{
                    System.out.println("***************");
                    System.out.println("---Opciones---");
                    System.out.println("1. Lanzamiento Horizontal.");
                    System.out.println("2. Lanzamiento Inclinado.");
                    System.out.println("3. Salir.");
                    System.out.print("-");
                    sw=leer.nextInt();
                    switch(sw){
                        case 1:
                            System.out.println("Ingrese la velocidad inicial.");
                            vo=leer.nextDouble();
                            System.out.println("Ingrese el tiempo.");
                            t=leer.nextDouble();
                            Metodos nuevoH=new Metodos(vo,t);
                            tmp.LanzamientoHorizontal(nuevoH);
                            break;
                        case 2:
                            System.out.println("Ingrese la velocidad inicial.");
                            vo=leer.nextDouble();
                            System.out.println("Ingrese el tiempo.");
                            t=leer.nextDouble();
                            System.out.println("Ingrese el angulo.");
                            ang=leer.nextDouble();
                            Metodos nuevoI=new Metodos(vo,t,ang);
                            tmp.LanzamientoInclinado(nuevoI);
                            break;
                        case 3:
                            System.out.println(" ");
                            System.out.println("Fin Programa.");
                        default:
                            System.out.println(" ");
                            System.out.println("Opcion Invalida.");
                            System.out.println(" ");
                            System.out.println("Elija una opcion.");
                            break;
                    }
                }
                while(sw!=3);
    }
    public void LanzamientoHorizontal (Metodos nuevoH){
        Scanner leer=new Scanner(System.in);
        int sw=0,st=0;
        do{
            System.out.println(" ");
            System.out.println("1. Velocidad.");
            System.out.println("2. Desplazamiento.");
            System.out.println("3. Altura(Posición) en funcion del tiempo.");
            System.out.println("4. Tiempo de Vuelo.");
            System.out.println("5. Direccion(Desplazamiento).");
            System.out.println("6. Salir.");
            System.out.print("-");
            sw=leer.nextInt();
            if(sw==1){
                do{
                    System.out.println(" ");
                    System.out.println("1. Componente Horizontal");
                    System.out.println("2. componente Vertical.");
                    System.out.println("3. Magnitud de la Velocidad.");
                    System.out.println("4. Direccion de la Velocidad.");
                    System.out.println("5. Salir.");
                    System.out.print("- ");
                    st=leer.nextInt();
                    switch(st){
                        case 1:
                            nt.componenteHorizontalH(nuevoH);
                                break;
                        case 2:
                            nt.componenteVerticalH(nuevoH);
                            break;
                        case 3:
                            nt.MagnitudVelocidad(nuevoH);
                            break;
                        case 4:
                            nt.DireccionVelocidad(nuevoH);
                            break;
                        case 5:
                            System.out.println(" ");
                            System.out.println("Fin calculo velocidades.");
                            break;
                        default:
                            System.out.println("OPCIÓN INVALIDA");
                            System.out.println("");
                            break;
                    }
                   }while(st!=5);
               }
            if(sw==2){
                do{
                    System.out.println(" ");
                System.out.println("1. Horizontal");
                System.out.println("2. Vertical.");
                System.out.println("3. Desplazamiento Total.");
                System.out.println("4. Alcance.");
                System.out.println("5. Salir.");
                System.out.print("- ");
                st=leer.nextInt();
                 switch(st){
                    case 1:
                        nt.horizontal(nuevoH);
                        break;
                    case 2:
                        nt.vertical(nuevoH);
                        break;
                    case 3:
                        nt.DesplazamientoTotal(nuevoH);
                        break;
                    case 4:
                        nt.Alcance(nuevoH);
                        break;
                    case 5:
                        System.out.println(" ");
                        System.out.println("Fin calculo Desplazamientos.");
                        break;
                    default:
                        System.out.println("OPCIÓN INVALIDA");
                        System.out.println("");
                        break;
                 }
                }while(st!=5);
            }
            if(sw==3){
                nt.AlturaTiempo(nuevoH);
            }
            if(sw==4){
                nt.TiempoVuelo(nuevoH);
            }
            if(sw==5){
                nt.direccion(nuevoH);
            }
            if(sw==6){
                System.out.println("Fin del ejercicio.");
            }
        }
        while(sw!=6);        
    }
    public void LanzamientoInclinado (Metodos nuevoI){
        Scanner leer=new Scanner(System.in);
        int sw=0,st=0;
        do{
            System.out.println(" ");
            System.out.println("1. Velocidad en el lanzamiento.");
            System.out.println("2. Velocidad en funcion del tiempo.");
            System.out.println("3. Desplazamiento.");
            System.out.println("4. Tiempo.");
            System.out.println("5. Salir.");
            System.out.print("-");
            sw=leer.nextInt();
            if(sw==1){
                do{
                    System.out.println(" ");
                    System.out.println("1. Componente Horizontal");
                    System.out.println("2. componente Vertical.");
                    System.out.println("3. Salir.");
                    System.out.print("- ");
                    st=leer.nextInt();
                    switch(st){
                        case 1:
                            nt.componenteHorizontalI(nuevoI);
                                break;
                        case 2:
                            nt.componenteVerticalI(nuevoI);
                            break;
                        case 3:
                            System.out.println(" ");
                            System.out.println("Fin calculo velocidades.");
                            break;
                        default:
                            System.out.println("OPCIÓN INVALIDA");
                            System.out.println("");
                            break;
                    }
                   }while(st!=3);
               }
            if(sw==2){
                do{
                    System.out.println(" ");
                    System.out.println("1. Horizontal");
                    System.out.println("2. Vertical.");
                    System.out.println("3. Magnitud de Velocidad.");
                    System.out.println("4. Direccion de la velocidad.");
                    System.out.println("5. Salir.");
                    System.out.print("- ");
                    st=leer.nextInt();
                    switch(st){
                        case 1:
                            nt.componenteHorizontalITiempo(nuevoI);
                            break;
                        case 2:
                            nt.componenteVerticalITiempo(nuevoI);
                            break;
                        case 3:
                            nt.MagnitudVelocidadI(nuevoI);
                            break;
                        case 4:
                            nt.DireccionVelocidadI(nuevoI);
                            break;
                        case 5:
                            System.out.println("Fin velocidade en funcion de tiempo");
                            break;
                        default:
                            break;
                    }
                }while(st!=5);
            }   
            if(sw==3){
                do{
                    System.out.println(" ");
                    System.out.println("1. Horizontal");
                    System.out.println("2. Vertical.");
                    System.out.println("3. Alcance Maximo.");
                    System.out.println("4. Altura Maxima.");
                    System.out.println("5. Salir.");
                    System.out.print("- ");
                    st=leer.nextInt();
                    switch(st){
                        case 1:
                            nt.Desplazamientohorizontal(nuevoI);
                            break;
                        case 2:
                            nt.DesplazamientoVertical(nuevoI);
                            break;
                        case 3:
                            nt.AlcanceMaximo(nuevoI);
                            break;
                        case 4:
                            nt.AlturaMaximo(nuevoI);
                            break;
                        case 5:
                            System.out.println(" ");
                            System.out.println("Fin Desplazamientos.");
                            break;
                        default:
                            break;
                    }
                }while(st!=5);
            }
            if(sw==4){
                do{
                    System.out.println(" ");
                    System.out.println("1. Tiempo Maximo");
                    System.out.println("2. Tiempo De Vuelo.");
                    System.out.println("3. Salir.");
                    System.out.print("- ");
                    st=leer.nextInt();
                    switch(st){
                        case 1:
                            nt.tiempoMaximo(nuevoI);
                            break;
                        case 2:
                            nt.tiempoVuelo(nuevoI);
                            break;
                        case 3:
                            System.out.println("Fin velocidade en funcion de tiempo");
                            break;
                        default:
                            System.out.println("Fin Calculo de tiempo.");
                            break;
                    }
                }while(st!=3);
            }
            if(sw==5){
                System.out.println("Fin del ejercicio.");
            }
        }
        while(sw!=5);        
    }
}
