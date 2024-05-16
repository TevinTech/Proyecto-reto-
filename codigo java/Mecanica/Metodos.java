package Mecanica;

import java.util.ArrayList;

public class Metodos {
    private double vo;
    private double t;
    private double x;
    private double ang;
    private double y;
    private double voy;
    private double vox;
    private double g = 9.81;
    private double tmax;
    private double vy;
    private double ymax;
    private double xx;
    private double xy;
    private double tv;
    
    
    public Metodos() {
    }

    public Metodos(double vo, double t) {
        this.vo = vo;
        this.t = t;
    }

    public Metodos(double vo, double t, double ang) {
        this.vo = vo;
        this.t = t;
        this.ang = ang;
    }

    public double getVo() {
        return vo;
    }

    public void setVo(double vo) {
        this.vo = vo;
    }

    public double getT() {
        return t;
    }

    public void setT(double t) {
        this.t = t;
    }

    public double getX() {
        return x;
    }

    public void setX(double x) {
        this.x = x;
    }

    public double getAng() {
        return ang;
    }

    public void setAng(double ang) {
        this.ang = ang;
    }

    public double getY() {
        return y;
    }

    public void setY(double y) {
        this.y = y;
    }

    public double getVoy() {
        return voy;
    }

    public void setVoy(double voy) {
        this.voy = voy;
    }

    public double getVox() {
        return vox;
    }

    public void setVox(double vox) {
        this.vox = vox;
    }

    public double getG() {
        return g;
    }

    public void setG(double g) {
        this.g = g;
    }

    public double getTmax() {
        return tmax;
    }

    public void setTmax(double tmax) {
        this.tmax = tmax;
    }

    public double getVy() {
        return vy;
    }

    public void setVy(double vy) {
        this.vy = vy;
    }

    public double getYmax() {
        return ymax;
    }

    public void setYmax(double ymax) {
        this.ymax = ymax;
    }

    public double getXx() {
        return xx;
    }

    public void setXx(double xx) {
        this.xx = xx;
    }

    public double getXy() {
        return xy;
    }

    public void setXy(double xy) {
        this.xy = xy;
    }

    public double getTv() {
        return tv;
    }

    public void setTv(double tv) {
        this.tv = tv;
    }

    public void componenteHorizontalH(Metodos nuevoH){
        double vx;
        vx = nuevoH.getVo();
        System.out.println("La velocidad en x es: "+ vx+"m/s");
    }
    public void componenteVerticalH(Metodos nuevoH){
        double Vy;
        Vy = nuevoH.getG() * nuevoH.getT();
        System.out.println("La velocidad en y es: "+ Vy+"m/s");
    }
    public void MagnitudVelocidad(Metodos nuevoH){
        double vx,vy,v;
        vy = nuevoH.getG() * nuevoH.getT();
        vx = nuevoH.getVo();
        v= Math.sqrt(Math.pow(vx,2)*Math.pow(vy,2));
        System.out.println("La Magnitud de la velocidada es: "+ v+"m/s");
    }
    public void DireccionVelocidad(Metodos nuevoH){
        double vy,ang,vx,gra;
        vy = nuevoH.getG() * nuevoH.getT();
        vx = nuevoH.getVo();
        ang=Math.toRadians(vy/vx);
        gra = Math.atan(ang);
        System.out.println("La direccion de la velocidad es: "+ gra+"°");
    }
    public void horizontal(Metodos nuevoH){
        double x;
        x=nuevoH.getVo()*nuevoH.getT();
        System.out.println("El desplazamiento horizontal es: "+x+"m");
    }
    public void vertical(Metodos nuevoH){
        double y;
        y=(nuevoH.getG()*(Math.pow(nuevoH.getT(), 2)))/2;
        System.out.println("El desplazamiento vertical es: "+y+"m");
    }
    public void DesplazamientoTotal(Metodos nuevoH){
        double xt,x,y;
        y=(nuevoH.getG()*(Math.pow(nuevoH.getT(), 2)))/2;
        x=nuevoH.getVo()*nuevoH.getT();
        xt=Math.sqrt(Math.pow(y, 2)*Math.pow(x, 2));
        System.out.println("El desplazamiento Total es: "+xt+"m");
    }
    public void Alcance(Metodos nuevoH){
        double xm,tv,vx;
        tv=Math.sqrt(((2*nuevoH.getVo())/nuevoH.getG()));
        vx = nuevoH.getVo();
        xm=vx*tv;
        System.out.println("El Alcance es: "+xm+"m");
    }
    public void TiempoVuelo(Metodos nuevoH){
        double tv;
        tv=Math.sqrt(((2*nuevoH.getVo())/nuevoH.getG()));
        System.out.println("El Tiempo de vuelo es: "+tv+"s");
    }
    public void direccion(Metodos nuevoH){
        double d,x,y,gra;
        y=(nuevoH.getG()*(Math.pow(nuevoH.getT(), 2)))/2;
        x=nuevoH.getVo()*nuevoH.getT();
        d=Math.toRadians(y/x);
        gra = Math.atan(d);
        System.out.println("La dirreccion es: "+gra+"°");
    }
    public void AlturaTiempo(Metodos nuevoH){
        double y;
        y=nuevoH.getVo()-((nuevoH.getG()*Math.pow(nuevoH.getT(), 2))/2);
        System.out.println("La altura en funcion del tiempo es: "+y+"m");
    }
    public void componenteHorizontalI(Metodos nuevoI){
        double vox,ang,gra;
        ang=Math.toRadians(nuevoI.getAng());
        gra = Math.cos(ang);
        vox = nuevoI.getVo()*gra;
        System.out.println("La Velocidad de lanzamiento en x es: "+ vox);
    }
    public void componenteVerticalI(Metodos nuevoI){
        double voy,ang,gra;
        ang=Math.toRadians(nuevoI.getAng());
        gra = Math.sin(ang);
        voy = nuevoI.getVo()*gra;
        System.out.println("La Velocidad de lanzamiento en y es: "+ voy);
    }
    public void componenteHorizontalITiempo(Metodos nuevoI){
        double vx,vox,ang,gra;
        ang=Math.toRadians(nuevoI.getAng());
        gra = Math.cos(ang);
        vox = nuevoI.getVo()*gra;
        vx =vox;
        System.out.println("La velocidad en funcion del tiempo en x es: "+ vx+"m/s");
    }
    public void componenteVerticalITiempo(Metodos nuevoI){
        double vy,voy,ang,gra;
        ang=Math.toRadians(nuevoI.getAng());
        gra = Math.sin(ang);
        voy = nuevoI.getVo()*gra;;
        vy =voy-(nuevoI.getG()*nuevoI.getT());
        System.out.println("La velocidad en funcion del tiempo en y es: "+ vy+"m/s");
    }
    public void MagnitudVelocidadI(Metodos nuevoI){
        double vy,voy,ang,gra;
        ang=Math.toRadians(nuevoI.getAng());
        gra = Math.sin(ang);
        voy = nuevoI.getVo()*gra;;
        vy =voy-(nuevoI.getG()*nuevoI.getT());
        double vx,vox,angx,grax,v;
        angx=Math.toRadians(nuevoI.getAng());
        grax = Math.cos(angx);
        vox = nuevoI.getVo()*grax;
        vx =vox;
        v= Math.sqrt(Math.pow(vx,2)*Math.pow(vy,2));
        System.out.println("La magnitud de la velocidad en funcoion del tiempo es: "+ v+"m/s");
    }
    public void DireccionVelocidadI(Metodos nuevoI){
    double vy,voy,angy,gray;
        angy=Math.toRadians(nuevoI.getAng());
        gray = Math.sin(angy);
        voy = nuevoI.getVo()*gray;;
        vy =voy-(nuevoI.getG()*nuevoI.getT());
        double vx,vox,angx,grax,ang,gra;
        angx=Math.toRadians(nuevoI.getAng());
        grax = Math.cos(angx);
        vox = nuevoI.getVo()*grax;
        vx =vox;
        ang=Math.toRadians(vy/vx);
        gra = Math.atan(ang);
        System.out.println("La magnitud de la velocidad en funcoion del tiempo es: "+ gra+"°");
    }
    public void Desplazamientohorizontal(Metodos nuevoI){
        double x,vox,ang,gra;
        ang=Math.toRadians(nuevoI.getAng());
        gra = Math.cos(ang);
        vox = nuevoI.getVo()*gra;
        x=vox*nuevoI.getT();
        System.out.println("El desplazamiento Horizontal es: "+ x+"m");
    }
    public void DesplazamientoVertical(Metodos nuevoI){
        double y,voy,ang,gra;
        ang=Math.toRadians(nuevoI.getAng());
        gra = Math.sin(ang);
        voy = nuevoI.getVo()*gra;
        y=voy*nuevoI.getT()-((nuevoI.getG()*Math.pow(nuevoI.getT(), 2))/2);
        System.out.println("El desplazamiento Vertical es: "+ y+"m");
    }
    public void AlcanceMaximo(Metodos nuevoI){
        double xmax,vx,vox,angx,grax;
        angx=Math.toRadians(nuevoI.getAng());
        grax = Math.cos(angx);
        vox = nuevoI.getVo()*grax;
        vx =vox;
        double tv,t,voy,angy,gray;
        angy=Math.toRadians(nuevoI.getAng());
        gray = Math.sin(angy);;
        voy = nuevoI.getVo()*gray;
        t=voy/nuevoI.getG();
        tv=2*t;
        xmax=vx*tv;
        System.out.println("EL alcance maximo es: "+xmax+"m/s");
    }
    public void AlturaMaximo(Metodos nuevoI){
        double y,voy,ang,gra;
        ang=Math.toRadians(nuevoI.getAng());
        gra = Math.sin(ang);
        voy = nuevoI.getVo()*gra;
        y=(Math.pow(voy, 2))/(2*nuevoI.getG());
        System.out.println("La altura maxima es: "+ y+"m");
    }
    public void tiempoMaximo(Metodos nuevoI){
        double t,voy,ang,gra;
        ang=Math.toRadians(nuevoI.getAng());
        gra = Math.sin(ang);
        voy = nuevoI.getVo()*gra;
        t=voy/nuevoI.getG();
        System.out.println("El tiempo maximo es: "+ t+"s");
    }
    public void tiempoVuelo(Metodos nuevoI){
        double tv,t,voy,ang,gra;
        ang=Math.toRadians(nuevoI.getAng());
        gra = Math.sin(ang);
        voy = nuevoI.getVo()*gra;
        t=voy/nuevoI.getG();
        tv=2*t;
        System.out.println("El tiempo de vuelo es: "+ tv+"s");
    }    
}
