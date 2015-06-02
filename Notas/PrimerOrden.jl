module PrimerOrden
using Intervalos
using RungeKutta
using PyPlot



export zetasKas, kaesimaZeta, graficaCono,conosInterv,uneInterv,envolvente,graficaEnvolvente


function zetasKas(tI,tF,x0,z0::Intervalo,f::Function)  
    x0 + Intervalo(tI,tF)*f(z0)
end


function kaesimaZeta(tI,tF,x0,z0::Intervalo,f::Function,k::Int64)
    zetas=[z0]
    for i=1:k
        zk=zetasKas(tI,tF,x0,zetas[i],f)
        if zk==zetas[i]
            return zetas
        else
            push!(zetas,zk)
        end
    end
    return zetas
end





####### PARA GRAFICAR Y PARA SACAR LOS SUBINTERVALOS


function graficaCono(tI,tF,x0,zk::Intervalo,col::ASCIIString)
    PyPlot.fill_between([tI,tF],[x0,float64(zk.a)],[x0,float64(zk.b)],color=col)
    #PyPlot.xlim(min(0.9*tI,1.1*tI),max(0.9*tF,1.1*tF))
    #PyPlot.ylim(min(0.9*float64(zk.a),1.1*float64(zk.a)),max(0.9*float64(zk.b),1.1*float64(zk.b)))
    return nothing
end

graficaCono(tI,tF,x0,zk::Intervalo)=graficaCono(tI,tF,x0,zk,"lightblue")


function conosInterv(tI,tF,x0::Intervalo,f::Function)
    t=tF-tI
    cono1=x0.a + t*f(x0)    
    cono2=x0.b + t*f(x0)
    conote=uneInterv([cono1],[cono2])
    return conote[1]
end

function uneInterv(conoA::Array{Intervalo,1},conoB::Array{Intervalo,1})
    # esta función toma dos arreglos de intervalos y los une aunque no se intersecten
    if length(conoA)!=length(conoB)
        error("Deben tener mismo tamaño")
    end
    if conoA[1].a≥conoB[1].a && conoA[end].a≥conoB[end].a
        bajo=conoB
        alto=conoA
    else
        bajo=conoA
        alto=conoB
    end
    cono=Intervalo[]
    for i=1:length(conoA)
        push!(cono,Intervalo((bajo[i]).a,(alto[i]).b))
    end
    return cono
end


function graficaCono(tI,tF,x0::Intervalo,zk::Intervalo,col::ASCIIString)
    PyPlot.fill_between([tI,tF],Float64[x0.a,zk.a],Float64[x0.b,zk.b],color=col)
    PyPlot.plot(tI*[1,1],Float64[x0.a,x0.b],"--",color="black")
    PyPlot.plot(tF*[1,1],Float64[zk.a,zk.b],"--",color="black")
    #PyPlot.xlim(min(0.9*tI,1.1*tI),max(0.9*tF,1.1*tF))
    #PyPlot.ylim(min(0.9*float64(zk.a),1.1*float64(zk.a)),max(0.9*float64(zk.b),1.1*float64(zk.b)))
    return nothing
end


graficaCono(tI,tF,x0::Intervalo,zk::Intervalo)=graficaCono(tI,tF,x0,zk,"lightblue")







##########################PARTICIONAR EN EL TIEMPO

function envolvente(tI,tF,x0,z0::Intervalo,f::Function,n::Int64)
    # Esta función hace el cálculo para una z0 dada y partiendo el tiempo en n pedazos    
    envolv=Intervalo[]
    tiempo=linspace(tI,tF,2+n)
    x1=zetasKas(tiempo[1],tiempo[2],x0,z0,f)
    push!(envolv,x1)
    for i=1:n
        push!(envolv,conosInterv(tiempo[2+n-1],tiempo[2+n],envolv[i],f))
    end
    return envolv
end

function graficaEnvolvente(tI,tF,x0,envolv::Array{Intervalo,1},col::ASCIIString)
    # Esta función grafica lo que hace la función envolvente
    n=length(envolv)-1
    tiempo=linspace(tI,tF,2+n)
    graficaCono(tiempo[1],tiempo[2],x0,envolv[1],col)
    for i=1:n
        graficaCono(tiempo[1+i],tiempo[2+i],envolv[i],envolv[1+i],col)
    end
    return nothing
end

graficaEnvolvente(tI,tF,x0,envolv::Array{Intervalo,1})=graficaEnvolvente(tI,tF,x0,envolv,"lightblue")


function ptosenvolventes(t0,h,n,x0,f,m) 
    T=BuscarTBuena(x0,h,n,f) 
    z0= zetacero(x0,h,n,f) 


    nuevaListaXs=envolvente(t0,T,x0,z0,f,m);

    z0= zetacero(x0,h,n,f) 
    arriba=maximum([(nuevaListaXs[i].b) for i in 1:length(nuevaListaXs)]);
    abajo=minimum([(nuevaListaXs[i].a) for i in 1:length(nuevaListaXs)]);
    x_k=Intervalo(abajo,arriba)
end


function ptosenvolventes(t0,h,n,x0,f,m) 
    T=BuscarTBuena(x0,h,n,f); 
    z0= zetacero(x0,h,n,f); 
    nuevaListaXs=envolvente(t0,T,x0,z0,f,m);
    
    arriba=maximum([(nuevaListaXs[i].b) for i in 1:length(nuevaListaXs)]);
    abajo=minimum([(nuevaListaXs[i].a) for i in 1:length(nuevaListaXs)]);

    x_k=Intervalo(abajo,arriba)
end



end
