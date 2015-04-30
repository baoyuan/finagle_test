package com.twitter.finagle

trait Client[Req, Rep] {
    
    def newService(dest: Name, label: String) : Service[Req, Rep] = {
        val client = newClient(dest, label)
        new FactoryToService[Req, Rep](client)
    }
    
    final def newServie(dest: String) : Service[Req, Rep] = {
        val (n, l) = Resolver.evalLabeled(dest)
        newService(n, l)
    }
    
    final def newClient(dest: String): ServiceFactory[Req, Rep] = {
        val (n, l) = Resolver.evalLabeled(dest)
        newClient(n, l)
    }
    
    final def newClient(dest: String, label: String): ServiceFactory[Req, Rep] = 
        newClient(Resolver.eval(dest), label)
        
    def newClient(dest: Name, label: String): ServiceFactory[Req, Rep]
}