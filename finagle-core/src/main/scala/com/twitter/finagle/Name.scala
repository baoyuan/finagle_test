package com.twitter.finagle

import com.twitter.util.Var
import com.twitter.finagle.util.Showable
import java.net.SocketAddress

/**
 * Names identify network locations
 */
sealed trait Name

object Name {

    case class Path(path: com.twitter.finagle.Path) extends Name

    class Bound private (
        val addr: Var[Addr],
        val id: Any,
        val path: com.twitter.finagle.Path) extends Name with Proxy {
        def self = id
        def canEqual(that: Any) = true
    }

    object Bound {
        def apply(addr: Var[Addr], id: Any, path: com.twitter.finagle.Path): Name.Bound =
            new Bound(addr, id, path)

        def apply(addr: Var[Addr], id: Any): Name.Bound =
            apply(addr, id, com.twitter.finagle.Path.empty)

        def unapply(name: Name.Bound): Option[Var[Addr]] = Some(name.addr)

        def singleton(addr: Var[Addr]): Name.Bound = Name.Bound(addr, new {})
    }
    
    implicit val showable: Showable[Name] = new Showable[Name] {
        def show(name: Name) = name match {
            case Path(path) => path.show
            case bound@Bound(_) => bound.id.toString
        }
    }
    
    def bound(addrs: SocketAddress*): Name.Bound = 
        Name.Bound(Var.value(Addr.Bound(addrs: _*)), addrs.toSet)
        
    val empty: Name.Bound = bound()
    
    def fromGroup(g: Group[SocketAddress]): Name.Bound = g match{
        
    }
    
    def apply(path: com.twitter.finagle.Path): Name =
        Name.Path(path)
        
    def apply(path: String): Name = 
        Name.Path(com.twitter.finagle.Path.read(path))
     
    def all(names: Set[Name.Bound]): Name.Bound = 
        if(names.isEmpty) empty
        else if(names.size == 1) names.head
        else {
            
        }
    def DONOTUSE_nameToGroup(name: Name) : Group[SocketAddress] = {
        val bound@Name.Bound(_) = name
        NameGroup(bound)
    }
}