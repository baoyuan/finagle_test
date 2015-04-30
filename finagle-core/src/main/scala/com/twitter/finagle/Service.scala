package com.twitter.finagle

import com.twitter.util.{ Future, Closable, Time }
import java.net.SocketAddress

object Service {
    def rescue[Req, Rep](service: Service[Req, Rep]) = new ServiceProxy[Req, Rep](service) {
        override def apply(request: Req) = {
            try {
                service(request)
            } catch {
                case e: Throwable => Future.exception(e)
            }
        }
    }

    def mk[Req, Rep](f: Req => Future[Rep]): Service[Req, Rep] = new Service[Req, Rep] {
        def apply(req: Req) = f(req)
    }

    def const[Rep](rep: Future[Rep]): Service[Any, Rep] =
        new service.ConstantService(rep)
}

abstract class Service[-Req, +Rep] extends (Req => Future[Rep]) with Closable {
    def map[Req1](f: Req1 => Req) = new Service[Req1, Rep] {
        def apply(req1: Req1) = Service.this.apply(f(req1))
        override def close(deadline: Time) = Service.this.close(deadline)
    }

    def apply(request: Req): Future[Rep]
    def close(deadline: Time) = Future.Done
    def status: Status = Status.Open
    def isAvailable: Boolean = status == Status.Open
}

trait ClientConnection extends Closable {
    def remoteAddress: SocketAddress
    def localAddress: SocketAddress
    def onClose: Future[Unit]
}

object ClientConnection {
    val nil: ClientConnection = new ClientConnection {
        private[this] val unconnected =
            new SocketAddress { override def toString = "unconnected" }
        def remoteAddress = unconnected
        def localAddress = unconnected
        def close(deadline: Time) = Future.Done
        def onClose = Future.never
    }
}

abstract class ServiceProxy[-Req, +Rep](val self: Service[Req, Rep])
    extends Service[Req, Rep] with Proxy {
    def apply(request: Req) = self(request)
    override def close(deadline: Time) = self.close(deadline)
    override def status = self.status
    override def isAvailable = self.isAvailable
    override def toString = self.toString()
}

object ServiceFactory {
    def const[Req, Rep](service: Service[Req, Rep]): ServiceFactory[Req, Rep] = new ServiceFactory[Req, Rep] {
        private[this] val noRelease = Future.value(new ServiceFactory[Req, Rep](service) {
            override def close(deadline: Time) = Future.Done
        })
        def apply(conn: ClientConnection) = noRelease
        def close(deadline: Time) = Future.Done
    }

    def apply[Req, Rep](f: () => Future[Service[Req, Rep]]): ServiceFactory[Req, Rep] =
        new ServiceFactory[Req, Rep] {
            def apply(_conn: ClientConnection) = f()
            def close(deadline: Time) = Future.Done
        }
}

abstract class ServiceFactory[-Req, +Rep](val self: Service[Req, Rep])
    extends (ClientConnection => Future[Service[Req, Rep]]) with Closable {
    self =>
    def apply(conn: ClientConnection): Future[Service[Req, Rep]]
    final def apply(): Future[Service[Req, Rep]] = this(ClientConnection.nil)
    def flatMap[Req1, Rep1](f: Service[Req, Rep] => Future[Service[Req1, Rep1]]): ServiceFactory[Req1, Rep1] =
        new ServiceFactory[Req1, Rep1] {
            def apply(conn: ClientConnection) =
                self(conn) flatMap { service =>
                    f(service) onFailure { _ => service.close() }
                }

            def close(deadline: Time) = self.close(deadline)
            override def isAvailable = self.isAvailable
            override def toString() = self.toString()
        }

    def map[Req1, Rep1](f: Service[Req, Rep] => Service[Req1, Rep1]): ServiceFactory[Req1, Rep1] =
        flatMap { s => Future.value(f(s)) }

    final def toService: Service[Req, Rep] = new FactoryToService(this)
    def status: Status = Status.Open
    def isAvailable: Boolean = status == Status.Open
}

object FactoryToService {
    val role = Stack.Role("FactoryToService")
    case class Enabled(enabled: Boolean){
        def mk():(Enabled, Stack.Param[Enabled]) = (this, Enabled.param)
    }
    object Enabled{
        implicit val param = Stack.Param(Enabled(false))
    }
    def module[Req, Rep]: Stackable[ServiceFactory[Req, Rep]]
}

class FactoryToService[Req, Rep](factory: ServiceFactory[Req, Rep])
    extends Service[Req, Rep]{
    
}

