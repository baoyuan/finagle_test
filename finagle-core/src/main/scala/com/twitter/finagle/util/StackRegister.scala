package com.twitter.finagle.util

import com.twitter.finagle.Stack
import scala.actors.threadpool.AtomicInteger
import com.twitter.finagle.param.Label
import com.twitter.util.registry.GlobalRegistry

object StackRegistry {
    /**
     * Represents an entry in the registry
     */
    case class Entry(addr: String, stack: Stack[_], params: Stack.Params) {
        val modules: Seq[Module] = stack.tails.map { node =>
            val raw = node.head.parameters.map { p => params(p) }
            val reflected = raw.foldLeft(Seq.empty[(String, String)]) {
                case (seq, p: Product) =>
                    val fields = p.getClass.getDeclaredFields.map(_.getName).toSeq
                    val values = p.productIterator.map(_.toString).toSeq
                    seq ++ (fields.zipAll(values, "<unknown>", "<unknown>"))
                case (seq, _) => seq
            }
            Module(node.head.role.name, node.head.description, reflected)
        }.toSeq
        val name: String = params[Label].label
    }

    case class Module(name: String, description: String, fields: Seq[(String, String)])
}

trait StackRegistry {
    import StackRegistry._
    def registryName: String

    private[this] var registry = Map.empty[String, Entry]
    private[this] val numEntries = new AtomicInteger(0)

    def register(addr: String, stk: Stack[_], params: Stack.Params): Unit = {
        val entry = Entry(addr, stk, params)
        addEntries(entry)
        synchronized { registry += entry.name -> entry }
    }

    def unregister(addr: String, stk: Stack[_], params: Stack.Params): Unit = {
        val entry = Entry(addr, stk, params)
        synchronized { registry -= entry.name }
        removeEntries(entry)
    }

    private[this] def addEntries(entry: Entry): Unit = {
        val gRegistry = GlobalRegistry.get
        entry.modules.foreach {
            case Module(paramName, _, reflected) =>
                reflected.foreach {
                    case (field, value) =>
                        val key = Seq(registryName, entry.name, entry.addr, paramName, field)
                        if (gRegistry.put(key, value).isEmpty)
                            numEntries.incrementAndGet()
                }
        }
    }

    private[this] def removeEntries(entry: Entry): Unit = {
        val gRegistry = GlobalRegistry.get
        val name = entry.name
        entry.modules.foreach {
            case Module(paramName, _, reflected) =>
                reflected.foreach {
                    case (field, value) =>
                        val key = Seq(registryName, name, entry.addr, paramName, field)
                        if (gRegistry.remove(key).isDefined)
                            numEntries.decrementAndGet()
                }
        }
    }

    def size: Int = numEntries.get
    def registrants: Iterable[Entry] = synchronized { registry.values }

    private[finagle] def clear(): Unit = synchronized { registry = Map.empty[String, Entry] }
}