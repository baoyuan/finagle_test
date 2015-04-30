package com.twitter.finagle

import com.twitter.io.Buf

/**
 * A Path comprises a sequence of byte buffers naming a hierarchically-addressed object
 */
case class Path(elems: Buf*) {
    
}

object Path{
    val empty = Path()
}