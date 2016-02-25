/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package trie

case class Data[T](positions: Seq[Int] = Seq.empty[Int], 
                   value: Option[T] = None) {

  type Index = Int
  
  type Position = Int

  def dimension: Int = positions.length

  def isEmpty: Boolean = value.isEmpty 

  def foreachPosition(f: (Index, Position) => Unit) = 
    positions.zipWithIndex.foreach { case (pos, idx) => f(idx, pos) }

}

protected[trie] class Node {

  protected[trie] var key = Int.MinValue

  protected[trie] val children = new java.util.HashMap[Int, Node]

  protected[trie] var value: Option[Any] = None

  protected[trie] var valueType: Option[Class[_]] = None

  protected[trie] var isLeaf: Boolean = false

  protected[trie] def hasValue: Boolean = None.equals(value)

}

protected[trie] class Trie {

  protected[trie] val root = new Node

  def insert[T](data: Data[T]) = if(!data.isEmpty) {
    var children = root.children
    data.foreachPosition { case (idx, pos) => 
      val node = if(children.containsKey(pos)) children.get(pos) else {
        val tmp = new Node
        children.put(pos, tmp)
        tmp 
      }
      children = node.children
      if(idx == data.dimension - 1) {
        node.value = data.value
        node valueType = data.value.map(_.getClass)
        node.isLeaf = true
      }
    }
  }

  protected[trie] def findBy(positions: Seq[Int]): Option[Node] = {
    var children = root.children
    var node: Option[Node] = None
    positions.foreach { pos => if(children.containsKey(pos)) 
      node = Option(children.get(pos)) else return None 
      children = node.map(_.children).
                      getOrElse(new java.util.HashMap[Int, Node])
    } 
    node
  }

}
