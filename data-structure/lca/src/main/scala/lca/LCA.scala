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
package lca

import scala.collection.JavaConversions._

object LCA {

  case class Node(id: String) {
    var children = List.empty[Node]
    override def toString(): String = "Node("+id+","+children+")"
  } 

  def data1(): Node = {
    val a = Node("A")
    val b = Node("B")
    val c = Node("C")
    val d = Node("D")
    val e = Node("E")
    val f = Node("F")
    val g = Node("G")
    val h = Node("H")
    val i = Node("I")
    val j = Node("J")
    val k = Node("K")
    val l = Node("L")
    a.children = List(b, c, d) 
    b.children = List(e, f)
    c.children = List(g)
    d.children = List(h)
    e.children = List(j, k)
    f.children = List(l)
    a
  }

  def data(): Node = {
    val a = Node("A")
    val b = Node("B")
    val c = Node("C")
    val d = Node("D")
    a.children = List(b, c)
    c.children = List(d) 
    a
  }

  def search(node: Node, collector: java.util.Map[String, Seq[String]]) {
    collector.get(node.id) match {
      case null => throw new RuntimeException("No path found for id "+node.id)
      case route: Seq[String] => {
        if(!node.children.isEmpty) collector.remove(node.id)
        node.children.foreach { child => 
          collector.put(child.id, (route ++ Seq(child.id)))
          search(child, collector)
        }
      }
    } 
  }

  def lca(routes: java.util.Map[String, Seq[String]], 
          targetA: String, targetB: String): String = {
    val found = routes.filter { case (k, v) => 
      v.contains(targetA) || v.contains(targetB) 
    }.values.toList
    if(1 == found.size) {
      val idxA = found(0).indexOf(targetA)
      val idxB = found(0).indexOf(targetB)
      if(idxA < idxB) targetA else targetB
    } else if(1 < found.size){
      found.foldLeft(found(0)){ (r, e) => r.intersect(e) }.last
    } else ""
  }
  
  def main(args: Array[String]) {
    val root = data1()
    val collector = new java.util.HashMap[String, Seq[String]]
    collector.put(root.id, Seq(root.id))
    search(root, collector)
    println("collector: "+collector)
    val common = lca(collector, "K", "L")
    println("least common element found: "+common)
  }
  
}
