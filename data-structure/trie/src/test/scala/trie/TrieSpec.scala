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

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TrieSpec extends FlatSpec with Matchers {

  "Trie" should "insert and find data successfully" in {
    val trie = new Trie
    trie.insert(Data(Seq(1, 2), Option(30)))
    trie.findBy(Seq(1, 2)) match {
      case Some(node) => {
        node.value should be (Option(30))
        node.valueType should be (Option(classOf[Integer]))
      }
      case None => throw new RuntimeException("Couldn't find inserted data!")
    }
  }
}
