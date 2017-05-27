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
object Map {

  def main(args: Array[String]) {
    println(map(Seq(1, 2, 3), { i => i * 2 } ))
  }

  def map(seq: Seq[Int], f: Int => Int): Seq[Int] = {
    def _map(all: Seq[Int], rest: Seq[Int]): Seq[Int] = rest match {
      case Nil => all
      case head :: tail => _map(all :+ f(head), tail)
    }
    _map(Seq.empty[Int], seq)
  }

}
