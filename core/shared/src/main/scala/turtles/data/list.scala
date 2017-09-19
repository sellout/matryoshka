/*
 * Copyright 2014–2017 SlamData Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package turtles.data

import slamdata.Predef.{Eq => _, _}
import turtles._
import turtles.patterns._

trait ListInstances {
  implicit def listBirecursive[A]: Birecursive.Aux[List[A], ListF[A, ?]] =
    Birecursive.algebraIso[List[A], ListF[A, ?]]({
      case ConsF(h, t) => h :: t
      case NilF()      => Nil
    }, {
      case h :: t => ConsF(h, t)
      case Nil    => NilF[A, List[A]]()
    })
}

object list extends ListInstances
