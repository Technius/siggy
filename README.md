# Siggy

Siggy is a naive, source file-based function search tool for Scala. It uses
[Scalameta](http://scalameta.org) to find type signatures in source files.

## Usage

`siggy <path> [query]`

Searches a given file or directory for all methods. When a query is specified,
only the methods that match the query are shown.

A query has the format `[TypeParams, ...] Type1 => Type2 => ... => ReturnType`.

Example: `siggy foo.scala "Int => String"`
```scala
import scala.util.{Failure,Success,Try}
object Foo {
  def intToString(i: Int): String = i.toString
}
```

```plain
/tmp/foo.scala:
Foo.intToString(i: Int): String
```

## License

Copyright 2017 Bryan Tan

Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License. You may obtain a copy of the
License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied. See the License for the
specific language governing permissions and limitations under the License.

