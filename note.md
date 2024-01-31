考虑如下的数据和类型间的逻辑关系：

第一种表示法：
```haskell
data Arcanist = ThirtySeven | Toothfairy | ...
```

第二种表示法
```haskell
class Arcanist a where
    attrGen :: a -> AttributeGenerator

-- 这里忘记怎么写 type-carried info 了
instance Arcanist ThirtySeven where
    attrGen _ = fromRaw [36, 269, 408, 683, 1019, 1199]
```

第三种表示法
```haskell
newtype ArcID = Int
data Arcanist = Arcanist {
    arcID :: ArcID
}

attrGen :: ArcID -> AttributeGenerator
```

一和三似乎是等价的，给一的 data 加一个 `deriving (Enum)` 就转化为了三，给三加一个 mapping（ThirtySeven = 1 :: ArcID）就转化为了一

也就是信息是存储在 data 中（data constructor 也是 data 的一部分）还是存储在 type 中
如果存储在 type 中，怎么让 Arcanist 成为一个 kind？

类比 oop，大概就是是一个类中存储 id 来标识这是哪一种，还是类继承，在类型中表示这是哪一种

优劣势？先来看一下 hid 中的例子

type system 是为了在编译时刻发现错误设计的. 方案二是否能增加规避的错误量？
支持 aggressive refactoring（因为 refactor 中可能 introduce 的很多错误都会在编译时间规避掉）

考虑摄氏度和华氏度转换的场景.（为什么不使用两个独立的 type？）
`Maybe` 作为一个 kind，是作用在 Type 上的函数.
全部用 `*` 有什么问题？无法将 type class 引入到这个 kind system 中（现在是怎么引入的？Constraint），同时将所有类型都送到 `*` kind 没有区分度（现在是怎么区分的？）
`NoStarIsType` extension
```haskell
Fractional :: Type -> Constraint
```

为什么不能看 `Maybe a` 的类型？因为 a 没有定义，就像一个谓词，不能判断 P(x) 的真假. 打开 `ExplicitForAll` 后，可以判断 `forall a. Maybe a` 的 `kind`.

`kind` 自身也有 kind，从 `Data.Kind` 中引入后，可以看到 `Type` 和 `Constraint` 的 kind 都是 `Type`.
也就是说，Term 的 type 是 Type，type 的 kind 是 type.

没有 term 的 type 有用吗？可以用来做 phantom parameters
应用场景：想要分辨摄氏度和华氏度，但是想用同一个类型，同一种方式存储
```haskell
newtype Temp unit = Temp Double deriving (...)
data F
data C
```
那么 `Temp F` 和 `Temp C` 就是不同的类型，不能被混用，但是它们有同样的存储方式.
缺陷在于可以定义 `Temp Bool` 等无意义的类型.

有些时候这样的类型需要一个无意义的取值.
```haskell
data Proxy t = Proxy
```
可以用 proxy 来定义 `Temp unit` 的 `Show` instance.
为什么不能直接定义？
如果使用 `OverlappingInstances`，应该可以定义这样的实例
```haskell
instance Show (Temp C) where
    show (Temp t) = ...
```

我们实际上可以利用 `Proxy`，对 unit 类型定义 type class，通过 `Proxy` 值的参数来区分返回值，`ScopedTypeVariables` 允许你在函数定义中使用``外部''的类型变量.
```haskell
class UnitName u where
    unitName :: Proxy u -> String
```
这样可以很容易地引入新的单位.
```haskell
instance UnitName K where
    unitName _ = "K"
```

在 type class 定义中的类型变量 u 默认的 kind 为 Type，所以我们不能为其他 kind 的类型定义 instance. `PolyKinds` 扩展可以放松这一限制.

```haskell
read :: (Read a) => String -> a
```
`read` 实际上需要一个类型 a 作为参数，在打开 `TypeApplications` 的情况下，我们可以通过 `read @Int "42"` 来给 a 提供值.
打开这一扩展后，我们可以不使用 `Proxy` 实现 `unitName`，这个时候我们无法得知 `unitName` 的类型（虽然看起来只是返回了 `String`，但是类型 (`UnitName u`) 也是它的一个参数），只有 `unitName @F` 才是一个返回 `String` 的函数.
visible type applications 不能完全取代 proxy.

---

打开 `DataKinds` 扩展后
```haskell
data TempUnits = F | C
```
会同时定义 type/term 和 kind/type，然后就可以如下限制类型变量
```haskell
newtype Temp (u :: TempUnits) = Temp Double
```
编译器会用 `'F` 来表示 `Type`.
在 `GHC.TypeLits` 里，自然数和字符串都是类型，它们对应的 kind 是 `Nat/Symbol`. list 也可以成为 kind.
读取的方法是
```haskell
symbolVal :: forall s. KnownSymbol s => Proxy x -> String
```

---

11.3 Type Families
type 上的映射
type synonym families（不创建新的类型）
data families（可以定义新的数据类型）
associated families（定义在 type class 中）

```haskell
type family Simplify t

type instance Simplify Integer = Integer
type instance Simplify Int = Integer
```

与一般的数据类型类似的是，`Simplify Integer` 确实是一个 type，只不过类似一个 type synonym，实现时可以得到简化

```haskell
type family Widen a where
    Widen Bool = Int
    Widen Int = Integer
    Widen Char = String
    Widen t = String
```

这是 closed type family，可以有 catch-all 的 instance

为了实现实际的函数的重载，我们需要一个 type class 实现功能.

data family：同样的外部表示，不同的内部实现
```haskell
data family XList a
newtype instance XList () = XListUnit Integer
data instance XList Bool = XBits Integer Integer
```
也就是可以把 `XList ()` 看作一个新的类型
然后使用一个 type class 描述一个 `forall a. XList a` 应满足的性质
data family 可以从 data constructor 推断出对应的 type parameter，这是一个单射，这是 type synonym family 没有的特性

---

associated families
场景：将图的接口与它内部的表达方式分离
```haskell
class Graph g where
    type Vertex g
    data Edge g
    src, tgt :: Edge g -> Vertex g
```

与 data family 类似，可以用于分离接口和实现的数据类型

associated family 更常用

---

GADTs

```haskell
data DynValue a where
    S :: String -> DynValue String
    C :: Char -> DynValue Char
    B :: Bool -> DynValue Bool
```

---

首先，上面将 `Arcanist` 作为 type class 有一个明显的问题，即 `Arcanist` 不是一个类型.
一个方法是使用 `DataKinds`
首先定义一个包含所有 arcanists 的数据类型
```haskell
data Arcanist = ThirtySeven | ToothFairy | ...
```

然后使用带 type parameter (`a :: Arcanist`) 的类型来描述 arcanist.
这样的问题在于不能把不同的神秘学家用一个类型来表述
我们可以使用 GADTs 对这样的类型进行包装
```haskell
data SomeArcanist where
    SomeArcanist :: (IsArcanist a) => Arcanist a -> SomeArcanist
```

这里为了避免需要在一个文件中列出所有的神秘学家，尝试不使用 `DataKinds`，而定义一个 `class IsArcanist`，然后在对应描述神秘学家的文件里定义一个空的 data 并实现对应的实例

或许应该选择称为 `IsArcanistType`，然后 `IsArcanist` 用来描述 实例 的属性？能否将 `Arcanist` 和它的 wrapper 都声明成这个实例？

需要解决的问题是，现在有多个具有相同接口（如各种数值，咒语效果等）的不同类型，如何构建一个能够容纳它们的列表.

包括有神秘学家的列表，任何能战斗生物的列表等等.

这个问题被称为 [heterogenous collections](https://wiki.haskell.org/Heterogenous_collections)

其中提到了 [HList-ext.pdf](https://okmij.org/ftp/Haskell/HList-ext.pdf)

暂时感觉这个解法没有直接使用 existential types

---

如何描述共鸣？

神秘学家类型到对应的共鸣主模块（作为被 `DataKinds` promote 的 type）的映射可以表示为一个 associated type family
```haskell
class IsArcanist arc where
    type ArcResType arc
```

但是由 DataKinds 产生的的类型并不是 Type kind 的，而是一个新的 type. 这时候改用如下代码居然神奇地通过了编译！
```haskell
data ResonanceType = X | T | Z | U
class IsArcanist arc where
    type ArcResType arc :: ResonanceType
```
