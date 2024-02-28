## System.Random

`RandomGen g` 说明 `g` 是一个合格的随机数生成器. 这里 `g` 实际上是随机数 "种子" 的类型，实际的随机数生成算法写在 `instance` 里（毕竟 `instance` 一定要绑定在一个类型上）

`StdGen` 是 `RandomGen` 的一个实例.

`Uniform a` 说明 `a` 是一个可以从整个值域被随机取值的类型，`UniformRange a` 说明 `a` 是一个可以从值域上的某个范围被随机取值的类型. 如果不需要对自己的类型进行随机的话可以不管这两个 type class.

```
uniform :: (RandomGen g, Uniform a) => g -> (a, g)
uniformR :: (RandomGen g, UniformRange a) => (a, a) -> g -> (a, g)
```

这两个函数的作用显而易见.

`Random a` 这个 type class 的存在只是为了兼容性.

```
mkStdGen :: Int -> StdGen
initStdGen :: MonadIO m => m StdGen
```

## System.Random.Stateful

```
class Monad m => StatefulGen g m where

instance (RandomGen g, MonadState g m) => StatefulGen (StateGenM g) m where
MonadIO ==> AtomicGenM
MonadIO ==> IOGenM
STM ==> TGenM
ST ==> STGenM
```

在 (state) monadic 的语境下，需要使用 `StateGenM g`（等）代替 `g` 作为随机数生成器（为什么？`StateT` 语境下这个包装实际上啥也没干，但比如在 `ST` 语境下就是一个 `STRef`）.

```
runStateGen_ :: RandomGen g => g -> (StateGenM g -> State g a) -> a
```

```
uniformM :: StatefulGen g m => g -> m a
uniformRM :: StatefulGen g m => (a, a) -> g -> m a
```
