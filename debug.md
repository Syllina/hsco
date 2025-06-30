在新机器上 pull 代码后尝试编译，由于其他项目都使用了 LTS 22.6，尝试直接用 22.6 编译无法通过.

听说 lhy 21.16 能过，怎么回事呢

报错

```
/home/vboxuser/workspace/hsco/src/Hsco/Reco/Arcanist.hs:84:35: error: [GHC-83865]
    • Couldn't match type: ArcResType arc0
                     with: ArcResType arc
      Expected: Resonance (ArcResType arc0)
        Actual: Resonance (ArcResType arc)
        NB: ‘ArcResType’ is a non-injective type family
        The type variable ‘arc0’ is ambiguous
    • In a record update at fields ‘arcInsight’, ‘arcLevel’,
      with type constructor ‘Arcanist’
      and data constructor ‘Arcanist’.
      In the second argument of ‘($)’, namely
        ‘arc {arcInsight = Insight 3, arcLevel = Level 60}’
      In a stmt of a 'do' block:
        stat360 <- getPlainStat
                     $ arc {arcInsight = Insight 3, arcLevel = Level 60}
    • Relevant bindings include
        arc :: Arcanist arc (bound at src/Hsco/Reco/Arcanist.hs:83:25)
        getResonanceStatMod :: Arcanist arc -> Maybe StatMod
          (bound at src/Hsco/Reco/Arcanist.hs:83:5)
   |
84 |         stat360 <- getPlainStat $ arc {
   |                                   ^^^^^...
```

将 `arc {...}` 改为 `(arc {...} :: Arcanist arc)` 通过了编译，原因待补充
