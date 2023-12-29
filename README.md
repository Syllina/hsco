# hsco

[![GitHub CI](https://github.com/Syllina/hsco/workflows/CI/badge.svg)](https://github.com/Syllina/hsco/actions)
[![Build status](https://img.shields.io/travis/Syllina/hsco.svg?logo=travis)](https://travis-ci.org/Syllina/hsco)
<!--
[![Hackage](https://img.shields.io/hackage/v/hsco.svg?logo=haskell)](https://hackage.haskell.org/package/hsco)
[![Stackage Lts](http://stackage.org/package/hsco/badge/lts)](http://stackage.org/lts/package/hsco)
[![Stackage Nightly](http://stackage.org/package/hsco/badge/nightly)](http://stackage.org/nightly/package/hsco)
-->
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

The very least comprehensive personal haskell tool

### Todo (thu)

- [ ] 获取作业信息 & 课程时间等并规划到日程中（考虑调课等）
    - [ ] 作业信息获取的 api
    - [ ] 通用的日程规划 module (expand)
- [ ] 命令行提交作业 / 下载作业文件
    - [ ] 作业提交的 api
    - [ ] 作业下载的 api
- [ ] 标记课件，下载课件，文件管理
    - [ ] 课件下载的 api
- [ ] 课程公告 / 作业反馈自动提醒
    - [ ] 课程公告与作业反馈的 api
    - [ ] 提醒（日程规划）的 module
- [ ] （可能的）作业信息分享
- [ ] 信息门户重要通知同步，日程规划
- [ ] 图书馆座位 / 研讨间预约（ / 教室状况查询）
- [ ] 考试安排同步
    - [ ] 考试安排的 api
- [ ] （可能的）选课系统

### Todo (dnd)

- [ ] 命令交互系统，roleplay 的格式化，骰子
- [ ] 公式的编码和解析（eDSL?）

### Todo (res1999)

- [ ] 角色数据存储
    - [ ] 找到数据规律
- [ ] 角色技能的描述

### Todo (general)

- [ ] 可本地同步的数据存储
- [ ] 多可用 client
    - [ ] 不同 client 对数据的渲染方式，通用（或不通用）的 c-s 数据传输格式
    - [ ] cli
        - [ ] cli 的 pretty-print
        - [ ] 菜单的选择，命令格式，自动补全
    - [ ] tui
    - [ ] gui
    - [ ] webui
- [ ] 网络通信架构
