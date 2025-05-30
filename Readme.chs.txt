﻿**********************************************************************
                     CnPack For Delphi/C++Builder
                   中国人自己的开放源码第三方开发包
                 (C)Copyright 2001-2025 CnPack 开发组
**********************************************************************

                          CnPack 内部测试代码包
                           Version 2025.05.12
                         =======================
                          CnPack 开发组 2025.05


======================================================================
1. 许可协议
======================================================================

    CnPack 开发包以开放源码 (Open Source) 的形式发布，遵守 CnPack 的许
可协议，受 CnPack 许可协议的保护。

    License 文件中有该协议的详细描述，具体内容可参考协议文件。

    请访问 CnPack 开发网站，以获得最新的更新消息：

    https://www.cnpack.org

======================================================================
2. 测试包内容
======================================================================

    该代码包仅用于开发组内部测试，并未正式发布，不推荐将其直接用于成品
软件中，如果您在使用中发现任何问题请与 CnPack 开发组联系：

    https://www.cnpack.org
    mailto:master@cnpack.org

    代码包中包含以下几个目录：
    1、Doc 开发包文档目录，里面包含了所有 CnPack 设计开发文档以及各种
开发规范文档。
    2、Packages 安装包目录，包含 Delphi 下的包安装文件。
    3、Examples 演示程序目录，包含开发人员为代码编写的演示程序和源码。
    4、Source 开发包源代码目录，所有的 CnPack 源码放于该目录下。

======================================================================
3. 安装使用
======================================================================

    开发包支持 Delphi 5/6/7/2005/2006/2007/2009/2010/XE/XE2/XE3/XE4/XE5/
XE6/XE7/XE8/10 Seattle/10.1 Berlin/10.2 Tokyo/10.3 Rio/10.4 Sydney/11
Alexandria/12 Athens 和 C++Builder 5/6。
    用户需要在 IDE 中打开 Packages 目录下的包文件（注意版本号），再在包
文件窗口中进行编译安装。其中带 dcl 前缀的为设计期包，无此前缀的为运行期
包。注意应该先编译运行期包，再安装设计期包。

    点击 Delphi 的 Tools 菜单下的 Environment Options，进入设置对话
框，点击 Library 页面中的 Library Path 项右边的按钮，将开发包 Source
目录下各个子目录分别加入到搜索路径中。

    如需要其他语言种类的组件包，可在 Source\Lang 目录下对应语言 ID 的目
录中将此语种的字符串常量定义文件和 dfm 文件覆盖 Source 各个目录下的同名
文件再重新编译，或运行 Source 目录中的 ToENU.bat 或对应语种的批处理文件
进行自动覆盖即可。

    注：该代码包仅用于开发组内部测试，并未正式发布，不推荐将其直接用于
成品软件中。

