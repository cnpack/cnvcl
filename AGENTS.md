# CnVCL 项目指南 (AGENTS.md)

> 本文件为 AI 编程助手提供 CnVCL 项目的完整上下文，涵盖目录结构、源码功能说明、架构设计思路等。

---

## 一、项目概述

CnVCL 是 CnPack 项目的组件库部分，包含 300+ 个纯 Pascal 实现的组件与工具单元，覆盖加密、网络、GUI、数据库、国际化等领域。支持 Delphi 5 到 Delphi 13 Florence、C++Builder 5/6/2007 以及 Free Pascal (FPC)。零外部依赖（唯一第三方文件为 `Source/ThirdParty/JsonDataObjects.pas`）。

- 官方网站：https://www.cnpack.org
- 开源协议：CnPack 开源协议（详见 `License.chs.txt` / `License.cht.txt` / `License.enu.txt`）
- 版本控制：Git（见 `.gitignore` 排除规则）

---

## 二、顶层目录结构

```
cnvcl/
├── Source/          # 全部源码（核心库）
├── Package/         # 各编译器版本的包文件（.dpk/.dproj/.lpk）
├── Example/         # 示例程序（VCL/FMX/FPC/Linux 四套）
├── Doc/             # 设计文档、开发文档、报告、模板
├── AGENTS.md        # 本文件
├── README.md        # 项目说明
├── CleanInplace.bat # 清理编译产物脚本
├── GenCrypto.bat    # 加密模块代码生成/编译脚本
├── GenMultiLang.bat # 多语言资源生成脚本
├── Delphinus.Info.json      # Delphinus 包管理器元信息
├── Delphinus.Install.json  # Delphinus 安装配置
├── License.*.txt     # 三语许可证
└── Readme.*.txt      # 三语说明
```

---

## 三、Source/ 源码目录详解

`Source/` 是整个库的核心，按功能模块分为以下子目录。每个 `.pas` 单元均通过 `{$I CnPack.inc}` 引入全局条件编译配置。

### 3.1 Common/（核心公共单元）

核心类、数学工具、数据结构、文件/字符串工具等基础功能。部分单元支持 FPC。

| 文件 | 功能说明 |
|------|----------|
| `CnPack.inc` | **全局条件编译配置文件**（约 3800 行）。检测编译器版本（`VERxxx`），定义 `DELPHIX`、`COMPILERX`、`BDSX` 等符号及累积符号（`DELPHI7_UP` 等），设置功能标志（`UNICODE_STRING`、`SUPPORT_FMX`、`SUPPORT_WIN64`、`SUPPORT_GENERIC`、`SUPPORT_INLINE` 等）。FPC 编译时自动设置 `{$mode Delphi}` 并映射 CPU 架构定义。 |
| `CnNative.pas` | 平台整数类型统一单元。定义 `TCnNativeInt`、`TCnNativeUInt` 等类型，解决不同 Delphi 版本间 `NativeInt`/`NativeUInt` 差异。提供大小端检测、字节序转换、定点时间格式转换等底层函数。被全局使用。 |
| `CnConsts.pas` | 全局常量与错误码定义单元。定义 `ECN_OK`、`ECN_FILE_NOT_FOUND`、`ECN_CUSTOM_ERROR_BASE` 等通用错误码及非本地化字符串常量。 |
| `CnClasses.pas` | CnPack 基础类层次定义单元。定义 `TCnLockObject`（线程安全锁封装）、`TCnAssignablePersistent`（通过 RTTI 自动实现 Assign）、`TCnComponent`（CnPack 组件基类，继承自 TComponent）等。 |
| `CnVCLBase.pas` | VCL 基类单元。定义 `TCnGraphicControl`（图形控件基类），支持 Alpha 透明度、父图像拷贝等。依赖 Windows API。 |
| `CnCommon.pas` | 通用函数库单元（约 9400 行）。提供大量跨平台工具函数，涵盖字符串处理、文件操作、窗口控件操作、日期时间、十六进制转换、身份证校验等。 |
| `CnFmxUtils.pas` | FMX 平台通用工具函数单元。提供 FMX 框架下的辅助函数。 |
| `CnContainers.pas` | 通用容器类单元。实现简单栈、队列等容器（尾 Push、头 Pop），支持对象指针存储。 |
| `CnHashMap.pas` | 哈希映射（HashMap）实现单元。基于哈希表的键值对存储。 |
| `CnHashTable.pas` | 哈希表（HashTable）实现单元。 |
| `CnLinkedList.pas` | 链表数据结构实现单元。 |
| `CnSkipList.pas` | 跳表（Skip List）数据结构实现单元。 |
| `CnLockFree.pas` | 无锁（Lock-Free）数据结构实现单元。提供无锁队列等并发数据结构。 |
| `CnTree.pas` | 树数据结构实现单元。提供通用树节点和树遍历功能。 |
| `CnTreeClasses.pas` | 树类型扩展单元。提供更丰富的树类型定义。 |
| `CnGraph.pas` | 图论数据结构与算法单元。实现图的表示和遍历。 |
| `CnGraphUtils.pas` | 图形工具函数单元。提供绘图相关辅助函数。 |
| `CnMath.pas` | 数学函数库单元。提供扩展数学运算函数。 |
| `CnMatrix.pas` | 矩阵运算单元。实现浮点矩阵的加减乘除、转置、求逆等运算。 |
| `CnFloat.pas` | 高精度浮点运算单元。提供扩展浮点数运算支持。 |
| `CnBigDecimal.pas` | 十进制无限精度大数实现单元。基于十进制的 `TCnBigDecimal`，底层使用 `TCnBigBinary`（二进制大数）表示有效数字，Integer 表示十进制指数。 |
| `CnBigRational.pas` | 有理数（分数）实现单元。`TCnBigRational` 基于两个 `TCnBigNumber` 表示分子和分母。 |
| `CnStrings.pas` | 字符串工具单元。提供字符串分割、匹配、编码转换等函数。 |
| `CnWideStrings.pas` | 宽字符串列表单元。兼容 Delphi 各版本的宽字符串容器。 |
| `CnStrDiff.pas` | 字符串差异比较单元。实现文本差异（Diff）算法。 |
| `CnBinaryDiffPatch.pas` | 二进制差异补丁单元。实现二进制文件的差异生成和补丁应用。 |
| `CnBloomFilter.pas` | 布隆过滤器（Bloom Filter）实现单元。 |
| `CnDancingLinks.pas` | 舞蹈链（Dancing Links）算法实现单元。用于精确覆盖问题求解。 |
| `CnRopes.pas` | Rope 字符串数据结构实现单元。适用于频繁插入/删除的超长字符串。 |
| `CnEventBus.pas` | 事件总线（Event Bus）实现单元。提供发布/订阅模式的事件通信。 |
| `CnEventHook.pas` | 事件钩子单元。实现对象事件的拦截和替换。 |
| `CnMethodHook.pas` | 方法钩子单元。通过汇编实现实例方法运行期替换。 |
| `CnIntfMethodHook.pas` | 接口方法钩子单元。实现接口方法运行期替换。 |
| `CnMulticastEvent.pas` | 多播事件单元。实现一个事件可挂载多个处理函数。 |
| `CnObjAuto.pas` | 对象自动化单元。提供运行期方法动态生成和调用。 |
| `CnDynObjBuilder.pas` | 动态对象构建器单元。支持运行期动态构建对象接口。 |
| `CnCallBack.pas` | 回调函数封装单元。提供类型安全的回调机制。 |
| `CnSingleton.pas` | 单例模式基类单元。实现线程安全的单例模式。 |
| `CnSingletonComp.pas` | 组件化单例单元。基于 TComponent 的单例实现。 |
| `CnThreadTaskMgr.pas` | 线程任务管理器单元。提供线程池任务调度。 |
| `CnDebug.pas` | 调试输出单元。提供向调试器/文件输出调试信息的函数。 |
| `CnMemProf.pas` | 内存分析单元。提供内存使用统计和泄漏检测。 |
| `CnFileUtils.pas` | 文件工具单元。提供文件/目录操作的扩展函数。 |
| `CnIni.pas` | INI 文件增强单元。提供比 TIniFile 更丰富的功能。 |
| `CnIniCfg.pas` | INI 配置管理单元。提供结构化的 INI 配置读写。 |
| `CnIniStrUtils.pas` | INI 字符串工具单元。 |
| `CnStream.pas` | 流处理增强单元。提供 TStream 的扩展操作。 |
| `CnShellUtils.pas` | Shell 工具单元。封装 Windows Shell API（仅 Windows）。 |
| `CnWinSvc.pas` | Windows 服务管理单元。封装 Windows Service API。 |
| `CnHardWareInfo.pas` | 硬件信息获取单元。读取 CPU、主板等硬件信息。 |
| `CnCalClass.pas` | 日历计算类单元。提供公历/农历转换的计算类。 |
| `CnCalendar.pas` | 日历控件单元。提供可视化日历组件。 |
| `CnGB18030.pas` | GB18030 编码转换单元。实现 GB18030 与 Unicode 互转。包含 `GB18030_Unicode_2.inc`、`GB18030_Unicode_4.inc` 映射表。 |
| `CnJSON.pas` | JSON 解析单元。提供 JSON 读写功能。 |
| `CnJsonPersistent.pas` | JSON 持久化单元。通过 RTTI 实现对象与 JSON 互转。 |
| `CnXML.pas` | XML 解析单元。提供 XML DOM/SAX 解析功能，兼容 Delphi 和 FPC。 |
| `CnXMLPersistent.pas` | XML 持久化单元。通过 RTTI 实现对象与 XML 互转。 |
| `CnMarkDown.pas` | Markdown 解析单元。提供 Markdown 文本解析和渲染。 |
| `CnPDF.pas` | PDF 生成单元。提供纯 Pascal 的 PDF 文件生成功能。 |
| `CnPE.pas` | PE 文件解析单元。解析 Windows PE/COFF 格式可执行文件。 |
| `CnZip.pas` | ZIP 压缩/解压单元。提供 ZIP 格式文件的读写。 |
| `CnRPC.pas` | RPC 调用单元。提供远程过程调用框架。 |
| `CnRTL.pas` | RTL 增强单元。提供对标准 RTL 函数的扩展和补充。 |
| `CnQuantum.pas` | 量子计算模拟单元。提供量子比特和量子门的模拟实现。 |
| `CnFitCurve.pas` | 曲线拟合单元。提供最小二乘法等曲线拟合算法。 |
| `CnAntiCheater.pas` | 反作弊单元。提供软件防篡改检测机制。 |
| `CnOTAUtils.pas` | OTA（Open Tools API）工具单元。封装 Delphi IDE 的 OTA 接口。 |
| `CnPropEditors.pas` | 属性编辑器单元。提供设计时属性编辑器。 |
| `CnPropSheetFrm.pas` | 属性页表单单元。提供组件属性页框架。 |
| `CnCompAboutFrm.pas` | 关于对话框表单单元。提供统一的"关于"对话框。 |
| `CnCompUtils.pas` | 组件工具函数单元。 |
| `CnVarList.pas` | 变量列表单元。提供 Variant 数组操作。 |
| `CnVclFmxMixed.pas` | VCL/FMX 混合编程单元。提供在同一项目中混用 VCL 和 FMX 的支持。 |
| `CnPack.pas` | CnPack 运行时包的主注册单元。 |
| `CnPackRegister.pas` | CnPack 设计时包的组件注册单元。注册 Common 模块的组件。 |
| `Unicode_Pua.inc` | Unicode 私用区映射表。 |

### 3.2 Crypto/（密码学与数论算法）

纯 Pascal 实现的密码学算法库，不依赖 VCL，可在 FPC/Linux 下独立编译。是 CnVCL 中最庞大的模块。

#### 核心基础

| 文件 | 功能说明 |
|------|----------|
| `CnNative.pas` | 平台整数类型（与 Common/ 共用，实际位于此目录）。定义 `TCnNativeInt`/`TCnNativeUInt`，解决 32/64 位及各 Delphi 版本的类型差异。 |
| `CnBigNumber.pas` | **大数运算核心单元**（约 11600 行）。实现 `TCnBigNumber` 类，支持加减乘除、模幂、GCD、素数检测（Miller-Rabin、BPSW）、平方根、比特操作等。内部使用 UInt32 或 UInt64 数组存储，支持多线程。提供固定时间窗口算法和滑动窗口算法两种模幂实现（通过 `FAST_POWERMOD` 开关切换）。还包含 `TCnBigNumberPool` 对象池。 |
| `CnPrime.pas` | **素数算法单元**（约 3560 行）。预生成 2^16 以内素数表，提供 Miller-Rabin 素性检测、BPSW 复合检测、Lucas U/V 序列计算、大数 IsPerfectPower 判断等。 |
| `CnPolynomial.pas` | **多项式运算单元**（约 17680 行）。支持 Int64 系数和 BigNumber 系数的一元/二元多项式运算，包括加减乘除、GCD、模运算、求导、求值。支持有限域上的多项式因式分解。模多项式下的无平方因式分解判别算法源自《一种新的模剩余类环上多项式无平方因式的判别法》（中国科学院研究生院学报，2009）。 |
| `CnPolynomialNew.pas` | 多项式运算的新版本实验单元。 |
| `CnComplex.pas` | 复数运算单元。实现复数的加减乘除、幂运算、指数、对数等。 |
| `CnDFT.pas` | 离散傅里叶变换（DFT）单元。实现 DFT/IDFT 及 FFT 快速算法，支持复数序列变换。 |
| `CnVector.pas` | 向量运算单元。提供 Float 和 BigNumber 的向量运算。 |
| `CnInt128.pas` | 128 位整数运算单元。在不原生支持 Int128 的编译器上提供 128 位整数运算。 |
| `CnBits.pas` | 位操作工具单元。提供位级读写和位域操作。 |

#### 对称加密

| 文件 | 功能说明 |
|------|----------|
| `CnAES.pas` | AES 对称加密单元。支持 AES-128/192/256 的 ECB/CBC/CTR/GCM 等模式。 |
| `CnDES.pas` | DES/3DES 对称加密单元。支持 DES 和 Triple DES 的 ECB/CBC 模式。 |
| `CnSM4.pas` | SM4 国密对称加密单元。实现 GB/T 32907-2016 规定的 SM4 分组密码算法。 |
| `CnRC4.pas` | RC4 流密码单元。 |
| `CnChaCha20.pas` | ChaCha20 流密码单元。实现 RFC 8439 规定的 ChaCha20-Poly1305。 |
| `CnTEA.pas` | TEA/XTEA/XXTEA 加密单元。 |
| `CnZUC.pas` | ZUC 祖冲之序列密码单元。实现 GB/T 33133-2016 规定的 ZUC 算法。 |
| `CnBCH.pas` | BCH 纠错编码单元。实现 Bose-Chaudhuri-Hocquenghem 循环码编解码。 |
| `CnFEC.pas` | 前向纠删码（FEC）单元。实现 Reed-Solomon 纠删码。 |

#### 哈希与消息摘要

| 文件 | 功能说明 |
|------|----------|
| `CnMD5.pas` | MD5 消息摘要单元。 |
| `CnSHA1.pas` | SHA-1 消息摘要单元。 |
| `CnSHA2.pas` | SHA-2 消息摘要单元。支持 SHA-224/256/384/512。 |
| `CnSHA3.pas` | SHA-3/Keccak 消息摘要单元。支持 SHA-3-224/256/384/512 及 SHAKE。 |
| `CnSM3.pas` | SM3 国密消息摘要单元。实现 GB/T 32905-2016 规定的 SM3 算法。 |
| `CnBLAKE.pas` | BLAKE 哈希算法单元。 |
| `CnBLAKE2.pas` | BLAKE2 哈希算法单元。支持 BLAKE2b/BLAKE2s。 |
| `CnBLAKE3.pas` | BLAKE3 哈希算法单元。 |
| `CnCRC32.pas` | CRC32 校验单元。 |
| `CnFNV.pas` | FNV（Fowler-Noll-Vo）非加密哈希单元。 |
| `CnXXH.pas` | xxHash 非加密快速哈希单元。 |

#### 公钥加密与数字签名

| 文件 | 功能说明 |
|------|----------|
| `CnRSA.pas` | **RSA 算法单元**（约 3900 行）。实现大数范围的 RSA 公私钥生成、加解密、签名验签。支持 PKCS1/OAEP 填充、CRT 加速解密（含 CRT 故障攻击防护）。支持 PEM 格式密钥文件读写（PKCS1/PKCS8）。也提供 Int64 范围的 RSA 简化实现。 |
| `CnDSA.pas` | DSA 数字签名算法单元。 |
| `CnECC.pas` | **椭圆曲线密码学核心单元**（约 10460 行）。实现 Weierstrass 形式椭圆曲线 `y^2 = x^3 + Ax + B mod p` 上的点运算、加解密、签名验签。支持 Int64 和大数范围。实现 Schoof 点计数算法。支持 PEM 格式密钥文件读写、ASN.1 编解码。 |
| `CnSM2.pas` | **SM2 国密椭圆曲线密码单元**。实现 GM/T 0003.x-2012 规定的 SM2 公钥加密算法，包括数据加解密、签名验签、密钥交换、协同密钥生成、协同签名、协同加密等。继承自 `TCnEcc`。 |
| `CnSM9.pas` | SM9 国密标识密码单元。实现 GB/T 32918 规定的 SM9 基于标识的加密和签名算法。 |
| `Cn25519.pas` | **Curve25519/Ed25519 单元**（约 6400 行）。实现 Curve25519 椭圆曲线 Diffie-Hellman 密钥交换和 Ed25519 EdDSA 签名。包含 X25519、Ed25519、Ristretto255、Ed448、X448 等变体。 |
| `CnPaillier.pas` | Paillier 同态加密单元。实现 Paillier 概率公钥加密系统，支持加法同态运算。 |
| `CnSecretSharing.pas` | 秘密共享（Shamir's Secret Sharing）单元。实现基于拉格朗日插值的秘密分割与重构。 |

#### 密钥派生与认证

| 文件 | 功能说明 |
|------|----------|
| `CnKDF.pas` | 密钥派生函数（KDF）单元。提供 PBKDF2、HKDF、SM2 KDF 等密钥派生算法。 |
| `CnAEAD.pas` | AEAD（认证加密）单元。提供 AEAD 封装接口。 |
| `CnPoly1305.pas` | Poly1305 消息认证码（MAC）单元。 |
| `CnOTP.pas` | 一次性密码（OTP/HOTP/TOTP）单元。实现 RFC 4226 HOTP 和 RFC 6238 TOTP。 |
| `CnOTS.pas` | 一次性签名（OTS）单元。实现 Lamport/Winternitz 一次性签名方案。 |

#### 后量子密码

| 文件 | 功能说明 |
|------|----------|
| `CnMLKEM.pas` | **ML-KEM（Module-Lattice-Based KEM）单元**。实现 FIPS 203 规定的 ML-KEM（原 Kyber）后量子密钥封装机制，支持 ML-KEM-512/768/1024。 |
| `CnMLDSA.pas` | **ML-DSA（Module-Lattice-Based DSA）单元**。实现 FIPS 204 规定的 ML-DSA（原 Dilithium）后量子数字签名算法。 |
| `CnSLHDSA.pas` | **SLH-DSA（Stateless Hash-Based DSA）单元**。实现 FIPS 205 规定的 SLH-DSA（原 SPHINCS+）无状态哈希后量子签名算法。 |
| `CnLattice.pas` | 格密码学基础单元。提供格运算和多项式环运算的基础实现。 |

#### 椭圆曲线点计数（SEA 算法）

| 文件 | 功能说明 |
|------|----------|
| `CnSEA.pas` | **Schoof-Elkies-Atkin 算法单元**（约 3660 行）。实现 SEA 椭圆曲线阶计算算法，用于计算有限域上椭圆曲线 #E(F_p)。分两阶段：Elkies 阶段（计算 Frobenius 迹的特征多项式）和 Atkin 阶段（通过 Atkin 素数信息组合求解）。支持模多项式文件加载。 |

#### 编码与格式

| 文件 | 功能说明 |
|------|----------|
| `CnBase64.pas` | Base64 编解码单元。 |
| `CnBerUtils.pas` | **BER/DER 编解码单元**（约 1700 行）。实现 ASN.1 BER/DER 格式的读写解析，用于 X.509 证书、PKCS 密钥等结构化数据的编解码。 |
| `CnPemUtils.pas` | PEM 格式工具单元。实现 PEM 格式文件的加载和保存，支持加密的 PEM。 |
| `CnCertificateAuthority.pas` | **CA 证书管理单元**。实现 X.509 证书的生成、签名、验证，支持自签名 CA 和终端证书。 |
| `CnCryptoExport.pas` | 加密算法导出单元。将加密算法封装为可供外部调用的接口。 |
| `CnCryptoIntf.pas` | **加密算法 DLL/SO 接口单元**（约 1740 行）。将 CnPack 加密算法封装为可编译为 DLL/SO/DYLIB 的接口，支持静态加载和运行时调用。同时提供 C 头文件 `CnCryptoIntf.h`。 |
| `CnPDFCrypt.pas` | PDF 加密单元。实现 PDF 文件的密码保护加密。 |
| `CnRandom.pas` | 安全随机数生成单元。提供密码学安全的随机数生成器。 |
| `CnQRCode.pas` | **二维码生成单元**（约 4825 行）。实现 QR Code Model 2 的编码，支持所有版本（1-40）和纠错级别，支持数字/字母/字节/汉字模式，自动选择最优编码模式。 |

### 3.3 Graphic/（VCL 图形 UI 控件）

VCL 可视化控件和图形工具，依赖 Windows API 和 VCL 框架。不支持 FPC。

| 文件 | 功能说明 |
|------|----------|
| `CnAAFont.pas` | 平滑字体（Anti-Aliased Font）单元。提供文字抗锯齿渲染。 |
| `CnAAFontDialog.pas` | 平滑字体设置对话框单元。 |
| `CnAAFontEditor.pas` | 平滑字体属性编辑器单元。 |
| `CnAACtrls.pas` | 平滑字体控件单元。提供使用抗锯齿文字的标签等控件。 |
| `CnAOTreeView.pas` | Always-On-Top TreeView 控件单元。 |
| `CnAutoOption.pas` | 自动选项配置组件单元。提供 INI/注册表自动持久化的选项组件。 |
| `CnButtonEdit.pas` | 按钮编辑框控件单元。带嵌入按钮的编辑框。 |
| `CnButtons.pas` | 按钮控件单元。提供增强按钮组件。 |
| `CnCheckTreeView.pas` | 带复选框的 TreeView 控件单元。 |
| `CnColorGrid.pas` | 颜色网格控件单元。提供调色板式颜色选择。 |
| `CnColorSpace.pas` | 颜色空间转换单元。实现 RGB/HSV/HSL/CMYK 等颜色空间互转。 |
| `CnEdit.pas` | 增强编辑框控件单元。 |
| `CnErrorProvider.pas` | 错误提示控件单元。类似 .NET ErrorProvider 的验证提示控件。 |
| `CnGIF.pas` | GIF 图像处理单元。支持 GIF 动画加载和播放。 |
| `CnGauge.pas` | 进度条控件单元。 |
| `CnGraphics.pas` | 图形绘制工具单元。提供增强的画布绘图函数。 |
| `CnGraphConsts.pas` | 图形模块常量单元。 |
| `CnGraphPropEditors.pas` | 图形模块属性编辑器单元。 |
| `CnGraphRegister.pas` | 图形模块组件注册单元。 |
| `CnHexEditor.pas` | 十六进制编辑器控件单元。提供文件/数据的十六进制查看和编辑。 |
| `CnHint.pas` | 增强提示窗口单元。支持自定义提示外观。 |
| `CnIconUtils.pas` | 图标工具单元。提供图标提取和转换。 |
| `CnImage.pas` | 图像显示控件单元。支持多种图像格式显示。 |
| `CnJPEG.pas` | JPEG 图像处理单元。扩展 JPEG 加载和保存功能。 |
| `CnLED.pas` | LED 显示控件单元。提供模拟 LED 灯显示。 |
| `CnListBox.pas` | 增强列表框控件单元。 |
| `CnMandelbrotImage.pas` | Mandelbrot 分形图像控件单元。 |
| `CnMemo.pas` | 增强多行编辑框控件单元。 |
| `CnMonthCalendar.pas` | 月历控件单元。 |
| `CnOpenGLPaintBox.pas` | OpenGL 绘图画布控件单元。 |
| `CnPanel.pas` | 增强面板控件单元。 |
| `CnQRImage.pas` | 二维码图像控件单元。在 UI 上显示二维码。 |
| `CnRichEdit.pas` | 富文本编辑控件单元。 |
| `CnSVG.pas` | SVG 图像解析与渲染单元。支持 SVG 文件的加载和绘制。 |
| `CnSearchCombo.pas` | 搜索下拉框控件单元。带搜索功能的组合框。 |
| `CnShellCtrls.pas` | Shell 控件单元。提供文件系统浏览的可视化控件。 |
| `CnSkinMagic.pas` | 皮肤魔法控件单元。 |
| `CnSpin.pas` | 数字微调控件单元。 |
| `CnSplitter.pas` | 分割条控件单元。 |
| `CnTabSet.pas` | 标签页集合控件单元。 |
| `CnTextControl.pas` | 文本控件基类单元。 |
| `CnValidateImage.pas` | 验证码图像控件单元。 |
| `CnVirtualControl.pas` | 虚拟控件单元。提供自绘的轻量控件框架。 |
| `CnWaterEffect.pas` | 水波纹特效单元。实现图像水波纹动画效果。 |
| `CnWaterImage.pas` | 水波纹图像控件单元。 |
| `CnWatermark.pas` | **数字水印单元**。实现基于 DFT/DCT 域的数字水印嵌入和提取，支持文本和图像水印。 |
| `CnWizardImage.pas` | 向导图像控件单元。 |

### 3.4 NonVisual/（不可视组件与系统工具）

钩子、停靠框架、系统工具、线程池等不可视组件。依赖 Windows API。

#### 停靠框架（Dock Framework）

| 文件 | 功能说明 |
|------|----------|
| `CnDockGlobal.pas` | 停靠框架全局定义单元。定义停靠框架的全局变量、常量和类型。 |
| `CnDockSupportClass.pas` | 停靠支持类单元。定义停靠框架的核心类。 |
| `CnDockSupportControl.pas` | 停靠支持控件单元。提供停靠区域控件。 |
| `CnDockSupportProc.pas` | 停靠支持过程单元。提供停靠框架的工具函数。 |
| `CnDockHashTable.pas` | 停靠哈希表单元。停靠框架内部的哈希表实现。 |
| `CnDockTree.pas` | 停靠树单元。管理停靠布局的树结构。 |
| `CnDockInfo.pas` | 停靠信息单元。存储和恢复停靠布局信息。 |
| `CnDockFormControl.pas` | 停靠表单控件单元。 |
| `CnDockPropertyReg.pas` | 停靠属性注册单元。注册停靠组件的属性编辑器。 |
| `CnConjoinDockHost.pas` | 联合停靠宿主窗口单元。 |
| `CnTabDockHost.pas` | 标签页停靠宿主窗口单元。 |
| `CnDelphiDockStyle.pas` | Delphi 风格停靠单元。 |
| `CnVCDockStyle.pas` | VC 风格停靠单元。 |
| `CnVIDDockStyle.pas` | Visual InterDev 风格停靠单元。 |
| `CnVSNETDockStyle.pas` | VS.NET 风格停靠单元。 |

#### 钩子与拦截（Hook）

| 文件 | 功能说明 |
|------|----------|
| `CnControlHook.pas` | 控件钩子单元。拦截控件消息。 |
| `CnGlobalKeyHook.pas` | 全局键盘钩子单元。实现系统级键盘按键拦截。 |
| `CnMenuHook.pas` | 菜单钩子单元。拦截菜单消息。 |
| `CnActionListHook.pas` | ActionList 钩子单元。拦截 Action 事件。 |
| `CnInProcessAPIHook.pas` | 进程内 API 钩子单元。实现同进程内 Windows API 调用拦截。 |
| `CnHardwareBreakpoint.pas` | 硬件断点单元。利用 CPU 硬件断点实现代码拦截。 |
| `CnKeyBlocker.pas` | 按键屏蔽单元。屏蔽指定键盘按键。 |

#### 系统工具

| 文件 | 功能说明 |
|------|----------|
| `CnActiveScript.pas` | ActiveX 脚本引擎单元。封装 Windows Script Host，支持 VBScript/JScript。 |
| `CnASCommon.pas` | ActiveScript 公共单元。 |
| `CnASHostServices.pas` | ActiveScript 宿主服务单元。 |
| `CnASIDispatchProxy.pas` | ActiveScript IDispatch 代理单元。 |
| `CnASInvoker.pas` | ActiveScript 调用单元。 |
| `CnASPropEditors.pas` | ActiveScript 属性编辑器单元。 |
| `CnConsole.pas` | 控制台操作单元。提供控制台输入输出封装。 |
| `CnStdio.pas` | 标准 I/O 单元。提供 C 风格 stdin/stdout 操作。 |
| `CnDragResizer.pas` | 拖拽缩放器单元。提供控件运行期拖拽调整大小。 |
| `CnFilePacker.pas` | 文件打包器单元。将多个文件打包为单个文件。 |
| `CnFileSystemWatcher.pas` | 文件系统监视器单元。监视文件/目录变化。 |
| `CnFormScaler.pas` | 表单缩放器单元。按 DPI 缩放表单。 |
| `CnMDIBackGround.pas` | MDI 背景单元。为 MDI 子窗口区域设置背景图。 |
| `CnMemorySearch.pas` | 内存搜索单元。在进程内存中搜索指定字节模式。 |
| `CnObjectPool.pas` | 对象池单元。提供对象复用池，减少频繁创建/销毁开销。 |
| `CnOuterControls.pas` | 外部控件单元。实现控件跨窗口显示。 |
| `CnRawInput.pas` | Raw Input 单元。封装 Windows Raw Input API。 |
| `CnRestoreSystemMenu.pas` | 系统菜单恢复单元。 |
| `CnSystemDebugControl.pas` | 系统调试控制单元。提供底层调试权限控制。 |
| `CnTaskBar.pas` | 任务栏单元。在 Windows 任务栏显示自定义按钮。 |
| `CnThreadPool.pas` | **线程池单元**。提供可配置的线程池组件，支持任务队列、最小/最大线程数、超时等。 |
| `CnTimer.pas` | 增强定时器单元。提供高精度定时器和定时器池。 |
| `CnTrayIcon.pas` | 系统托盘图标单元。在系统通知区域显示图标。 |
| `CnVolumeCtrl.pas` | 音量控制单元。控制 Windows 音量。 |
| `CnWinampCtrl.pas` | Winamp 控制单元。通过 API 控制 Winamp 播放器。 |
| `CnADOConPool.pas` | ADO 连接池单元。提供 ADO 数据库连接的连接池。 |
| `CnCompConsts.pas` | 不可视组件常量单元。 |
| `CnCompRegister.pas` | 不可视组件注册单元。 |

### 3.5 NetComm/（网络通信）

Socket、TCP/UDP、DNS、TLS、Redis 等网络通信组件。部分支持 FPC 跨平台。

| 文件 | 功能说明 |
|------|----------|
| `CnSocket.pas` | **跨平台 Socket 封装单元**。支持 Delphi 和 FPC，覆盖 Windows/Mac/Linux 平台。提供 TCP/UDP Socket 的统一封装。 |
| `CnTCPClient.pas` | TCP 客户端单元。基于 CnSocket 实现 TCP 客户端组件。 |
| `CnThreadingTCPServer.pas` | 多线程 TCP 服务器单元。每个连接使用独立线程处理。 |
| `CnTCPForwarder.pas` | TCP 转发器单元。实现 TCP 端口转发/代理。 |
| `CnUDP.pas` | UDP 通信单元。提供 UDP 收发组件。 |
| `CnDNS.pas` | DNS 解析单元。实现 DNS 协议的查询和响应解析。 |
| `CnMulticastDNS.pas` | 多播 DNS（mDNS）单元。实现 RFC 6762 mDNS 协议。 |
| `CnNetwork.pas` | 网络工具单元。提供本机 IP、MAC 地址获取等网络信息函数。 |
| `CnIP.pas` | IP 地址工具单元。提供 IP 地址解析和验证。 |
| `CnInetUtils.pas` | Internet 工具单元。提供 URL 解析、HTTP 下载等。 |
| `CnPing.pas` | Ping 组件单元。实现 ICMP Echo 请求。 |
| `CnIocpSimpleMemPool.pas` | IOCP 简单内存池单元。为 IOCP Socket 提供内存池。 |
| `CnIocpSocketAdapter.pas` | IOCP Socket 适配器单元。基于 IOCP 的高性能 Socket 适配器。 |
| `CnTLS.pas` | TLS 单元。实现 TLS/SSL 协议封装。 |
| `CnRedisClient.pas` | Redis 客户端单元。实现 Redis 协议（RESP）的客户端。 |
| `CnRS232.pas` | 串口通信单元。封装 Windows 串口 API。 |
| `CnRS232Dialog.pas` | 串口设置对话框单元。 |
| `CnDialUp.pas` | 拨号网络单元。实现 RAS 拨号连接。 |
| `CnModem.pas` | 调制解调器控制单元。通过 AT 命令控制 Modem。 |
| `CnIISCtrl.pas` | IIS 控制单元。通过 API 控制 IIS 服务。 |
| `CnCameraEye.pas` | 摄像头单元。封装视频采集 API。 |
| `CnTwain.pas` | TWAIN 图像采集单元。封装 TWAIN 协议，支持扫描仪/摄像头。 |
| `CnNetConsts.pas` | 网络模块常量单元。 |
| `CnNetPropEditor.pas` | 网络模块属性编辑器单元。 |
| `CnNetRegister.pas` | 网络模块组件注册单元。 |

### 3.6 DbReport/（数据库工具）

数据库工具、ADO 扩展、DHibernate ORM 等。依赖 ADO/VCL。

| 文件 | 功能说明 |
|------|----------|
| `CnADOBinding.pas` | ADO 数据绑定单元。实现 ADO Recordset 与控件的绑定。 |
| `CnADOUpdateSQL.pas` | ADO 更新 SQL 单元。提供自定义 ADO 更新语句的组件。 |
| `CnADOUpdateSQLEditor.pas` | ADO 更新 SQL 编辑器单元。 |
| `CnADOUpdateSQLFrm.pas` | ADO 更新 SQL 编辑表单单元。 |
| `CnDataGrid.pas` | 数据网格控件单元。增强的数据网格组件。 |
| `CnPagedGrid.pas` | 分页网格控件单元。支持分页显示的数据网格。 |
| `CnSQLAnalyzer.pas` | SQL 分析器单元。提供 SQL 语法分析和高亮。 |
| `CnRunSqlFrame.pas` | SQL 执行框架单元。 |
| `CnRunSqlUnit.pas` | SQL 执行单元。 |
| `CnExcelUnit.pas` | Excel 导出单元。提供数据导出为 Excel 文件功能。 |
| `CnXlsWriter.pas` | XLS 写入器单元。纯 Pascal 实现 XLS 格式文件写入。 |

#### DHibernate ORM

| 文件 | 功能说明 |
|------|----------|
| `CnDHibernateBase.pas` | DHibernate 基类单元。 |
| `CnDHibernateClasses.pas` | DHibernate 核心类单元。 |
| `CnDHibernateUtils.pas` | DHibernate 工具函数单元。 |
| `CnDHibernateAppUtils.pas` | DHibernate 应用工具单元。 |
| `CnDHibernateArrayList.pas` | DHibernate 数组列表单元。 |
| `CnDHibernateSet.pas` | DHibernate 集合单元。 |
| `CnDHibernatePodoList.pas` | DHibernate PODO（Plain Old Delphi Object）列表单元。 |
| `CnDHibernateMemData.pas` | DHibernate 内存数据集单元。 |
| `CnDHibernateNavigator.pas` | DHibernate 导航器单元。 |
| `CnDHibernateQueryAdv.pas` | DHibernate 高级查询单元。 |
| `CnDHibernateSubQuery.pas` | DHibernate 子查询单元。 |
| `CnDHibernateSubQueryAdv.pas` | DHibernate 高级子查询单元。 |
| `CnDHibernateBatchSQL.pas` | DHibernate 批量 SQL 单元。 |
| `CnDHibernateSQLThread.pas` | DHibernate SQL 线程单元。 |
| `CnDHibernateThread.pas` | DHibernate 线程单元。 |
| `CnDHibernateCalc.pas` | DHibernate 计算单元。 |
| `CnDHibernateDateUtils.pas` | DHibernate 日期工具单元。 |
| `CnDHibernateStringUtils.pas` | DHibernate 字符串工具单元。 |
| `CnDHibernateExport.pas` | DHibernate 数据导出单元。 |
| `CnDHibernateImport.pas` | DHibernate 数据导入单元。 |
| `CnDHibernateBackupRestore.pas` | DHibernate 备份恢复单元。 |
| `CnDHibernateConsts.pas` | DHibernate 常量单元。 |
| `CnDHibernateAbout.pas` | DHibernate 关于对话框单元。 |
| `CnDBConsts.pas` | 数据库模块常量单元。 |
| `CnDBRegister.pas` | 数据库模块组件注册单元。 |

### 3.7 MultiLang/（国际化/多语言）

多语言/国际化支持框架。部分支持 FPC。

| 文件 | 功能说明 |
|------|----------|
| `CnLangMgr.pas` | **多语言管理器单元**。核心类 `TCnLanguageManager`，负责语言切换、翻译调度，支持窗体/帧/组件的自动翻译。 |
| `CnLangStorage.pas` | 语言存储基类单元。定义 `TCnCustomLangStorage` 抽象基类。 |
| `CnHashLangStorage.pas` | 哈希语言存储单元。基于哈希表的语言项存储。 |
| `CnIniLangFileStorage.pas` | INI 语言文件存储单元。基于 INI 文件的语言项存储。 |
| `CnHashIniFile.pas` | 哈希 INI 文件单元。结合哈希表加速的 INI 文件读写。 |
| `CnLangCollection.pas` | 语言集合单元。定义语言项集合类。 |
| `CnLangTranslator.pas` | 语言翻译器单元。实现实际翻译逻辑。 |
| `CnLangUtils.pas` | 多语言工具函数单元。提供语言切换的便捷函数。 |
| `CnLangConsts.pas` | 多语言模块常量单元。 |
| `CnLangEditors.pas` | 多语言属性编辑器单元。 |
| `CnLangReg.pas` | 多语言模块组件注册单元。 |
| `CnTransEditor.pas` | 翻译编辑器表单单元。提供可视化翻译编辑界面。 |
| `CnTransFilter.pas` | 翻译过滤器表单单元。提供翻译项筛选功能。 |
| `QLangIDs.inc` | 语言 ID 常量包含文件。 |

### 3.8 Skin/（皮肤引擎）

VCL 窗体皮肤引擎。依赖 Windows API。

| 文件 | 功能说明 |
|------|----------|
| `CnSkinTheme.pas` | 皮肤主题管理单元。定义 `TCnSkinTheme` 核心类，管理皮肤主题的加载和应用。 |
| `CnSkinStyle.pas` | 皮肤样式基类单元。定义皮肤样式的抽象基类。 |
| `CnSkinForm.pas` | 窗体皮肤单元。实现窗体的皮肤渲染。 |
| `CnSkinStdCtrls.pas` | 标准控件皮肤单元。实现标准控件的皮肤渲染。 |
| `CnSkinMenu.pas` | 菜单皮肤单元。实现菜单的皮肤渲染。 |
| `CnSkinXPBlueStyle.pas` | XP 蓝色皮肤样式单元。 |
| `CnSkinXPGreenStyle.pas` | XP 绿色皮肤样式单元。 |
| `CnSkinXPSilverStyle.pas` | XP 银色皮肤样式单元。 |

### 3.9 ObjRep/（对象仓库组件）

| 文件 | 功能说明 |
|------|----------|
| `CnFoxmailMsgFrm.pas` | Foxmail 风格消息框单元。提供类似 Foxmail 风格的消息提示窗口。 |
| `CnProgressFrm.pas` | 进度对话框单元。提供带进度条的模式对话框。 |

### 3.10 ThirdParty/（第三方文件）

| 文件 | 功能说明 |
|------|----------|
| `JsonDataObjects.pas` | 第三方 JSON 库。CnVCL 唯一的外部依赖文件。 |

### 3.11 Lang/（多语言资源文件）

按 LCID（区域设置标识符）组织的多语言资源文件目录：

| 子目录 | LCID | 语言 |
|--------|------|------|
| `1028/` | 1028 | 繁体中文 |
| `1033/` | 1033 | 英文 |
| `2052/` | 2052 | 简体中文 |

每个目录下包含各模块的本地化资源文件（`CnConsts.pas`、`CnCompConsts.pas`、`CnDBConsts.pas`、`CnGraphConsts.pas`、`CnLangConsts.pas`、`CnNetConsts.pas` 以及对应的 `.dfm` 窗体文件）。

通过根目录的 `ToCHS.bat`（切换简体）、`ToCHT.bat`（切换繁体）、`ToENU.bat`（切换英文）脚本实现语言资源文件切换。

---

## 四、Package/ 包文件目录

按编译器版本组织的包文件。每个目录包含运行时包（`CnPack_D*.dpk`，无前缀）和设计时包（`dclCnPack_D*.dpk`，`dcl` 前缀）。

### 4.1 Delphi 包

| 目录 | 编译器版本 | 说明 |
|------|-----------|------|
| `Delphi5/` | Delphi 5 | 最老支持版本 |
| `Delphi6/` | Delphi 6 | |
| `Delphi7/` | Delphi 7 | |
| `Delphi2005/` | Delphi 2005 | |
| `Delphi2006/` | Delphi 2006 | |
| `Delphi2007/` | Delphi 2007 | |
| `Delphi2009/` | Delphi 2009 | 引入 UnicodeString |
| `Delphi2010/` | Delphi 2010 | |
| `DelphiXE/` | Delphi XE | |
| `DelphiXE2/` | Delphi XE2 | 引入 64 位支持 |
| `DelphiXE3/` | Delphi XE3 | |
| `DelphiXE4/` | Delphi XE4 | |
| `DelphiXE5/` | Delphi XE5 | |
| `DelphiXE6/` | Delphi XE6 | |
| `DelphiXE7/` | Delphi XE7 | |
| `DelphiXE8/` | Delphi XE8 | |
| `Delphi10S/` | Delphi 10 Seattle | |
| `Delphi101B/` | Delphi 10.1 Berlin | |
| `Delphi102T/` | Delphi 10.2 Tokyo | |
| `Delphi103R/` | Delphi 10.3 Rio | |
| `Delphi104S/` | Delphi 10.4 Sydney | |
| `Delphi110A/` | Delphi 11 Alexandria | |
| `Delphi120A/` | Delphi 12 Athens | 含 32 位和 64 位包 |
| `Delphi130F/` | Delphi 13 Florence | 含 32 位和 64 位包 |

### 4.2 C++Builder 包

| 目录 | 编译器版本 |
|------|-----------|
| `BCB5/` | C++Builder 5 |
| `BCB6/` | C++Builder 6 |
| `BCB2007/` | C++Builder 2007 |

### 4.3 FPC 包

| 目录 | 说明 |
|------|------|
| `FPC3/` | Free Pascal 3.x 的 Lazarus 包（`.lpk`）。运行时包 `CnPack_FPC3.lpk`，设计时包 `dclCnPack_FPC3.lpk`。 |

> **注意**：修改包文件时，`.dpk` 和对应的 `.dproj` 必须同步修改（`.dproj` 中包含源码路径）。

---

## 五、Example/ 示例目录

按平台分类的示例程序，每个子目录对应一个功能组件的示例：

| 子目录 | 平台 | 说明 |
|--------|------|------|
| `VCL/` | Windows VCL | 最完整的示例集（160+ 个），涵盖所有模块 |
| `FMX/` | FireMonkey | 跨平台 FMX 示例，主要覆盖 Common/Crypto 模块 |
| `FPC/` | Free Pascal | FPC 命令行示例，覆盖 Common/Crypto/NetComm 模块 |
| `Linux/` | Linux | Linux 平台示例，包含 Cnt 终端和 Crypto 示例 |

批量构建脚本：
- `Example/VCL/BuildVclExamples.bat` — 使用 dcc32 编译所有 VCL 示例
- `Example/FPC/BuildFpcExamples.bat` — 使用 FPC 编译所有 FPC 示例
- `Example/FMX/BuildFmxExamples.bat` — 使用 dcc32 编译所有 FMX 示例

---

## 六、Doc/ 文档目录

| 子目录 | 说明 |
|--------|------|
| `Design/` | 架构设计文档，按模块分子目录（Common、Debug、MultiLang、NonVisual、Skin） |
| `Develop/` | 开发文档，包含各组件的使用说明、算法说明、代码注释规则等 |
| `Project/` | 项目管理文档 |
| `Report/` | 测试报告 |
| `Template/` | 代码模板 |

---

## 七、架构设计

### 7.1 条件编译系统

所有 `.pas` 单元通过 `{$I CnPack.inc}` 引入全局条件编译配置（约 3800 行）。该文件完成三件事：

1. **编译器版本检测**：通过 `{$IFDEF VERxxx}` 定义 `DELPHIX`、`COMPILERX`、`BDSX` 等符号
2. **累积版本符号**：定义 `DELPHI7_UP`、`COMPILER12_UP` 等 `_UP` 后缀符号，用于条件编译（如 `{$IFDEF DELPHI7_UP}` 表示 Delphi 7 及以上）
3. **功能标志**：根据版本定义 `UNICODE_STRING`、`SUPPORT_FMX`、`SUPPORT_WIN64`、`SUPPORT_GENERIC`、`SUPPORT_INLINE` 等

FPC 编译时自动设置 `{$mode Delphi}` 并映射 CPU 架构定义（`CPUX86`、`CPUX64` 等）。

### 7.2 基础类层次

```
TCnLockObject              — 线程安全锁封装（CnClasses.pas）
TCnAssignablePersistent    — 通过 RTTI 自动实现 Assign（CnClasses.pas）
TCnComponent               — CnPack 组件基类（继承自 TComponent，CnClasses.pas）
TCnGraphicControl          — 图形控件基类（CnVCLBase.pas）
```

### 7.3 平台整数类型

`CnNative.pas`（位于 `Source/Crypto/`）统一 32/64 位平台的整数类型（`TCnNativeInt`、`TCnNativeUInt`），解决不同 Delphi 版本间 `NativeInt`/`NativeUInt` 差异。虽然位于 Crypto 目录，但被全局使用。

### 7.4 模块依赖关系

```
CnPack.inc → CnNative.pas → CnConsts.pas → CnClasses.pas → CnCommon.pas
                                                    |
              +--------+--------+--------+--------+---------+--------+
              |        |        |        |        |         |        |
           Crypto  Graphic  NonVisual  NetComm  MultiLang  DbReport  Skin
           (纯算法) (VCL控件) (钩子)   (网络)   (国际化)   (数据库)  (皮肤)
```

- **Crypto** 不依赖 VCL，可在 FPC/Linux 下独立编译
- **Graphic**、**NonVisual**、**Skin** 依赖 Windows API
- **设计时包** 依赖运行时包和 `designide`

### 7.5 组件注册

每个模块有独立的注册单元（仅在设计时包中使用）：

| 注册单元 | 模块 |
|----------|------|
| `CnPackRegister.pas` | Common 模块 |
| `CnGraphRegister.pas` | Graphic 模块 |
| `CnCompRegister.pas` | NonVisual 模块 |
| `CnNetRegister.pas` | NetComm 模块 |
| `CnDBRegister.pas` | DbReport 模块 |
| `CnLangReg.pas` | MultiLang 模块 |

### 7.6 密码学模块架构

Crypto 模块是 CnVCL 中最核心、最庞大的部分，采用分层设计：

```
基础层：
  CnNative.pas     — 平台类型
  CnBigNumber.pas  — 大数运算（核心基础）
  CnPrime.pas      — 素数检测
  CnPolynomial.pas — 多项式运算
  CnComplex.pas    — 复数运算
  CnDFT.pas        — 傅里叶变换

算法层：
  对称加密：AES / DES / SM4 / RC4 / ChaCha20 / TEA / ZUC
  哈希摘要：MD5 / SHA-1/2/3 / SM3 / BLAKE/2/3 / CRC32 / FNV / xxHash
  公钥密码：RSA / ECC / SM2 / SM9 / Ed25519 / Paillier / DSA
  密钥派生：KDF / AEAD / Poly1305 / OTP / OTS
  后量子：ML-KEM / ML-DSA / SLH-DSA / Lattice

编码层：
  Base64 / BER(DER) / PEM / CertificateAuthority

高级层：
  CnSEA.pas          — 椭圆曲线点计数（依赖 BigNumber + Polynomial + Prime + ECC）
  CnQRCode.pas       — 二维码生成
  CnCryptoIntf.pas   — DLL/SO 接口封装
  CnCryptoExport.pas — 算法导出
```

### 7.7 内存安全与侧信道防护

- 大数运算提供常量时间比较函数（`BigNumberConstTimeEqual`）
- RSA 解密提供 CRT 故障攻击防护（验证 m mod p == M1, m mod q == M2）
- 随机数使用密码学安全的生成器（`CnRandom.pas`）
- 内存清零函数（`MemoryCheckZero`）用于验证 KDF 输出非全零

---

## 八、构建指南

### 8.1 Delphi 包编译

包文件位于 `Package/<CompilerVersion>/`（如 `Package/Delphi130F/`）：

1. 先编译运行时包：`CnPack_D130F.dpk`
2. 再安装设计时包：`dclCnPack_D130F.dpk`

### 8.2 FPC 编译

非可视单元（Common、Crypto）支持 FPC 编译。典型命令：

```bash
fpc -MDelphi -Scghi -O1 -Fu../Source/Crypto -Fu../Source/Common <source_file>
```

FPC 的 Lazarus 包在 `Package/FPC3/`。

#### macOS 下的特殊链接要求

**在 macOS 上通过 fpc 命令行编译程序时，必须指定链接器使用的系统 SDK 路径**，否则即使编译通过也无法链接生成可执行文件。需要使用 `-XR` 开关指向 Xcode SDK 路径，例如：

```bash
fpc -MDelphi -Scghi -O2 \
  -Fu../Source/Common -Fu../Source/Crypto \
  -Fi../Source/Common -Fi../Source/Crypto \
  -XR/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.1.sdk \
  -FE. <source_file>
```

其中 `-XR` 后的路径需根据本机安装的 Xcode 版本及 SDK 版本进行调整。可通过以下命令查看可用的 SDK：

```bash
xcodebuild -showsdks
```

### 8.3 批量构建示例

```bash
Example/VCL/BuildVclExamples.bat    # dcc32 编译所有 VCL 示例
Example/FPC/BuildFpcExamples.bat    # FPC 编译所有 FPC 示例
Example/FMX/BuildFmxExamples.bat    # dcc32 编译所有 FMX 示例
```

### 8.4 其他脚本

| 脚本 | 说明 |
|------|------|
| `CleanInplace.bat` | 清理编译产物（*.dcu, *.ppu, *.o 等） |
| `GenCrypto.bat` | 加密模块代码生成/编译 |
| `GenMultiLang.bat` | 多语言资源生成 |
| `Source/ToCHS.bat` | 切换简体中文资源文件 |
| `Source/ToCHT.bat` | 切换繁体中文资源文件 |
| `Source/ToENU.bat` | 切换英文资源文件 |

---

## 九、开发规范

### 9.1 语言与兼容性

- **语言**：Object Pascal (Delphi)
- **编译器兼容性**：代码必须支持 Delphi 5 到最新版本。使用 `{$IFDEF DELPHI7_UP}` 等条件编译处理版本差异
- **命名约定**：所有单元以 `Cn` 前缀命名；常量单元命名为 `Cn*Consts.pas`

### 9.2 文件编码与格式

- `.pas`/`.dpk` 源文件使用 **GBK 编码**（非 UTF-8），且**不带 BOM**
- 行分隔符使用 **CRLF**（`\r\n`），不使用 LF
- `.dproj` 使用 UTF-8
- 每个文件有统一的 CnPack 版权头模板

### 9.3 全局编译选项

| 选项 | 设置 | 说明 |
|------|------|------|
| `{$R-}` | 关闭范围检查 | 性能优先 |
| `{$OVERFLOWCHECKS OFF}` | 关闭溢出检查 | 密码学运算需要 |
| `{$B-}` | 短路求值 | 标准布尔求值 |
| `{$H+}` | 长字符串 | 使用 AnsiString/WideString |

### 9.4 编码规范

#### 9.4.1 uses 子句

`uses` 中的单元名不使用命名空间前缀（如 `Vcl.`、`WinApi.` 等），直接使用基础单元名。例如写 `uses Classes, SysUtils;` 而非 `uses System.Classes, System.SysUtils;`。

#### 9.4.2 标识符命名

- **类型名**：interface 部分所有类型以 `TCn` 开头（如 `TCnBigNumber`、`TCnEccPoint`）
- **整型常量**：以 `CN_` 开头（如 `CN_SM2_FINITEFIELD_BYTESIZE`）
- **字符串常量**：以 `SCN_` 开头（如 `SCN_RSA_ERROR`）
- **标识符大小写**：标识符均采用首字母大写（PascalCase）；一两个字母的局部变量一般全大写（如 `I`、`SL`、`BN`）

#### 9.4.3 语法兼容性（面向 Delphi 5）

代码须兼容 Delphi 5 的基础语法，以下高级语法特性**不可使用**：

- **record 不支持方法**：不使用带方法的记录类型
- **不使用匿名函数**：不使用匿名方法/闭包
- **类中不支持 `const` 和 `type`**：类体内不嵌套常量或类型定义，常量和类型需定义在单元的独立 `const`/`type` 区
- **不使用带赋值的枚举类型语法**：如 `type TFoo = (a = 1, b = 2)` 不使用，一般用独立常量代替
- **不使用内联变量声明**（inline var）：局部变量必须在函数/过程头部的 `var` 段中显式声明
- **不使用 `Exit(True)` 语法**：需写成 `Result := True; Exit;` 两句

#### 9.4.4 类成员与属性

- 类的对外变量尽量使用 `property` 暴露
- 但类的**数组成员变量**不使用 `property`，以保证与 C++Builder 的兼容性

#### 9.4.5 整数类型规范

| 类别 | 类型 | 说明 |
|------|------|------|
| 无符号 8 位 | `Byte` | |
| 无符号 16 位 | `Word` | |
| 无符号 32 位 | `Cardinal` | |
| 无符号 64 位 | `TUInt64` | 自定义类型，兼容低版本 Delphi |
| 有符号 8 位 | `ShortInt` | |
| 有符号 16 位 | `SmallInt` | |
| 有符号 32 位 | `Integer` | |
| 有符号 64 位 | `Int64` | |

动态字节数组使用 `TBytes`，不要使用原始的 `array of Byte`。

#### 9.4.6 函数组织方式

不使用辅助工具类来集合常用函数当作方法集。通常使用固定前缀的独立函数（如 `CnRSAEncrypt`、`BigNumberAdd` 等），而非将函数封装为某个类的静态方法。

### 9.5 语言资源

默认简体中文（`{$DEFINE GB2312}`），可通过 `Source/ToCHT.bat`/`ToENU.bat` 切换繁中/英文。

### 9.6 包文件修改

修改包文件时，`.dpk` 和对应的 `.dproj` 必须同步修改（`.dproj` 中包含源码路径）。

---

## 十、关键文件快速索引

| 需求 | 查看文件 |
|------|----------|
| 了解编译器版本条件 | `Source/Common/CnPack.inc` |
| 大数运算 | `Source/Crypto/CnBigNumber.pas` |
| 椭圆曲线运算 | `Source/Crypto/CnECC.pas` |
| 椭圆曲线点计数 | `Source/Crypto/CnSEA.pas` |
| 多项式运算 | `Source/Crypto/CnPolynomial.pas` |
| SM2 国密算法 | `Source/Crypto/CnSM2.pas` |
| RSA 算法 | `Source/Crypto/CnRSA.pas` |
| Ed25519/X25519 | `Source/Crypto/Cn25519.pas` |
| 后量子密码 | `Source/Crypto/CnMLKEM.pas`, `CnMLDSA.pas`, `CnSLHDSA.pas` |
| 二维码生成 | `Source/Crypto/CnQRCode.pas` |
| 停靠框架 | `Source/NonVisual/CnDockGlobal.pas` |
| 多语言管理 | `Source/MultiLang/CnLangMgr.pas` |
| 网络通信 | `Source/NetComm/CnSocket.pas` |
| 线程池 | `Source/NonVisual/CnThreadPool.pas` |
| 数字水印 | `Source/Graphic/CnWatermark.pas` |
| 基础类 | `Source/Common/CnClasses.pas` |
| 通用工具函数 | `Source/Common/CnCommon.pas` |
| 平台整数类型 | `Source/Crypto/CnNative.pas` |
| 示例程序 | `Example/VCL/`, `Example/FPC/`, `Example/FMX/` |
| 包文件 | `Package/<CompilerVersion>/` |
