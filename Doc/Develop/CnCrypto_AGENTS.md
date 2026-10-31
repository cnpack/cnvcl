# CnCrypto（CnPack 密码算法库）项目指南 (AGENTS.md)

> 本文件为 AI 编程助手提供 CnPack 密码算法库（cncrypto）的完整上下文，涵盖目录结构、源码功能说明、架构设计、构建与测试方法、开发规范及安全协作约定等。

---

## 一、项目概述

CnPack 密码算法库（CnPack Crypto Library）源于 CnPack 开发组的 CnVcl 组件包，是一套**纯 Object Pascal** 编写的开源加解密源码库，**不依赖任何 VCL/FMX 界面组件**，以基础库形式提供。

- 支持编译器：Delphi 5 至最新版 RAD Studio、C++Builder 5/6、Lazarus/Free Pascal (FPC)
- 支持平台：32/64 位的 Windows、macOS、Linux（**仅在小端 CPU 上测试过，大端 CPU 不保证支持**）
- 算法覆盖：国际主流算法 + 中国国家商用密码算法（SM2/SM3/SM4/SM9/ZUC）+ 后量子算法（ML-KEM/ML-DSA/SLH-DSA）
- 零外部依赖（纯 Pascal 实现）
- 官方网站：https://www.cnpack.org
- 开源协议：CnPack 开源协议（详见 `License.txt`）

---

## 二、与 cnvcl 主库的关系（重要）

**本目录整体是生成物**。日常开发在 CnVcl 主库中进行，通过构建脚本同步生成：

- 在 Windows 下运行 `cnvcl\GenCrypto.bat`，会**先删除再重建**整个 `cncrypto` 目录
- `Source/` 内容 = `cnvcl/Source/Common/` 的 15 个公共单元 + `CnPack.inc` + `cnvcl/Source/Crypto/` 的全部 `.pas`（但 `CnCryptoIntf.pas` 被移到 `Include/`，实验性单元不包含）
- `Include/`、`Package/`、`Test/`、`Example/`、`Doc/`、`README.md`、`License.txt`、`Clean.bat` 及**本文件（AGENTS.md）**均由该脚本从 `cnvcl` 复制或生成

因此：

1. **直接修改 `cncrypto/` 下的任何内容（包括本文件）都可能在下次运行 `GenCrypto.bat` 时被覆盖**。
2. 源码的正式修改应在 `cnvcl/Source/`（Common 或 Crypto）中进行，再重新生成同步。
3. 本文件的持久模板位于 `cnvcl/Doc/Develop/CnCrypto_AGENTS.md`。
4. 本库与 CnVcl 组件包**不可同时编译安装**（单元命名冲突）；已安装 CnPack 组件包的用户无需也不应再编译本库的包。

---

## 三、顶层目录结构

```
cncrypto/
├── Source/        # 全部源码（扁平结构，无子目录，68 个 .pas + 1 个 .inc）
├── Include/       # 动态库导出接口（CnCryptoIntf.h / CnCryptoIntf.pas）
├── Package/       # CnCrypto.dpk(BPL) / CnCrypto.dpr(Delphi DLL) / CnCrypto.lpr(FPC SO/DYLIB)
├── Test/          # 批量测试工程（CryptoTest.pas + 各 IDE 工程文件）
├── Example/       # Delphi/ 与 Lazarus/ 两套演示示例
├── Doc/           # 算法实现说明文档
├── Help/          # HTML 帮助文件
├── AGENTS.md      # 本文件
├── Clean.bat      # 清理编译产物脚本
├── License.txt    # 中英双语许可协议
└── README.md      # 中英双语项目说明
```

---

## 四、Source/ 源码单元详解

所有 `.pas` 单元均通过 `{$I CnPack.inc}` 引入全局条件编译配置（`CnPack.inc` 也位于本目录）。以下按功能分类列出全部 68 个单元。

### 4.1 全局配置与公共基础（同步自 cnvcl/Source/Common）

| 文件 | 功能说明 |
|------|----------|
| `CnPack.inc` | **全局条件编译配置文件**。检测编译器版本（`VERxxx`），定义 `DELPHIX`、`COMPILERX` 等符号及累积符号（`DELPHI7_UP` 等），设置功能标志（`UNICODE_STRING`、`SUPPORT_WIN64` 等）。FPC 编译时自动设置 `{$MODE Delphi}` 并映射 CPU 架构定义。 |
| `CnConsts.pas` | 全局常量与错误码定义单元。定义 `ECN_OK`、`ECN_FILE_NOT_FOUND` 等通用错误码。 |
| `CnNative.pas` | 平台整数类型统一单元。定义 `TCnNativeInt`、`TCnNativeUInt`、`TUInt64` 等，解决不同 Delphi 版本间 `NativeInt`/`NativeUInt`/64 位无符号运算差异；提供大小端检测、字节序转换、常量时间交换/选择/比较等底层函数。 |
| `CnStrings.pas` | 字符串工具单元。字符串分割、匹配、UTF-8 处理、StringBuilder 等函数。 |
| `CnWideStrings.pas` | 宽字符串列表单元。兼容 Delphi 各版本的宽字符串容器。 |
| `CnContainers.pas` | 通用容器类单元。简单栈、队列等容器。 |
| `CnHashMap.pas` | 哈希映射（HashMap）实现单元。 |
| `CnTree.pas` | 树数据结构实现单元。 |
| `CnMath.pas` | 数学函数库单元。扩展数学运算函数。 |
| `CnMatrix.pas` | 矩阵运算单元。浮点矩阵的加减乘除、转置、求逆等。 |
| `CnFloat.pas` | 高精度浮点运算单元。扩展浮点数（Extended）运算支持。 |
| `CnBigDecimal.pas` | 十进制无限精度大数单元。基于 `TCnBigBinary` 表示有效数字、Integer 表示十进制指数。 |
| `CnBigRational.pas` | 有理数（分数）单元。基于两个 `TCnBigNumber` 表示分子和分母。 |
| `CnCalendar.pas` | 历法计算单元。公历/农历转换（说明见 `Doc/CnCalendar历法说明.txt`）。 |
| `CnFileUtils.pas` | 文件工具单元。文件/目录操作扩展函数。 |
| `CnZip.pas` | ZIP 压缩/解压单元。 |

### 4.2 数学与数论基础（密码学基石）

| 文件 | 功能说明 |
|------|----------|
| `CnBigNumber.pas` | **大数运算核心单元**（约 11600 行）。`TCnBigNumber` 类：加减乘除、模幂、GCD、素数检测、平方根、比特操作等。内部使用 UInt32/UInt64 数组存储，支持多线程；提供固定时间窗口与滑动窗口两种模幂实现（`FAST_POWERMOD` 开关切换）；含 `TCnBigNumberPool` 对象池与常量时间比较。 |
| `CnPrime.pas` | **素数算法单元**（约 3560 行）。预生成 2^16 以内素数表，Miller-Rabin 素性检测、BPSW 复合检测、Lucas U/V 序列、大数 IsPerfectPower 判断等。 |
| `CnPolynomial.pas` | **多项式运算单元**（约 17680 行）。Int64 系数和 BigNumber 系数的一元/二元多项式运算，GCD、模运算、求导、求值、有限域因式分解等。 |
| `CnComplex.pas` | 复数运算单元。复数的加减乘除、幂、指数、对数等。 |
| `CnDFT.pas` | 离散傅里叶变换单元。DFT/IDFT 及 FFT 快速算法。 |
| `CnVector.pas` | 向量运算单元。Float 和 BigNumber 的向量运算。 |
| `CnInt128.pas` | 128 位整数运算单元。在不原生支持 Int128 的编译器上提供 128 位整数运算。 |
| `CnBits.pas` | 位操作工具单元。位级读写和位域操作。 |

### 4.3 对称加密与序列密码

| 文件 | 功能说明 |
|------|----------|
| `CnAES.pas` | AES 单元。AES-128/192/256 的 ECB/CBC/CTR 等模式；CCM/GCM 模式在 `CnAEAD.pas` 中。 |
| `CnDES.pas` | DES/3DES 单元。DES 与 Triple DES 的 ECB/CBC 模式。 |
| `CnSM4.pas` | SM4 国密分组密码单元（GB/T 32907-2016）。 |
| `CnRC4.pas` | RC4 流密码单元。 |
| `CnChaCha20.pas` | ChaCha20 流密码单元。RFC 8439 ChaCha20-Poly1305。 |
| `CnTEA.pas` | TEA/XTEA/XXTEA 加密单元。 |
| `CnZUC.pas` | ZUC 祖冲之序列密码单元（GB/T 33133-2016）。 |
| `CnFEC.pas` | 前向纠删码（FEC）单元。Reed-Solomon 纠删码。 |

### 4.4 哈希与消息认证

| 文件 | 功能说明 |
|------|----------|
| `CnMD5.pas` | MD5 消息摘要单元。 |
| `CnSHA1.pas` | SHA-1 消息摘要单元。 |
| `CnSHA2.pas` | SHA-2 单元。SHA-224/256/384/512。 |
| `CnSHA3.pas` | SHA-3/Keccak 单元。SHA-3-224/256/384/512 及 SHAKE。 |
| `CnSM3.pas` | SM3 国密杂凑单元（GM/T 0004-2012）。 |
| `CnBLAKE.pas` | BLAKE 哈希单元。 |
| `CnBLAKE2.pas` | BLAKE2 单元。BLAKE2b/BLAKE2s。 |
| `CnBLAKE3.pas` | BLAKE3 哈希单元。 |
| `CnCRC32.pas` | CRC 校验单元。CRC8/16/32/64。 |
| `CnFNV.pas` | FNV 非加密哈希单元。 |
| `CnXXH.pas` | xxHash 非加密快速哈希单元（32/64 位）。 |
| `CnPoly1305.pas` | Poly1305 消息认证码（MAC）单元。 |

### 4.5 公钥密码与数字签名

| 文件 | 功能说明 |
|------|----------|
| `CnRSA.pas` | **RSA 单元**（约 3900 行）。大数范围 RSA 公私钥生成、加解密、签名验签；PKCS1/OAEP 填充；CRT 加速解密（含盲化与故障攻击防护）；PEM 格式密钥读写（PKCS1/PKCS8）；另提供 Int64 范围简化实现。 |
| `CnDSA.pas` | DSA 数字签名算法单元。 |
| `CnECC.pas` | **椭圆曲线密码核心单元**（约 10460 行）。Weierstrass 曲线 `y^2 = x^3 + Ax + B mod p` 的点运算、加解密、签名验签；Int64 和大数范围；Schoof 点计数；PEM 密钥读写、ASN.1 编解码。 |
| `CnSM2.pas` | **SM2 国密椭圆曲线单元**（GM/T 0003.x-2012）。数据加解密、签名验签、密钥交换、协同密钥生成、协同签名、协同加密等。继承自 `TCnEcc`。 |
| `CnSM9.pas` | SM9 国密标识密码单元（GM/T 0044-2016）。基于标识的加密和签名。 |
| `Cn25519.pas` | **Curve25519/Ed25519 单元**（约 6400 行）。X25519 密钥交换、Ed25519 签名，及 Ristretto255、Ed448、X448 等变体。 |
| `CnPaillier.pas` | Paillier 同态加密单元。支持加法同态运算。 |
| `CnSecretSharing.pas` | 秘密共享单元。Shamir 门限方案及 Feldman VSS 可验证秘密共享。 |

### 4.6 密钥派生、认证加密与口令

| 文件 | 功能说明 |
|------|----------|
| `CnKDF.pas` | 密钥派生单元。PBKDF2、HKDF、SM2 KDF 等。 |
| `CnAEAD.pas` | AEAD 认证加密单元。GCM/CCM 等模式的认证加密封装。 |
| `CnOTP.pas` | 一次性密码单元。RFC 4226 HOTP、RFC 6238 TOTP 及国密动态口令（GB/T 38556-2020）。 |
| `CnOTS.pas` | 一次性签名（OTS）单元。Lamport/Winternitz 方案。 |

### 4.7 后量子密码

| 文件 | 功能说明 |
|------|----------|
| `CnMLKEM.pas` | **ML-KEM 单元**（FIPS 203，原 Kyber）。基于模块格的密钥封装机制，支持 ML-KEM-512/768/1024。 |
| `CnMLDSA.pas` | **ML-DSA 单元**（FIPS 204，原 Dilithium）。基于模块格的数字签名算法。 |
| `CnSLHDSA.pas` | **SLH-DSA 单元**（FIPS 205，原 SPHINCS+）。无状态基于哈希的数字签名。 |
| `CnLattice.pas` | 格密码学基础单元。格运算和多项式环运算的基础实现（含 NTRU 相关支持）。 |

### 4.8 高级数论（椭圆曲线求阶）

| 文件 | 功能说明 |
|------|----------|
| `CnSEA.pas` | **Schoof-Elkies-Atkin 算法单元**（约 3660 行）。计算有限域上椭圆曲线 #E(F_p)。分 Elkies 阶段（Frobenius 迹特征多项式）与 Atkin 阶段（Atkin 素数信息组合求解），支持模多项式文件加载。依赖 BigNumber + Polynomial + Prime + ECC。 |

### 4.9 编码、格式与应用工具

| 文件 | 功能说明 |
|------|----------|
| `CnBase64.pas` | Base64/Base64URL/Base32 编解码单元。 |
| `CnBerUtils.pas` | **BER/DER 编解码单元**（约 1700 行）。ASN.1 BER/DER 格式读写解析，用于 X.509 证书、PKCS 密钥等结构化数据。 |
| `CnPemUtils.pas` | PEM 格式工具单元。PEM 文件加载与保存，支持加密的 PEM。 |
| `CnCertificateAuthority.pas` | **CA 证书管理单元**。X.509 证书生成、签名、验证，支持自签名 CA 和终端证书。 |
| `CnPDFCrypt.pas` | PDF 加密单元。PDF 文件密码保护加密。 |
| `CnQRCode.pas` | **二维码生成单元**（约 4825 行）。QR Code Model 2，版本 1-40、全部纠错级别，数字/字母/字节/汉字模式，自动选择最优编码。 |
| `CnRandom.pas` | 安全随机数单元。密码学安全的随机数生成器（CSPRNG）。 |
| `CnCryptoExport.pas` | 动态库导出封装单元。将算法封装为 `CnCrypto.dpr`/`CnCrypto.lpr` 动态库的导出接口（配合 `Include/CnCryptoIntf.*`）。 |

---

## 五、Include/ 动态库导出接口

| 文件 | 功能说明 |
|------|----------|
| `CnCryptoIntf.pas` | **C ABI 导出接口的 Pascal 定义**。以 `cn_` 前缀导出百余个函数，涵盖：版本与初始化（`cn_get_version`/`cn_lib_init`）、内存管理（`cn_alloc`/`cn_free`/`cn_memzero`）、字节序检测、常量时间比较/选择、Base64/32、KDF（PBKDF2/HKDF）、OTP、哈希与 HMAC、对称加解密与 AEAD、RSA/ECC/SM2/Ed25519/Curve25519 密钥管理与签名验签、ML-KEM/ML-DSA 等。 |
| `CnCryptoIntf.h` | 对应的 C 头文件，供 C/C++ 调用者链接参考。 |

注意：导出函数中字符串参数支持双字节（Unicode）或单字节（ANSI），取决于编译动态库的编译器是否为 Unicode 版本。修改 ABI 前必须同时检查 `.h` 与 `.pas` 两个文件，并保持调用约定、结构布局、导出符号与错误码稳定。

---

## 六、Package/ 工程文件

| 文件 | 说明 |
|------|------|
| `CnCrypto.dpk` | Delphi 运行期包（编译为 BPL）。Delphi 5 下需手工将 `requires` 中的 `vcl` 改为 `vcl50`。 |
| `CnCrypto.dpr` | Delphi 动态库工程（编译为 Windows DLL）。 |
| `CnCrypto.lpr` | FPC 动态库工程（编译为 Linux SO / macOS DYLIB）。 |

三个工程均为脚本生成，`uses` 列表与 `Source/` 单元一一对应；**在 Source 中新增/删除单元后必须同步维护这三个工程文件**（正式流程是在 cnvcl 中修改 `GenCrypto.bat` 后重新生成）。

---

## 七、Test/ 测试框架

| 文件 | 说明 |
|------|------|
| `CryptoTest.pas` | **批量测试单元**。测试框架约定如下（修改/新增测试必须遵守）：<br>1. 每个测试是一个无参函数 `function TestXxx: Boolean;`，**声明在 interface 区**、实现在 implementation 区、按算法分节注释组织；<br>2. 总入口 `procedure TestCrypto` 中以 `MyAssert(TestXxx, 'TestXxx');` 注册执行，`MyAssert` 会计时运行并用 Assert 断言结果（失败抛异常终止并报告）；<br>3. 入口先打印 CPU 位数、指针/Extended 长度、大小端信息，最后输出通过/失败计数；<br>4. 该单元为保持纯净性**不 `{$I CnPack.inc}`**，仅在 FPC 下 `{$MODE Delphi}` 并关闭 Range Check。 |
| `Crypto.dpr` | Delphi 控制台测试工程。 |
| `Crypto.lpr` / `Crypto.lpi` | FPC 命令行 / Lazarus 测试工程。 |
| `Crypto.bpr` / `Crypto.bpf` / `Crypto.cpp` | C++Builder 5/6 测试工程。 |

运行方式见"十、构建指南"。部分用例耗时较长（可能数小时）。

---

## 八、Example/ 与 Doc/、Help/

### 8.1 Example/

- `Example/Delphi/`：28 个 Delphi（VCL）示例工程，源自 `cnvcl/Example/VCL/` 对应子目录（搜索路径已改写为本库 `Source`）
- `Example/Lazarus/`：20 个 Lazarus/FPC 示例工程，源自 `cnvcl/Example/FPC/`
- 覆盖：25519、BerParse、BigDecimal、BigNumber、BigRational、CertificateAuthority、Complex、Crypto、CryptoDLL、CryptoExport、DFT、DSA、ECC、Float、Int128、KDF、MLDSA、MLKEM、Native、OTP、OTS、Pailler、Polynomial、PrimeNumber、RSA、SEA、SecretSharing、SLHDSA、SM2、SM9
- 示例工程本身一般仅保证在 Win32/Win64 下运行；**修改公共 API 后应检查示例是否仍能编译**。

### 8.2 Doc/

算法实现说明文档（安全性质的论述不能替代源码审查）：

| 文件 | 内容 |
|------|------|
| `CnPack密码算法库怎样抵抗侧信道攻击.txt` | 侧信道防护的设计与限制 |
| `CnPack密码算法库怎样实现内存安全.txt` | 内存安全（清零、边界）的设计 |
| `CnRSA算法实现说明与帮助.txt` | RSA 实现细节与使用说明 |
| `如何在Delphi7下手动支持64位无符号整数运算.txt` | 旧编译器下 `TUInt64` 的实现原理 |
| `CnCalendar历法说明.txt` | 历法算法说明 |

### 8.3 Help/

HTML 格式的帮助文件（接口与用法参考）。

---

## 九、架构设计

### 9.1 分层依赖

```
配置层：
  CnPack.inc        — 条件编译配置（编译器版本、功能标志）

基础类型层：
  CnNative.pas      — 平台整数类型、字节序、常量时间原语
  CnConsts.pas      — 错误码与常量

数学基础层：
  CnBigNumber.pas   — 大数运算（公钥密码的基石）
  CnPrime.pas       — 素数检测
  CnPolynomial.pas  — 多项式（后量子/SEA 依赖）
  CnInt128.pas / CnBits.pas / CnComplex.pas / CnDFT.pas / CnVector.pas
  CnBigDecimal / CnBigRational / CnMatrix / CnFloat / CnMath

算法层：
  对称加密：AES / DES / SM4 / RC4 / ChaCha20 / TEA / ZUC
  哈希摘要：MD5 / SHA1 / SHA2 / SHA3 / SM3 / BLAKE·2·3 / CRC / FNV / xxHash
  消息认证：Poly1305 / HMAC / OTS
  公钥密码：RSA / DSA / ECC / SM2 / SM9 / 25519 / Paillier / SecretSharing
  密钥管理：KDF / AEAD / OTP
  后量子：  MLKEM / MLDSA / SLHDSA / Lattice
  高级数论：SEA（椭圆曲线求阶）

编码格式层：
  Base64 / BER(DER) / PEM / CertificateAuthority

应用工具层：
  QRCode / PDFCrypt / Zip / Calendar / Random / FileUtils / Strings 等

导出层：
  CnCryptoExport.pas + Include/CnCryptoIntf.h/.pas — DLL/SO/DYLIB 的 C ABI
```

### 9.2 密码学类型与安全原语体系

- **64 位无符号**：旧 Delphi 下 `TUInt64 = Int64` 模拟；无符号比较必须用 `UInt64Compare`，禁止直接用可能被解释为有符号的 `<`、`>`、`<=`、`>=`。
- **指针宽度**：地址与偏移用 `TCnNativeUInt`/`TCnIntAddress`，乘法/加法前检查溢出。
- **常量时间原语**：`BigNumberConstTimeEqual`（逐 limb 精确相等）、常量时间 Swap/Select/ExpandBool 等，用于防时序侧信道。
- **CRT 防护**：RSA 私钥运算经盲化，CRT 结果有公钥复核/故障检测。
- **CSPRNG**：`CnRandom.pas` 按平台选择安全随机源（Windows BCrypt/CryptoAPI、Linux getrandom、macOS 系统 CSPRNG），懒初始化需同步保护。
- **内存安全**：密钥、明文、中间状态在失败/异常路径上清零；详见 `Doc/CnPack密码算法库怎样实现内存安全.txt` 与 `Doc/CnPack密码算法库怎样抵抗侧信道攻击.txt`。

---

## 十、构建指南

### 10.1 源码方式（推荐）

直接将 `Source/` 目录加入 Delphi 或 Lazarus 工程的搜索路径，`uses` 相应单元即可。

### 10.2 Delphi 运行期包（BPL）

在任一版本 Delphi 中打开 `Package/CnCrypto.dpk` 编译。Delphi 5 需手工将 `requires` 中的 `vcl` 改为 `vcl50`。注意与 CnVcl 组件包不可同装。

### 10.3 动态库（DLL/SO/DYLIB）

- Windows：Delphi 编译 `Package/CnCrypto.dpr` 生成 DLL
- Linux/macOS：FPC 编译 `Package/CnCrypto.lpr` 生成 SO/DYLIB
- 调用方参考 `Include/CnCryptoIntf.h` / `CnCryptoIntf.pas`

### 10.4 FPC 命令行编译测试

**Windows（Win32 FPC 示例，路径按本机调整）：**

```bat
cd Test
C:\lazarus\fpc\3.2.2\bin\i386-win32\fpc.exe -Fu..\Source Crypto.lpr
Crypto.exe
```

**Linux/macOS（fpc 已在 PATH）：**

```sh
cd Test
fpc -Fu../Source Crypto.lpr
./Crypto
```

**macOS 链接特别说明**：fpc 命令行编译必须指定系统 SDK 路径，否则即使编译通过也无法链接（`ld: library not found for -lc`）。使用 `-XR` 指向 Xcode SDK：

```sh
fpc -Fu../Source -XR/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk Crypto.lpr
```

SDK 路径按本机 Xcode 版本调整，可用 `xcodebuild -showsdks` 查看。

### 10.5 其他

- `Clean.bat`：清理编译产物（`*.dcu`、`*.ppu`、`*.o`、`*.rsj` 等），这些中间文件已在 `.gitignore` 排除，不要提交。
- 一次 Win32 编译通过**不能**代替 Win64、Linux、macOS、ARM 等其他目标的验证。

---

## 十一、开发规范

### 11.1 语言与兼容性

- **语言**：Object Pascal (Delphi)
- **编译器兼容性**：代码必须支持 Delphi 5 到最新版本，以及 C++Builder 5/6 与 FPC/Lazarus。使用 `{$IFDEF DELPHI7_UP}` 等条件编译处理版本差异（符号定义见 `Source/CnPack.inc`）
- **命名约定**：所有单元以 `Cn` 前缀命名

### 11.2 CnPack.inc 条件编译符号

`CnPack.inc` 完成三件事，代码中应使用它定义的符号，而非在业务代码中自行判断编译器版本：

1. **编译器版本检测**：通过 `{$IFDEF VERxxx}` 定义 `DELPHIX`、`COMPILERX`、`BDSX` 等符号
2. **累积版本符号**：定义 `DELPHI7_UP`、`COMPILER12_UP` 等 `_UP` 后缀符号，用于条件编译（如 `{$IFDEF DELPHI7_UP}` 表示 Delphi 7 及以上）
3. **功能标志**：根据版本定义 `UNICODE_STRING`、`SUPPORT_WIN64`、`SUPPORT_INLINE` 等功能标志

FPC 编译时自动设置 `{$MODE Delphi}` 并映射 CPU 架构定义（`CPUX86`、`CPUX64` 等）。

### 11.3 语法兼容性（面向 Delphi 5）

代码须兼容 Delphi 5 的基础语法，以下高级语法特性**不可使用**：

- **record 不支持方法**：不使用带方法的记录类型
- **不使用匿名函数**：不使用匿名方法/闭包、类助手
- **类中不支持 `const` 和 `type`**：类体内不嵌套常量或类型定义，常量和类型需定义在单元的独立 `const`/`type` 区
- **不使用带赋值的枚举类型语法**：如 `type TFoo = (a = 1, b = 2)` 不使用，一般用独立常量代替
- **不使用内联变量声明**（inline var）：局部变量必须在函数/过程头部的 `var` 段中显式声明
- **不使用 `Exit(True)` 语法**：需写成 `Result := True; Exit;` 两句
- **不假设 `UInt64`/`NativeUInt`/`NativeInt` 在旧编译器中存在或语义一致**：一律使用 `CnNative.pas` 提供的兼容类型与封装函数

### 11.4 uses 子句

`uses` 中的单元名不使用命名空间前缀（如 `System.`、`Winapi.` 等），直接使用基础单元名。例如写 `uses Classes, SysUtils;` 而非 `uses System.Classes, System.SysUtils;`。

### 11.5 标识符命名

- **类型名**：interface 部分所有类型以 `TCn` 开头（如 `TCnBigNumber`、`TCnEccPoint`）
- **整型常量**：以 `CN_` 开头（如 `CN_SM2_FINITEFIELD_BYTESIZE`）
- **字符串常量**：以 `SCN_` 开头（如 `SCN_RSA_ERROR`）；新增异常消息使用 `resourcestring`，命名遵循相应单元已有前缀
- **标识符大小写**：标识符均采用首字母大写（PascalCase）；一两个字母的局部变量一般全大写（如 `I`、`SL`、`BN`）

### 11.6 类成员与属性

- 类的对外变量尽量使用 `property` 暴露
- 但类的**数组成员变量**不使用 `property`，以保证与 C++Builder 的兼容性

### 11.7 整数类型与指针安全

整数类型规范：

| 类别 | 类型 | 说明 |
|------|------|------|
| 无符号 8 位 | `Byte` | |
| 无符号 16 位 | `Word` | |
| 无符号 32 位 | `Cardinal` | |
| 无符号 64 位 | `TUInt64` | 自定义类型，兼容低版本 Delphi（旧编译器下以 `Int64` 模拟） |
| 有符号 8 位 | `ShortInt` | |
| 有符号 16 位 | `SmallInt` | |
| 有符号 32 位 | `Integer` | |
| 有符号 64 位 | `Int64` | |

- 动态字节数组使用 `TBytes`，不要使用原始的 `array of Byte`
- 无符号 64 位比较必须使用 `UInt64Compare` 等 `CnNative` 封装函数，不要直接使用可能被解释为有符号比较的 `<`、`>`、`<=`、`>=`
- 对长度参数先检查负数，再转换为无符号 API 参数；`Integer` 长度与 `Cardinal`、`NativeUInt`、C 的 `size_t` 之间不能隐式假定安全
- 指针地址和偏移应使用 `TCnNativeUInt`/`TCnIntAddress` 等项目类型，并在乘法、加法前检查溢出；不能先用 `Integer` 或 `Int64` 计算再强制转换为指针宽度类型

### 11.8 函数组织方式

不使用辅助工具类来集合常用函数当作方法集。通常使用固定前缀的独立函数（如 `CnRSAEncrypt`、`BigNumberAdd` 等），而非将函数封装为某个类的静态方法。

### 11.9 全局编译选项

| 选项 | 设置 | 说明 |
|------|------|------|
| `{$R-}` | 关闭范围检查 | 性能优先 |
| `{$OVERFLOWCHECKS OFF}` | 关闭溢出检查 | 密码学运算需要 |
| `{$B-}` | 短路求值 | 标准布尔求值 |
| `{$H+}` | 长字符串 | 使用 AnsiString/WideString |

### 11.10 文件编码与格式

- `.pas` 源文件使用 **GBK（ANSI）编码 + CRLF 换行，无 BOM**；终端按 UTF-8 读取出现乱码不代表源码损坏
- **不要把源码批量转成 UTF-8**，不要因乱码重写无关文件
- 修改 GBK 文件的流程：只对目标文件做临时 GBK→UTF-8 转换 → 最小范围编辑 → 转回 GBK/CRLF → 抽样检查中文、换行和文件头字节，确认无替换字符或整文件编码 diff
- `.md` 等文档使用 UTF-8；本文件为 UTF-8 + CRLF
- 每个源文件带统一的 CnPack 版权头模板

### 11.11 条件编译规范

如果代码中需要使用 `{$IFDEF` 这样的条件编译来区分代码的行为，那要尽量避免出现局部多个 `var`、`const`、`begin end` 对，语句中也要避免出现标识符并列，以免影响格式化。因为部分格式化工具会将条件编译指令解释为注释，而在当成注释的情况下，多个 `var`、`const`、`begin end` 对，以及语句中出现的简单的两个或更多的标识符并列而没有其他符号分开，必然导致格式化时出语法错误。

### 11.12 工程文件同步

在 `Source/` 中新增或删除单元后，必须同步维护由脚本生成的各工程文件中的单元列表：`Package/CnCrypto.dpk`（contains）、`CnCrypto.dpr`/`CnCrypto.lpr`（uses 与 exports）、`Test/Crypto.dpr`、`Crypto.lpr`、`Crypto.bpr`/`Crypto.bpf`。正式流程是在 cnvcl 中修改 `GenCrypto.bat` 后重新生成。

### 11.13 修改工作流

1. `git status --short` 记录并保护已有未提交修改；不要用 `git reset --hard` 等清理工作区
2. 通读相关单元的完整接口与实现，定位所有声明、实现、包装函数、导出接口和测试调用点
3. 先建立最小复现或负向边界测试，再做最小修复；不重排无关代码、不批量格式化
4. 修改后检查编码/CRLF、资源字符串、条件编译分支和旧编译器语法，编译相关单元及 `Test/Crypto.lpr`
5. `git diff --stat` 核对只有目标 hunk；报告具体文件、函数、行为变化、兼容性影响和验证命令
6. 长时间测试需先告知用户；终止测试只杀本次对应的精确进程

---

## 十二、安全协作与审计约定

安全相关任务必须以**源码实际行为、调用链和可复现实验**为依据，不因注释、函数名或"已验证/安全/严格"等文字跳过代码检查。所有"常量时间""严格"类声明必须由代码与负向测试重新证明。主要审计红线：

| 领域 | 核心红线 |
|------|----------|
| API 与内存边界 | 长度/容量/偏移先查负值、截断与溢出；正长度必须有非空指针；输出必须验证容量；长度与容量不混用 |
| C ABI | Pascal 异常、对象指针不得跨边界；导出函数把异常映射为稳定错误码；核验句柄类型、生命周期、重复释放 |
| AEAD | GHash/GCM/CCM 的长度上限、Tag/Nonce 约束与标准逐项核对；先写临时缓冲、认证成功才输出明文；Tag 恒时全长度比较；失败清理全部敏感中间数据 |
| CSPRNG | 负长度失败、零长度 no-op、正长度 nil 必须失败；懒初始化需同步保护；Linux getrandom 处理短读与 EINTR |
| RSA | 私钥运算必须盲化；CRT 结果复核；PKCS#1 填充解析严格分离检查；新密钥至少 2048 位、e=65537；raw RSA 不得与生产 API 同级暴露 |
| ECC/SM2/Ed25519 | 验签拒绝单位元、小阶、无效/非规范编码点；私钥范围校验；nonce 用 RFC 6979 或审查过的确定性方案；秘密不得控制循环或表索引 |
| 大数与恒时 | 恒时比较为逐 limb 精确相等；"常量时间"须查实际编译产物，禁止秘密相关分支/动态长度/秘密索引 |
| 后量子 | ML-KEM 解封装无条件计算候选与回退密钥；畸形输入返回稳定失败；密钥导入精确消费固定长度 |
| 对称与哈希 | MD5/SHA-1/DES/RC4/ECB 等仅作 Legacy 兼容，不得成为新安全 API 默认值 |
| PEM/KDF | 口令保护优先现代格式（PBES2/Argon2 等 + AEAD）；传统格式只读兼容并告警；防 padding 预言机 |
| BER/证书 | 长度/深度/最小编码/尾随字节严格检查；仅验签名的 Verify 须明确命名 signature-only，不冒充信任链验证 |
| 平台与并发 | 平台 API 不能互相冒充；仅小端测试过，大端安全不得外推；全局状态/懒加载/对象池须明确线程安全 |

**不应做的事情**：不为通过测试放宽解析器或吞掉认证失败；不混淆密文主体/附加数据/Tag/编码总长度；不在未确认编码前用脚本重写整文件；不删除用户未提交的安全修复与测试；不凭单平台编译通过宣称全平台兼容。

---

## 十三、关键文件快速索引

| 需求 | 查看文件 |
|------|----------|
| 编译器版本条件编译 | `Source/CnPack.inc` |
| 平台类型与常量时间原语 | `Source/CnNative.pas` |
| 大数运算 | `Source/CnBigNumber.pas` |
| 素数/多项式 | `Source/CnPrime.pas`、`Source/CnPolynomial.pas` |
| RSA 算法 | `Source/CnRSA.pas` |
| 椭圆曲线/SM2 | `Source/CnECC.pas`、`Source/CnSM2.pas` |
| Ed25519/X25519 | `Source/Cn25519.pas` |
| 国密 SM3/SM4/SM9/ZUC | `Source/CnSM3.pas`、`CnSM4.pas`、`CnSM9.pas`、`CnZUC.pas` |
| 后量子密码 | `Source/CnMLKEM.pas`、`CnMLDSA.pas`、`CnSLHDSA.pas`、`CnLattice.pas` |
| 椭圆曲线求阶 | `Source/CnSEA.pas` |
| AEAD（GCM/CCM） | `Source/CnAEAD.pas` |
| BER/PEM/证书 | `Source/CnBerUtils.pas`、`CnPemUtils.pas`、`CnCertificateAuthority.pas` |
| 动态库接口 | `Include/CnCryptoIntf.h`、`Include/CnCryptoIntf.pas`、`Source/CnCryptoExport.pas` |
| 批量测试 | `Test/CryptoTest.pas`（入口 `TestCrypto`，注册用 `MyAssert`） |
| 侧信道/内存安全设计 | `Doc/CnPack密码算法库怎样抵抗侧信道攻击.txt`、`Doc/CnPack密码算法库怎样实现内存安全.txt` |
| 示例 | `Example/Delphi/`、`Example/Lazarus/` |
