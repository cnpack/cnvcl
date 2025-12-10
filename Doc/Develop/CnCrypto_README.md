# CnPack 密码算法库

## 概述

CnPack 密码算法库（CnPack Crypto Library）源于 CnPack 开发组的开源项目 CnVcl 组件包（CnVcl Component Package），是一套以纯 Object Pascal 语言编写的开源加解密源码库，支持 Delphi 5 至最新版本的 RAD Studio，支持 C++Builder 5/6 及 Lazarus/FPC，并支持 32 位与 64 位的 Windows、MacOS、Linux 等操作系统。

CnPack 密码算法库致力于提供强大的加解密算法支持与数据安全保障。它覆盖了国内外常见的加解密算法及中国国家商用密码算法，包括对称加密、非对称加密、杂凑（或名散列、摘要、哈希）等大类及众多其他基础与辅助功能。

## 许可协议

CnPack 开发包以开放源码（Open Source）的形式发布，遵守 CnPack 的许可协议，受 CnPack 许可协议的保护。License 文件中有该协议的详细描述，具体内容可参考协议文件。

## 主要内容

- **对称加密**：SM4、AES、DES、3DES、RC4、ChaCha20、TEA、ZUC
- **非对称加密**：RSA、ECC 椭圆曲线（魏尔斯特拉斯曲线，包括 SM2）、25519/448 椭圆曲线（蒙哥马利及扭曲爱德华曲线）
- **杂凑**：SM3、MD5、SHA1、SHA2/SHA3/BLAKE 系列（224、256、384、512）SHAKE、BLAKE2（2S/2B）、XXHash（32/64）、CRC8/16/32/64、Poly1305、FNV、一次性杂凑签名算法 OTS
- **其他密码学领域**：SM9、同态加密、协同加密、格密码、NTRU
- **编码解码**：Base64、ASN.1（DER/BER）、Hamming 校验
- **CA证书**：RSA/ECC 证书申请、签发与校验
- **大数支持**：大整数、大有理数、大浮点数、一元大数多项式、二元大数多项式、一元大有理数分式
- **运算支持**：素数运算与判定、浮点复数运算、位运算、矩阵运算、离散傅立叶变换、128 位整型支持
- **数论计算**：中国剩余定理、Lucas 序列、PollardRho 因数分解、模素数二次剩余、勒让德/雅可比符号
- **抗量子计算**：基于模块化格的密钥封装机制 MLKEM、基于模块化格的数字签名算法 MLDSA
- **其他**：DSA 签名验证、关联数据认证加密 AEAD、动态口令 OTP、密钥派生 KDF、秘密共享的 Shamir 门限方案及 Feldman VSS 方案、历法计算

## 国密支持

CnPack 密码算法库对中国国家商用密码算法标准有着完整而强大的支持：

- **SM2**：GM/T 0003.x-2012《SM2椭圆曲线公钥密码算法》，1～5
- **SM3**：GM/T 0004-2012《SM3密码杂凑算法》
- **SM4**：GM/T 0002-2012《SM4分组密码算法》
- **SM9**：GM/T 0044-2016《SM9 标识密码算法》
- **ZUC**：GM/T 0001.x-2012《祖冲之序列密码算法》，1～3

另外也包括 **OTP**：《GB/T 38556-2020 信息安全技术动态口令密码应用技术规范》。

## 安装与使用

### 获取源码

有两种途径可获取最新版的 CnPack 密码算法库。第一种途径是 CnPack 网站上下载或 gitee 上直接拉取。第二种途径是 CnPack 网站上下载或 github 上拉取下载最新的 CnVcl 源码包，在 Windows 系统中解压后运行 `cnvcl\GenCrypto.bat`，即可在 `cnvcl` 的同级目录下生成 `cncrypto` 目录，内有 CnPack 密码算法库所有源码。日常 CnPack 密码算法库也是自 CnVcl 中维护再同步的。

### 编译引入

CnPack 密码算法库不涉及 VCL/FMX 界面组件，仅是基础库的形式提供，因而可直接将 `cncrypto\Source` 目录加入 Delphi 或 Lazarus 的工程搜索路径，便可直接在工程中 `uses` 相应单元并调用。

另外 CnPack 密码算法库也提供了运行期包的形式，在任一版本 Delphi 中打开 `cncrypto\Package\CnCrypto.dpk`，即可编译成 BPL 使用。注意如果已经编译安装了 CnPack 组件包，则无需也不应编译使用 CnPack 密码算法库的包，会出现单元命名冲突。

注：如果在 Delphi 5 下编译该 BPL，需手工将 `requires` 语句中的 `vcl` 改为 `vcl50`。

## 测试用例

`cncrypto\Test` 目录下有一完整的命令行测试用例 `CryptoTest.dpr`，使用任一版本 Delphi 打开运行即可覆盖验证 CnPack 密码算法库的绝大多数功能，或用 C++Builder 5/6 打开 `Crypto.bpr`、或用 Lazarus 打开 `Crypto.lpi`，均同样可以运行。在没有 Lazarus 仅有 FPC 时，也可用 `fpc` 命令行编译 `Crypto.lpr` 以运行。尾部两个用例耗时较长以小时计，跑时需有耐心。

另外，本加解密库仅在小端 CPU 上运行测试过，大端 CPU 暂不保证支持。

## 演示例子

`cncrypto\Example` 目录下有 Delphi 和 Lazarus 两个目录，其中各个子目录下分别是 Delphi 下以及 Lazarus 下的各类算法的演示示例，可使用对应 IDE 打开以了解各类算法库的调用方式。注意例子工程本身不保证能够跨平台，一般仅能在 Win32 及 Win64 平台下运行。

## 关于我们

CnPack 开发组由互联网上热爱 Delphi/C++Builder 程序开发的一群中国程序员自愿者自发组成，其目标是开发中国人自己的真正优秀的第三方免费开源产品。

- **网站**：https://www.cnpack.org
- **邮件**：master@cnpack.org
- **微信公众号**：CnPack开发组



# CnPack Crypto Library

## Overview

The CnPack Crypto Library originates from the CnVcl Component Package, an open-source project by the CnPack Development Team. It is a pure Object Pascal-based open-source cryptographic library supporting Delphi 5 up to the latest versions of RAD Studio, C++Builder 5/6, and Lazarus/FPC. It supports 32-bit and 64-bit versions of operating systems including Windows, macOS, and Linux.

The CnPack Crypto Library aims to provide robust cryptographic algorithm support and data security. It covers common domestic and international encryption/decryption algorithms, including China's State Cryptography Administration (Commercial Cryptography) standards, such as symmetric encryption, asymmetric encryption, hash (also known as digest or checksum) functions, and numerous other fundamental and auxiliary cryptographic features.

## License

The CnPack Development Package is released as open-source software under the CnPack License Agreement, which provides legal protection. Detailed terms of the license are described in the License file. Please refer to the license document for specifics.

## Main Features

- **Symmetric Encryption**: SM4, AES, DES, 3DES, RC4, ChaCha20, TEA, ZUC
- **Asymmetric Encryption**: RSA, ECC Elliptic Curves (Weierstrass curves, including SM2), 25519/448 Elliptic Curves (Montgomery and Twisted Edwards curves)
- **Hash Functions**: SM3, MD5, SHA1, SHA2/SHA3/BLAKE series (224, 256, 384, 512), SHAKE, BLAKE2 (2S/2B), XXHash (32/64), CRC8/16/32/64, Poly1305, FNV, One-Time Signature (OTS) hash schemes
- **Other Cryptographic Areas**: SM9, Homomorphic Encryption, Collaborative Encryption, Lattice-based Cryptography, NTRU
- **Encoding/Decoding**: Base64, ASN.1 (DER/BER), Hamming Check
- **CA Certificates**: RSA/ECC Certificate Request, Issuance, and Verification
- **Big Number Support**: Big Integers, Big Rational Numbers, Big Floating-Point Numbers, Univariate and Bivariate Big Number Polynomials, Univariate Big Rational Functions
- **Mathematical Operations**: Prime Number Operations and Primality Testing, Complex Floating-Point Arithmetic, Bitwise Operations, Matrix Operations, Discrete Fourier Transform, 128-bit Integer Support
- **Number Theory Computations**: Chinese Remainder Theorem, Lucas Sequences, Pollard-Rho Factorization, Quadratic Residues Modulo Primes, Legendre and Jacobi Symbols
- **Post-Quantum Cryptography**: Module-Lattice-Based Key-Encapsulation Mechanism (MLKEM), Module-Lattice-Based Digital Signature Algorithm (MLDSA).
- **Others**: DSA Signature Verification, Authenticated Encryption with Associated Data (AEAD), One-Time Password (OTP), Key Derivation Functions (KDF), Shamir's Secret Sharing Threshold Scheme, and Feldman Verifiable Secret Sharing (VSS), Calendar.

## Support for Chinese National Cryptography Standards

The CnPack Crypto Library provides comprehensive and robust support for China's State Cryptography Administration (Commercial Cryptography) standards:

- **SM2**: GM/T 0003.x-2012 "Public Key Cryptographic Algorithm Based on Elliptic Curves - SM2", Parts 1–5
- **SM3**: GM/T 0004-2012 "Cryptographic Hash Algorithm - SM3"
- **SM4**: GM/T 0002-2012 "Block Cipher Algorithm - SM4"
- **SM9**: GM/T 0044-2016 "Identity-Based Cryptographic Algorithm - SM9"
- **ZUC**: GM/T 0001.x-2012 "Zu Chongzhi Sequence Cipher Algorithm", Parts 1–3

Also includes **OTP**: GB/T 38556-2020 "Information Security Technology - Dynamic Password Cryptographic Application Specifications".

## Installation and Usage

### Obtaining the Source Code

There are two ways to obtain the latest version of the CnPack Crypto Library. The first is downloading from the CnPack website or directly cloning from Gitee. The second is downloading from the CnPack website or pulling the latest CnVcl source package from GitHub. After extracting on Windows, run `cnvcl\GenCrypto.bat`, which will generate a `cncrypto` directory at the same level as `cnvcl`, containing all source code of the CnPack Crypto Library. The library is routinely maintained within CnVcl and then synchronized externally.

### Compilation and Integration

The CnPack Crypto Library does not involve VCL/FMX UI components and is provided purely as a foundational library. You can directly add the `cncrypto\Source` directory to your Delphi or Lazarus project's search path, then `uses` the corresponding units in your code to access the functionality.

Alternatively, the library also provides a runtime package. Open `cncrypto\Package\CnCrypto.dpk` in any version of Delphi to compile it into a BPL file for use. Note: If you have already installed the full CnPack Component Package, you should **not** compile or use the CnPack Crypto Library package, as this will cause unit naming conflicts.

> **Note**: When compiling the BPL in Delphi 5, manually change `vcl` to `vcl50` in the `requires` clause.

## Test Cases

The `cncrypto\Test` directory contains a complete command-line test suite, `CryptoTest.dpr`. Open and run it in any version of Delphi to verify the majority of the CnPack Crypto Library's functionality. It can also be opened with C++Builder 5/6 (`Crypto.bpr`) or Lazarus (`Crypto.lpi`) to run the tests. If only FPC (Free Pascal Compiler) is available without Lazarus, you may compile and run `Crypto.lpr` using the `fpc` command-line tool. The last two test cases are time-consuming and may take hours to complete. Please be patient during execution.

> **Note**: This cryptographic library has only been tested on little-endian CPUs. Support for big-endian CPUs is not currently guaranteed.

## Example Demos

The `cncrypto\Example` directory contains two subdirectories: Delphi and Lazarus. Each contains demonstration examples for various algorithms implemented in Delphi and Lazarus environments, respectively. These examples can be opened in their corresponding IDEs to learn how to use the different components of the library.

## About Us

The CnPack Development Team is a volunteer group of Chinese programmers from the internet who are passionate about Delphi/C++Builder development. Our goal is to create high-quality, open-source, free third-party tools developed by Chinese developers for the global community.

- **Website**: https://www.cnpack.org
- **Email**: master@cnpack.org
