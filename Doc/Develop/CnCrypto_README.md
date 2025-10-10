# CnPack �����㷨��

## ����

CnPack �����㷨�⣨CnPack Crypto Library��Դ�� CnPack ������Ŀ�Դ��Ŀ CnVcl �������CnVcl Component Package������һ���Դ� Object Pascal ���Ա�д�Ŀ�Դ�ӽ���Դ��⣬֧�� Delphi 5 �����°汾�� RAD Studio��֧�� C++Builder 5/6 �� Lazarus/FPC����֧�� 32 λ�� 64 λ�� Windows��MacOS��Linux �Ȳ���ϵͳ��

CnPack �����㷨���������ṩǿ��ļӽ����㷨֧�������ݰ�ȫ���ϡ��������˹����ⳣ���ļӽ����㷨���й��������������㷨�������ԳƼ��ܡ��ǶԳƼ��ܡ��Ӵգ�����ɢ�С�ժҪ����ϣ���ȴ��༰�ڶ����������븨�����ܡ�

## ���Э��

CnPack �������Կ���Դ�루Open Source������ʽ���������� CnPack �����Э�飬�� CnPack ���Э��ı�����License �ļ����и�Э�����ϸ�������������ݿɲο�Э���ļ���

## ��Ҫ����

- **�ԳƼ���**��SM4��AES��DES��3DES��RC4��ChaCha20��TEA��ZUC
- **�ǶԳƼ���**��RSA��ECC ��Բ���ߣ�κ��˹����˹���ߣ����� SM2����25519/448 ��Բ���ߣ��ɸ�������Ť�����»����ߣ�
- **�Ӵ�**��SM3��MD5��SHA1��SHA2/SHA3/BLAKE ϵ�У�224��256��384��512��SHAKE��BLAKE2��2S/2B����XXHash��32/64����CRC8/16/32/64��Poly1305��FNV��һ�����Ӵ�ǩ���㷨 OTS
- **��������ѧ����**��SM9��̬ͬ���ܡ�Эͬ���ܡ������롢NTRU
- **�������**��Base64��ASN.1��DER/BER����Hamming У��
- **CA֤��**��RSA/ECC ֤�����롢ǩ����У��
- **����֧��**���������������������󸡵�����һԪ��������ʽ����Ԫ��������ʽ��һԪ����������ʽ
- **����֧��**�������������ж������㸴�����㡢λ���㡢�������㡢��ɢ����Ҷ�任��128 λ����֧��
- **���ۼ���**���й�ʣ�ඨ��Lucas ���С�PollardRho �����ֽ⡢ģ��������ʣ�ࡢ���õ�/�ſɱȷ���
- **����**��DSA ǩ����֤������������֤���ܡ���̬���� OTP����Կ���� KDF�����ܹ���� Shamir ���޷����� Feldman VSS ����

## ����֧��

CnPack �����㷨����й��������������㷨��׼����������ǿ���֧�֣�

- **SM2**��GM/T 0003.x-2012��SM2��Բ���߹�Կ�����㷨����1��5
- **SM3**��GM/T 0004-2012��SM3�����Ӵ��㷨��
- **SM4**��GM/T 0002-2012��SM4���������㷨��
- **SM9**��GM/T 0044-2016��SM9 ��ʶ�����㷨��
- **ZUC**��GM/T 0001.x-2012�����֮���������㷨����1��3

����Ҳ���� **OTP**����GB/T 38556-2020 ��Ϣ��ȫ������̬��������Ӧ�ü����淶����

## ��װ��ʹ��

### ��ȡԴ��

������;���ɻ�ȡ���°�� CnPack �����㷨�⡣��һ��;���� CnPack ��վ�����ػ� gitee ��ֱ����ȡ���ڶ���;���� CnPack ��վ�����ػ� github ����ȡ�������µ� CnVcl Դ������� Windows ϵͳ�н�ѹ������ `cnvcl\GenCrypto.bat`�������� `cnvcl` ��ͬ��Ŀ¼������ `cncrypto` Ŀ¼������ CnPack �����㷨������Դ�롣�ճ� CnPack �����㷨��Ҳ���� CnVcl ��ά����ͬ���ġ�

### ��������

CnPack �����㷨�ⲻ�漰 VCL/FMX ������������ǻ��������ʽ�ṩ�������ֱ�ӽ� `cncrypto\Source` Ŀ¼���� Delphi �� Lazarus �Ĺ�������·�������ֱ���ڹ����� `uses` ��Ӧ��Ԫ�����á�

���� CnPack �����㷨��Ҳ�ṩ�������ڰ�����ʽ������һ�汾 Delphi �д� `cncrypto\Package\CnCrypto.dpk`�����ɱ���� BPL ʹ�á�ע������Ѿ����밲װ�� CnPack �������������Ҳ��Ӧ����ʹ�� CnPack �����㷨��İ�������ֵ�Ԫ������ͻ��

ע������� Delphi 5 �±���� BPL�����ֹ��� `requires` ����е� `vcl` ��Ϊ `vcl50`��

## ��������

`cncrypto\Test` Ŀ¼����һ�����������в������� `CryptoTest.dpr`��ʹ����һ�汾 Delphi �����м��ɸ�����֤ CnPack �����㷨��ľ���������ܣ����� C++Builder 5/6 �� `Crypto.bpr`������ Lazarus �� `Crypto.lpi`����ͬ���������С���û�� Lazarus ���� FPC ʱ��Ҳ���� `fpc` �����б��� `Crypto.lpr` �����С�β������������ʱ�ϳ���Сʱ�ƣ���ʱ�������ġ�
���⣬���ӽ��ܿ����С�� CPU �����в��Թ������ CPU �ݲ���֤֧�֡�

## ��ʾ����

`cncrypto\Example` Ŀ¼���� Delphi �� Lazarus ����Ŀ¼�����и�����Ŀ¼�·ֱ��� Delphi ���Լ� Lazarus �µĸ����㷨����ʾʾ������ʹ�ö�Ӧ IDE �����˽�����㷨��ĵ��÷�ʽ��

## ��������

CnPack �������ɻ��������Ȱ� Delphi/C++Builder ���򿪷���һȺ�й�����Ա��Ը���Է���ɣ���Ŀ���ǿ����й����Լ�����������ĵ�������ѿ�Դ��Ʒ��

- **��վ**��https://www.cnpack.org
- **�ʼ�**��master@cnpack.org
- **΢�Ź��ں�**��CnPack������



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
- **Others**: DSA Signature Verification, Authenticated Encryption with Associated Data (AEAD), One-Time Password (OTP), Key Derivation Functions (KDF), Shamir's Secret Sharing Threshold Scheme, and Feldman Verifiable Secret Sharing (VSS)

## Support for Chinese National Cryptography Standards

The CnPack Crypto Library provides comprehensive and robust support for China's State Cryptography Administration (Commercial Cryptography) standards:

- **SM2**: GM/T 0003.x-2012 "Public Key Cryptographic Algorithm Based on Elliptic Curves - SM2", Parts 1�C5
- **SM3**: GM/T 0004-2012 "Cryptographic Hash Algorithm - SM3"
- **SM4**: GM/T 0002-2012 "Block Cipher Algorithm - SM4"
- **SM9**: GM/T 0044-2016 "Identity-Based Cryptographic Algorithm - SM9"
- **ZUC**: GM/T 0001.x-2012 "Zu Chongzhi Sequence Cipher Algorithm", Parts 1�C3

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
