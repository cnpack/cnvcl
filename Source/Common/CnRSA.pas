{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2018 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnRSA;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：RSA 算法单元
* 单元作者：刘啸
* 备    注：包括 Int64 范围内的 RSA 算法以及大数算法，公钥 Exponent 固定使用 65537。
* 开发平台：WinXP + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2018.05.27 V1.3
*               能够从 Openssl 1.0.2 生成的未加密的公私钥 PEM 格式文件中读入公私钥，如
*               openssl genrsa -out private_pkcs1.pem 2048
*                  // PKCS#1 格式的公私钥
*               openssl pkcs8 -topk8 -inform PEM -in private_pkcs1.pem -outform PEM -nocrypt -out private_pkcs8.pem
*                  // PKCS#8 格式的公私钥
*               openssl rsa -in private_pkcs1.pem -outform PEM -RSAPublicKey_out -out public_pkcs1.pem
*                  // PKCS#1 格式的公钥
*               openssl rsa -in private_pkcs1.pem -outform PEM -pubout -out public_pkcs8.pem
*                  // PKCS#8 格式的公钥
*           2018.05.22 V1.2
*               将公私钥组合成对象以方便使用
*           2017.04.05 V1.1
*               实现大数的 RSA 密钥生成与加解密
*           2017.04.03 V1.0
*               创建单元，Int64 范围内的 RSA 从 CnPrimeNumber 中独立出来
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, CnPrimeNumber, CnBigNumber, CnBase64, CnBerUtils;

type
  TCnRSAKeyType = (cktPKCS1, cktPKCS8);

  TCnRSAPrivateKey = class
  private
    FPrimeKey1: TCnBigNumber;
    FPrimeKey2: TCnBigNumber;
    FPrivKeyProduct: TCnBigNumber;
    FPrivKeyExponent: TCnBigNumber;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    property PrimeKey1: TCnBigNumber read FPrimeKey1 write FPrimeKey1;
    {* 大素数 1，p}
    property PrimeKey2: TCnBigNumber read FPrimeKey2 write FPrimeKey2;
    {* 大素数 2，q}
    property PrivKeyProduct: TCnBigNumber read FPrivKeyProduct write FPrivKeyProduct;
    {* 俩素数乘积 n，也叫 Modulus}
    property PrivKeyExponent: TCnBigNumber read FPrivKeyExponent write FPrivKeyProduct;
    {* 私钥指数 d}
  end;

  TCnRSAPublicKey = class
  private
    FPubKeyProduct: TCnBigNumber;
    FPubKeyExponent: TCnBigNumber;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    property PubKeyProduct: TCnBigNumber read FPubKeyProduct write FPubKeyProduct;
    {* 俩素数乘积 n，也叫 Modulus}
    property PubKeyExponent: TCnBigNumber read FPubKeyExponent write FPubKeyExponent;
    {* 公钥指数 e，65537}
  end;

// Int64 范围内的 RSA 加解密实现

function Int64ExtendedEuclideanGcd(A, B: Int64; out X: Int64; out Y: Int64): Int64;
{* 扩展欧几里得辗转相除法求二元一次不定方程 A * X + B * Y = 1 的整数解}

function CnInt64RSAGenerateKeys(out PrimeKey1: Integer; out PrimeKey2: Integer;
  out PrivKeyProduct: Int64; out PrivKeyExponent: Int64;
  out PubKeyProduct: Int64; out PubKeyExponent: Int64): Boolean;
{* 生成 RSA 算法所需的公私钥，素数均不大于 Integer，Keys 均不大于 Int64}

function CnInt64RSAEncrypt(Data: Int64; PrivKeyProduct: Int64;
  PrivKeyExponent: Int64; out Res: Int64): Boolean;
{* 利用上面生成的私钥对数据进行加密，返回加密是否成功}

function CnInt64RSADecrypt(Res: Int64; PubKeyProduct: Int64;
  PubKeyExponent: Int64; out Data: Int64): Boolean;
{* 利用上面生成的公钥对数据进行解密，返回解密是否成功}

// 大数范围内的 RSA 加解密实现

function CnRSAGenerateKeys(Bits: Integer; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey): Boolean;
{* 生成 RSA 算法所需的公私钥，Bits 是素数范围，其余参数均为生成}

function CnRSALoadKeysFromPem(const PemFileName: string;
  PrivateKey: TCnRSAPrivateKey; PublicKey: TCnRSAPublicKey): Boolean;
{* 从 PEM 格式文件中加载公私钥数据，如某钥参数为空则不载入}

function CnRSASaveKeysToPem(const PemFileName: string; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType = cktPKCS1): Boolean;
{* 将公私钥写入 PEM 格式文件中，返回是否成功}

function CnRSALoadPublicKeyFromPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean;
{* 从 PEM 格式文件中加载公钥数据，返回是否成功}

function CnRSASavePublicKeyToPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType = cktPKCS8): BOolean;
{* 将公钥写入 PEM 格式文件中，返回是否成功}

function CnRSAEncrypt(Data: TCnBigNumber; PrivateKey: TCnRSAPrivateKey;
  Res: TCnBigNumber): Boolean;
{* 利用上面生成的私钥对数据进行加密，返回加密是否成功}

function CnRSADecrypt(Res: TCnBigNumber; PublicKey: TCnRSAPublicKey;
  Data: TCnBigNumber): Boolean;
{* 利用上面生成的公钥对数据进行解密，返回解密是否成功}

implementation

const
  // PKCS#1
  PEM_RSA_PRIVATE_HEAD = '-----BEGIN RSA PRIVATE KEY-----';  // 已解析
  PEM_RSA_PRIVATE_TAIL = '-----END RSA PRIVATE KEY-----';

  PEM_RSA_PUBLIC_HEAD = '-----BEGIN RSA PUBLIC KEY-----';    // 已解析
  PEM_RSA_PUBLIC_TAIL = '-----END RSA PUBLIC KEY-----';

  // PKCS#8
  PEM_PRIVATE_HEAD = '-----BEGIN PRIVATE KEY-----';          // 已解析
  PEM_PRIVATE_TAIL = '-----END PRIVATE KEY-----';

  PEM_PUBLIC_HEAD = '-----BEGIN PUBLIC KEY-----';            // 已解析
  PEM_PUBLIC_TAIL = '-----END PUBLIC KEY-----';

  // OID 预先写死，不动态计算编码了
  OID_RSAENCRYPTION_PKCS1: array[0..8] of Byte = ( // 1.2.840.113549.1.1.1
    $2A, $86, $48, $86, $F7, $0D, $01, $01, $01
  );  // $2A = 40 * 1 + 2


// 利用公私钥对数据进行加解密，注意加解密使用的是同一套机制，无需区分
function Int64RSACrypt(Data: Int64; Product: Int64; Exponent: Int64;
  out Res: Int64): Boolean;
begin
  Res := MontgomeryPowerMod(Data, Exponent, Product);
  Result := True;
end;

// 扩展欧几里得辗转相除法求二元一次不定方程 A * X + B * Y = 1 的整数解
function Int64ExtendedEuclideanGcd(A, B: Int64; out X: Int64; out Y: Int64): Int64;
var
  R, T: Int64;
begin
  if B = 0 then
  begin
    X := 1;
    Y := 0;
    Result := A;
  end
  else
  begin
    R := Int64ExtendedEuclideanGcd(B, A mod B, X, Y);
    T := X;
    X := Y;
    Y := T - (A div B) * Y;
    Result := R;
  end;
end;

// 生成 RSA 算法所需的公私钥，素数均不大于 Integer，Keys 均不大于 Int64
function CnInt64RSAGenerateKeys(out PrimeKey1: Integer; out PrimeKey2: Integer;
  out PrivKeyProduct: Int64; out PrivKeyExponent: Int64;
  out PubKeyProduct: Int64; out PubKeyExponent: Int64): Boolean;
var
  N: Integer;
  Product, Y: Int64;
begin
  PrimeKey1 := CnGenerateInt32Prime;

  N := Trunc(Random * 1000);
  Sleep(N);

  PrimeKey2 := CnGenerateInt32Prime;
  if PrimeKey2 > PrimeKey1 then  // 一般使 p > q
  begin
    N := PrimeKey1;
    PrimeKey1 := PrimeKey2;
    PrimeKey2 := N;
  end;

  PrivKeyProduct := Int64(PrimeKey1) * Int64(PrimeKey2);
  PubKeyProduct := Int64(PrimeKey2) * Int64(PrimeKey1);   // 积在公私钥中是相同的
  PubKeyExponent := 65537;                                // 固定

  Product := Int64(PrimeKey1 - 1) * Int64(PrimeKey2 - 1);

  //                      e                d                p
  // 用辗转相除法求 PubKeyExponent * PrivKeyExponent mod Product = 1 中的 PrivKeyExponent
  // 也就是解方程 e * d + p * y = 1，其中 e、p 已知，求 d 与 y。
  Int64ExtendedEuclideanGcd(PubKeyExponent, Product, PrivKeyExponent, Y);
  while PrivKeyExponent < 0 do
  begin
     // 如果求出来的 d 小于 0，则不符合条件，需要将 d 加上 p，加到大于零为止
     PrivKeyExponent := PrivKeyExponent + Product;
  end;
  Result := True;
end;

// 利用上面生成的私钥对数据进行加密，返回加密是否成功
function CnInt64RSAEncrypt(Data: Int64; PrivKeyProduct: Int64;
  PrivKeyExponent: Int64; out Res: Int64): Boolean;
begin
  Result := Int64RSACrypt(Data, PrivKeyProduct, PrivKeyExponent, Res);
end;

// 利用上面生成的公钥对数据进行解密，返回解密是否成功
function CnInt64RSADecrypt(Res: Int64; PubKeyProduct: Int64;
  PubKeyExponent: Int64; out Data: Int64): Boolean;
begin
  Result := Int64RSACrypt(Res, PubKeyProduct, PubKeyExponent, Data);
end;

function CnRSAGenerateKeys(Bits: Integer; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey): Boolean;
var
  N: Integer;
  R, Y, Rem, S1, S2, One: TCnBigNumber;
begin
  Result := False;
  if Bits <= 16 then
    Exit;

  PrivateKey.Clear;
  PublicKey.Clear;

  if not BigNumberGeneratePrime(PrivateKey.PrimeKey1, Bits div 8) then
    Exit;

  N := Trunc(Random * 1000);
  Sleep(N);

  if not BigNumberGeneratePrime(PrivateKey.PrimeKey2, Bits div 8) then
    Exit;

  // 一般要求 Prime1 > Prime2 以便计算 CRT 等参数
  if BigNumberCompare(PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) < 0 then
    BigNumberSwap(PrivateKey.PrimeKey1, PrivateKey.PrimeKey2);

  if not BigNumberMul(PrivateKey.PrivKeyProduct, PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) then
    Exit;

  if not BigNumberMul(PublicKey.PubKeyProduct, PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) then
    Exit;

  PublicKey.PubKeyExponent.SetDec('65537');

  Rem := nil;
  Y := nil;
  R := nil;
  S1 := nil;
  S2 := nil;
  One := nil;

  try
    Rem := TCnBigNumber.Create;
    Y := TCnBigNumber.Create;
    R := TCnBigNumber.Create;
    S1 := TCnBigNumber.Create;
    S2 := TCnBigNumber.Create;
    One := TCnBigNumber.Create;

    BigNumberSetOne(One);
    BigNumberSub(S1, PrivateKey.PrimeKey1, One);
    BigNumberSub(S2, PrivateKey.PrimeKey2, One);
    BigNumberMul(R, S1, S2);     // 计算积二，R = (p - 1) * (q - 1)

    // 求 e 也就是 PubKeyExponent（65537）针对积二 R 的模反元素 d 也就是 PrivKeyExponent
    BigNumberExtendedEuclideanGcd(PublicKey.PubKeyExponent, R, PrivateKey.PrivKeyExponent, Y, Rem);

    // 如果求出来的 d 小于 0，则不符合条件，需要将 d 加上积二 R
    if BigNumberIsNegative(PrivateKey.PrivKeyExponent) then
       BigNumberAdd(PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyExponent, R);
  finally
    One.Free;
    S2.Free;
    S1.Free;
    R.Free;
    Y.Free;
    Rem.Free;
  end;

  Result := True;
end;

function LoadPemFileToMemory(const FileName, ExpectHead, ExpectTail: string;
  MemoryStream: TMemoryStream): Boolean;
var
  I: Integer;
  S: string;
  Sl: TStringList;
begin
  Result := False;

  if (ExpectHead <> '') and (ExpectTail <> '') then
  begin
    Sl := TStringList.Create;
    try
      Sl.LoadFromFile(FileName);
      if Sl.Count > 2 then
      begin
        if Trim(Sl[0]) <> ExpectHead then
          Exit;

        if Trim(Sl[Sl.Count - 1]) = '' then
          Sl.Delete(Sl.Count - 1);

        if Trim(Sl[Sl.Count - 1]) <> ExpectTail then
          Exit;

        Sl.Delete(Sl.Count - 1);
        Sl.Delete(0);
        S := '';
        for I := 0 to Sl.Count - 1 do
          S := S + Sl[I];

        // To De Base64 S
        MemoryStream.Clear;
        Result := (BASE64_OK = Base64Decode(S, MemoryStream));
      end;
    finally
      Sl.Free;
    end;
  end;
end;

procedure PutIndexedBigIntegerToBigInt(Node: TCnBerReadNode; BigNumber: TCnBigNumber);
var
  P: Pointer;
begin
  if (Node = nil) or (Node.BerDataLength <= 0) then
    Exit;

  P := GetMemory(Node.BerDataLength);
  Node.CopyDataTo(P);
  BigNumber.SetBinary(P, Node.BerDataLength);
  FreeMemory(P);
end;

// 从 PEM 格式文件中加载公私钥数据
(*
PKCS#1:
  RSAPrivateKey ::= SEQUENCE {                        0
    version Version,                                  1 0
    modulus INTEGER, – n                             2 公私钥
    publicExponent INTEGER, – e                      3 公钥
    privateExponent INTEGER, – d                     4 私钥
    prime1 INTEGER, – p                              5 私钥
    prime2 INTEGER, – q                              6 私钥
    exponent1 INTEGER, – d mod (p-1)                 7 CRT 系数 1
    exponent2 INTEGER, – d mod (q-1)                 8 CRT 系数 2
    coefficient INTEGER, – (inverse of q) mod p      9 CRT 系数 3：q 针对 n 的模反系数 mod p
    otherPrimeInfos OtherPrimeInfos OPTIONAL          10

    q 针对 n 的模反系数 x，是指满足 (q * x) mod n = 1 的正值 x，
    可以用扩展欧几里得辗转相除法直接求解，CRT 系数 3 即 x mod p 的值
  }

PKCS#8:
  PrivateKeyInfo ::= SEQUENCE {
    version         Version,
    algorithm       AlgorithmIdentifier,
    PrivateKey      OCTET STRING
  }

  AlgorithmIdentifier ::= SEQUENCE {
    algorithm       OBJECT IDENTIFIER,
    parameters      ANY DEFINED BY algorithm OPTIONAL
  }
  PrivateKey 是上面 PKCS#1 的 RSAPrivateKey 结构
  也即：
  SEQUENCE (3 elem)
    INTEGER 0
    SEQUENCE (2 elem)
      OBJECT IDENTIFIER 1.2.840.113549.1.1.1 rsaEncryption(PKCS #1)
      NULL
    OCTET STRING (1 elem)
      SEQUENCE (9 elem)
        INTEGER 0
        INTEGER                                       8 公私钥 Modulus
        INTEGER                                       9 公钥   e
        INTEGER                                       10 私钥  d
        INTEGER                                       11 私钥  p
        INTEGER                                       12 私钥  q
        INTEGER
        INTEGER
        INTEGER
*)
function CnRSALoadKeysFromPem(const PemFileName: string;
  PrivateKey: TCnRSAPrivateKey; PublicKey: TCnRSAPublicKey): Boolean;
var
  MemStream: TMemoryStream;
  Ber: TCnBerReader;
  Node: TCnBerReadNode;
begin
  Result := False;
  MemStream := nil;
  Ber := nil;

  try
    MemStream := TMemoryStream.Create;
    if LoadPemFileToMemory(PemFileName, PEM_RSA_PRIVATE_HEAD, PEM_RSA_PRIVATE_TAIL, MemStream) then
    begin
      // 读 PKCS#1 的 PEM 公私钥格式
      Ber := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size);
      if Ber.TotalCount >= 8 then
      begin
        Node := Ber.Items[1]; // 0 是整个 Sequence，1 是 Version
        if Node.AsByte = 0 then // 只支持版本 0
        begin
          // 2 和 3 整成公钥
          if PublicKey <> nil then
          begin
            PutIndexedBigIntegerToBigInt(Ber.Items[2], PublicKey.PubKeyProduct);
            PutIndexedBigIntegerToBigInt(Ber.Items[3], PublicKey.PubKeyExponent);
          end;

          // 2 4 5 6 整成私钥
          if PrivateKey <> nil then
          begin
            PutIndexedBigIntegerToBigInt(Ber.Items[2], PrivateKey.PrivKeyProduct);
            PutIndexedBigIntegerToBigInt(Ber.Items[4], PrivateKey.PrivKeyExponent);
            PutIndexedBigIntegerToBigInt(Ber.Items[5], PrivateKey.PrimeKey1);
            PutIndexedBigIntegerToBigInt(Ber.Items[6], PrivateKey.PrimeKey2);
          end;

          Result := True;
        end;
      end;
    end
    else if LoadPemFileToMemory(PemFileName, PEM_PRIVATE_HEAD, PEM_PRIVATE_TAIL, MemStream) then
    begin
      // 读 PKCS#8 的 PEM 公私钥格式
      Ber := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size, True);
      if Ber.TotalCount >= 12 then
      begin
        Node := Ber.Items[1]; // 0 是整个 Sequence，1 是 Version
        if Node.AsByte = 0 then // 只支持版本 0
        begin
          // 8 和 9 整成公钥
          if PublicKey <> nil then
          begin
            PutIndexedBigIntegerToBigInt(Ber.Items[8], PublicKey.PubKeyProduct);
            PutIndexedBigIntegerToBigInt(Ber.Items[9], PublicKey.PubKeyExponent);
          end;
      
          // 8 10 11 12 整成私钥
          if PrivateKey <> nil then
          begin
            PutIndexedBigIntegerToBigInt(Ber.Items[8], PrivateKey.PrivKeyProduct);
            PutIndexedBigIntegerToBigInt(Ber.Items[10], PrivateKey.PrivKeyExponent);
            PutIndexedBigIntegerToBigInt(Ber.Items[11], PrivateKey.PrimeKey1);
            PutIndexedBigIntegerToBigInt(Ber.Items[12], PrivateKey.PrimeKey2);
          end;
      
          Result := True;
        end;
      end;
    end;
  finally
    MemStream.Free;
    Ber.Free;
  end;
end;

// 从 PEM 格式文件中加载公钥数据
// 注意 PKCS#8 的 PublicKey 的 PEM 在标准 ASN.1 上做了一层封装，
// 把 Modulus 与 Exponent 封在了 BitString 中，需要 Paser 解析出来
(*
PKCS#1:
  RSAPublicKey ::= SEQUENCE {
      modulus           INTEGER,  -- n
      publicExponent    INTEGER   -- e
  }

PKCS#8:
  PublicKeyInfo ::= SEQUENCE {
    algorithm       AlgorithmIdentifier,
    PublicKey       BIT STRING
  }

  AlgorithmIdentifier ::= SEQUENCE {
    algorithm       OBJECT IDENTIFIER,
    parameters      ANY DEFINED BY algorithm OPTIONAL
  }
  也即：
  SEQUENCE (2 elem)
    SEQUENCE (2 elem)
      OBJECT IDENTIFIER 1.2.840.113549.1.1.1 rsaEncryption(PKCS #1)
      NULL
    BIT STRING (1 elem)
      SEQUENCE (2 elem)
        INTEGER     - Modulus
        INTEGER     - Exponent
*)
function CnRSALoadPublicKeyFromPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean;
var
  Mem: TMemoryStream;
  Ber: TCnBerReader;
begin
  Result := False;
  Mem := nil;
  Ber := nil;

  try
    Mem := TMemoryStream.Create;
    if LoadPemFileToMemory(PemFileName, PEM_PUBLIC_HEAD, PEM_PUBLIC_TAIL, Mem) then
    begin
      // 读 PKCS#8 格式的公钥
      Ber := TCnBerReader.Create(PByte(Mem.Memory), Mem.Size, True);
      if Ber.TotalCount >= 7 then
      begin
        // 6 和 7 整成公钥
        if PublicKey <> nil then
        begin
          PutIndexedBigIntegerToBigInt(Ber.Items[6], PublicKey.PubKeyProduct);
          PutIndexedBigIntegerToBigInt(Ber.Items[7], PublicKey.PubKeyExponent);
        end;

        Result := True;
      end;
    end
    else if LoadPemFileToMemory(PemFileName, PEM_RSA_PUBLIC_HEAD, PEM_RSA_PUBLIC_TAIL, Mem) then
    begin
      // 读 PKCS#1 格式的公钥
      Ber := TCnBerReader.Create(PByte(Mem.Memory), Mem.Size);
      if Ber.TotalCount >= 3 then
      begin
        // 1 和 2 整成公钥
        if PublicKey <> nil then
        begin
          PutIndexedBigIntegerToBigInt(Ber.Items[1], PublicKey.PubKeyProduct);
          PutIndexedBigIntegerToBigInt(Ber.Items[2], PublicKey.PubKeyExponent);
        end;
      
        Result := True;
      end;
    end;
  finally
    Mem.Free;
    Ber.Free;
  end;
end;

// 如果整数最高位是 1，则需要前面补 0 避免与负数的表述混淆
function CalcIntegerTLV(BigNumber: TCnBigNumber): Cardinal;
begin
  Result := BigNumber.GetBytesCount;
  if BigNumber.IsBitSet((Result * 8) - 1) then
    Inc(Result);
end;

procedure SplitStringToList(const S: string; List: TStrings);
const
  LINE_WIDTH = 64;
var
  C, R: string;
begin
  if List = nil then
    Exit;

  List.Clear;
  if S <> '' then
  begin
    R := S;
    while R <> '' do
    begin
      C := Copy(R, 1, LINE_WIDTH);
      Delete(R, 1, LINE_WIDTH);
      List.Add(C);
    end;
  end;
end;

function AddBigNumberToWriter(Writer: TCnBerWriter; Num: TCnBigNumber;
  Parent: TCnBerWriteNode): TCnBerWriteNode;
var
  P: Pointer;
  C, D: Integer;
begin
  Result := nil;
  if (Writer = nil) or (Num = nil) then
    Exit;

  // Integer 编码需要处理最高位
  C := CalcIntegerTLV(Num);
  if C <= 0 then
    Exit;

  P := GetMemory(C);
  D := C - Num.GetBytesCount;
  ZeroMemory(P, D);
  Num.ToBinary(PAnsiChar(Integer(P) + D));

  Result := Writer.AddBasicNode(CN_BER_TAG_INTEGER, P, C, Parent);
  FreeMemory(P);
end;

// 将公私钥写入 PEM 格式文件中
function CnRSASaveKeysToPem(const PemFileName: string; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType): Boolean;
var
  Root, Node: TCnBerWriteNode;
  Writer: TCnBerWriter;
  Mem: TMemoryStream;
  List: TStrings;
  N1, N2, T, R1, R2, Co, X, Y, Rem : TCnBigNumber;
  S: string;
  B: Byte;
begin
  Result := False;
  if (PublicKey = nil) or (PublicKey.PubKeyProduct.GetBytesCount <= 0) or
    (PublicKey.PubKeyExponent.GetBytesCount <= 0) then
    Exit;

  if (PrivateKey = nil) or (PrivateKey.PrivKeyProduct.GetBytesCount <= 0) or
    (PrivateKey.PrivKeyExponent.GetBytesCount <= 0) then
    Exit;

  Mem := nil;
  List := nil;
  Writer := nil;
  T := nil;
  R1 := nil;
  R2 := nil;
  N1 := nil;
  N2 := nil;
  X := nil;
  Y := nil;
  Rem := nil;
  Co := nil;

  try
    T := BigNumberNew;
    R1 := BigNumberNew;
    R2 := BigNumberNew;
    N1 := BigNumberNew;
    N2 := BigNumberNew;
    X := BigNumberNew;
    Y := BigNumberNew;
    Rem := BigNumberNew;
    Co := BigNumberNew;
    if not T.SetOne then
      Exit;

    BigNumberSub(N1, PrivateKey.PrimeKey1, T);
    BigNumberMod(R1, PrivateKey.PrivKeyExponent, N1); // R1 = d mod (p - 1)

    BigNumberSub(N2, PrivateKey.PrimeKey2, T);
    BigNumberMod(R2, PrivateKey.PrivKeyExponent, N2); // R2 = d mod (q - 1)

    // Co = (q 针对 n 的模反元素) mod p
    BigNumberExtendedEuclideanGcd(PrivateKey.PrimeKey2, PrivateKey.PrimeKey1,
      X, Y, Rem);
    if BigNumberIsNegative(X) then
      BigNumberAdd(X, X, PrivateKey.PrivKeyExponent); // 求得模反元素 X。FIXME: 但 X 有问题

    BigNumberMod(Co, X, PrivateKey.PrimeKey1);        // 求得 CRT 系数3

    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    B := 0;
    if KeyType = cktPKCS1 then
    begin
      // 拼 PKCS1 格式的内容
      Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, Root);
      AddBigNumberToWriter(Writer, PrivateKey.PrivKeyProduct, Root);
      AddBigNumberToWriter(Writer, PublicKey.PubKeyExponent, Root);
      AddBigNumberToWriter(Writer, PrivateKey.PrivKeyExponent, Root);
      AddBigNumberToWriter(Writer, PrivateKey.PrimeKey1, Root);
      AddBigNumberToWriter(Writer, PrivateKey.PrimeKey2, Root);
      AddBigNumberToWriter(Writer, R1, Root);
      AddBigNumberToWriter(Writer, R2, Root);
      AddBigNumberToWriter(Writer, Co, Root);
    end
    else if KeyType = cktPKCS8 then
    begin
      // 拼 PKCS8 格式的内容
      Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, Root);
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);

      // 给 Node1 加 ObjectIdentifier 与 Null
      Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_RSAENCRYPTION_PKCS1[0],
        SizeOf(OID_RSAENCRYPTION_PKCS1), Node);
      Writer.AddNullNode(Node);

      Node := Writer.AddContainerNode(CN_BER_TAG_OCTET_STRING, Root);
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Node);

      Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, Node);
      AddBigNumberToWriter(Writer, PrivateKey.PrivKeyProduct, Node);
      AddBigNumberToWriter(Writer, PublicKey.PubKeyExponent, Node);
      AddBigNumberToWriter(Writer, PrivateKey.PrivKeyExponent, Node);
      AddBigNumberToWriter(Writer, PrivateKey.PrimeKey1, Node);
      AddBigNumberToWriter(Writer, PrivateKey.PrimeKey2, Node);
      AddBigNumberToWriter(Writer, R1, Node);
      AddBigNumberToWriter(Writer, R2, Node);
      AddBigNumberToWriter(Writer, Co, Node);
    end;

    // 树搭好了，输出并 Base64 再分段再拼头尾最后写文件
    Mem := TMemoryStream.Create;
    Writer.SaveToStream(Mem);
    Mem.Position := 0;

    if Base64_OK = Base64Encode(Mem, S) then
    begin
      List := TStringList.Create;
      SplitStringToList(S, List);
      if KeyType = cktPKCS1 then
      begin
        List.Insert(0, PEM_RSA_PRIVATE_HEAD);
        List.Add(PEM_RSA_PRIVATE_TAIL);
      end
      else if KeyType = cktPKCS8 then
      begin
        List.Insert(0, PEM_PRIVATE_HEAD);
        List.Add(PEM_PRIVATE_TAIL);
      end;
      List.SaveToFile(PemFileName);
      Result := True;
    end;
  finally
    BigNumberFree(T);
    BigNumberFree(R1);
    BigNumberFree(R2);
    BigNumberFree(N1);
    BigNumberFree(N2);
    BigNumberFree(X);
    BigNumberFree(Y);
    BigNumberFree(Rem);
    BigNumberFree(Co);

    Mem.Free;
    List.Free;
    Writer.Free;
  end;
end;

// 将公钥写入 PEM 格式文件中
function CnRSASavePublicKeyToPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType): Boolean;
var
  Root, Node: TCnBerWriteNode;
  Writer: TCnBerWriter;
  Mem: TMemoryStream;
  List: TStrings;
  S: string;
begin
  Result := False;
  if (PublicKey = nil) or (PublicKey.PubKeyProduct.GetBytesCount <= 0) or
    (PublicKey.PubKeyExponent.GetBytesCount <= 0) then
    Exit;

  Mem := nil;
  List := nil;
  Writer := nil;
  try
    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    if KeyType = cktPKCS1 then
    begin
      // 拼 PKCS1 格式的内容，比较简单
      AddBigNumberToWriter(Writer, PublicKey.PubKeyProduct, Root);
      AddBigNumberToWriter(Writer, PublicKey.PubKeyExponent, Root);
    end
    else if KeyType = cktPKCS8 then
    begin
      // 拼 PKCS8 格式的内容
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);

      // 给 Node 加 ObjectIdentifier 与 Null
      Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_RSAENCRYPTION_PKCS1[0],
        SizeOf(OID_RSAENCRYPTION_PKCS1), Node);
      Writer.AddNullNode(Node);

      Node := Writer.AddContainerNode(CN_BER_TAG_BIT_STRING, Root);
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Node);
      AddBigNumberToWriter(Writer, PublicKey.PubKeyProduct, Node);
      AddBigNumberToWriter(Writer, PublicKey.PubKeyExponent, Node);
    end;

    // 树搭好了，输出并 Base64 再分段再拼头尾最后写文件
    Mem := TMemoryStream.Create;
    Writer.SaveToStream(Mem);
    Mem.Position := 0;

    if Base64_OK = Base64Encode(Mem, S) then
    begin
      List := TStringList.Create;
      SplitStringToList(S, List);
      if KeyType = cktPKCS1 then
      begin
        List.Insert(0, PEM_RSA_PUBLIC_HEAD);
        List.Add(PEM_RSA_PUBLIC_TAIL);
      end
      else if KeyType = cktPKCS8 then
      begin
        List.Insert(0, PEM_PUBLIC_HEAD);
        List.Add(PEM_PUBLIC_TAIL);
      end;
      List.SaveToFile(PemFileName);
      Result := True;
    end;
  finally
    Mem.Free;
    List.Free;
    Writer.Free;
  end;
end;

// 利用公私钥对数据进行加解密，注意加解密使用的是同一套机制，无需区分
function RSACrypt(Data: TCnBigNumber; Product: TCnBigNumber; Exponent: TCnBigNumber;
  out Res: TCnBigNumber): Boolean;
begin
  Result := BigNumberMontgomeryPowerMod(Res, Data, Exponent, Product);
end;

// 利用上面生成的私钥对数据进行加密，返回加密是否成功
function CnRSAEncrypt(Data: TCnBigNumber; PrivateKey: TCnRSAPrivateKey;
  Res: TCnBigNumber): Boolean;
begin
  Result := RSACrypt(Data, PrivateKey.PrivKeyProduct, PrivateKey.PrivKeyExponent, Res);
end;

// 利用上面生成的公钥对数据进行解密，返回解密是否成功
function CnRSADecrypt(Res: TCnBigNumber; PublicKey: TCnRSAPublicKey;
  Data: TCnBigNumber): Boolean;
begin
  Result := RSACrypt(Res, PublicKey.PubKeyProduct, PublicKey.PubKeyExponent, Data);
end;

{ TCnRSAPrivateKey }

procedure TCnRSAPrivateKey.Clear;
begin
  FPrimeKey1.Clear;
  FPrimeKey2.Clear;
  FPrivKeyProduct.Clear;
  FPrivKeyExponent.Clear;
end;

constructor TCnRSAPrivateKey.Create;
begin
  FPrimeKey1 := TCnBigNumber.Create;
  FPrimeKey2 := TCnBigNumber.Create;
  FPrivKeyProduct := TCnBigNumber.Create;
  FPrivKeyExponent := TCnBigNumber.Create;
end;

destructor TCnRSAPrivateKey.Destroy;
begin
  FPrimeKey1.Free;
  FPrimeKey2.Free;
  FPrivKeyProduct.Free;
  FPrivKeyExponent.Free;
  inherited;
end;

{ TCnRSAPublicKey }

procedure TCnRSAPublicKey.Clear;
begin
  FPubKeyProduct.Clear;
  FPubKeyExponent.Clear;
end;

constructor TCnRSAPublicKey.Create;
begin
  FPubKeyProduct := TCnBigNumber.Create;
  FPubKeyExponent := TCnBigNumber.Create;
end;

destructor TCnRSAPublicKey.Destroy;
begin
  FPubKeyExponent.Free;
  FPubKeyProduct.Free;
  inherited;
end;

end.
