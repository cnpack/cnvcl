{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2022 CnPack 开发组                       }
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

unit CnCertificateAuthority;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：基于 RSA 与 ECC 的 CA 证书认证单元
* 单元作者：刘啸
* 备    注：生成客户端 CSR 文件做证书签名请求，类似于命令：
*               openssl req -new -key clientkey.pem -out client.csr -config /c/Program\ Files/Git/ssl/openssl.cnf
*               其中 clientkey.pem 是预先生成的 RSA 或 ECC 私钥
*           一次性生成自签名的 crt 证书：
*               openssl req -new -x509 -keyout ca.key -out ca.crt -config /c/Program\ Files/Git/ssl/openssl.cnf
*           或利用现有 Key 对此 Key 生成的 CSR 请求文件进行自签名：
*               openssl x509 -req -days 365 -in client.csr -signkey clientkey.pem -out selfsigned.crt
*           或利用 openssl ca 命令，用根私钥与根证书签发其他的 CSR 生成 CRT 证书
*
*           证书 CRT 文件解析字段说明，散列算法以 sha256 为例：
*                    RSA 签 RSA                RSA 签 ECC                ECC 签 RSA           ECC 签 ECC
* 靠近签发者的类型： sha256WithRSAEncryption   sha256WithRSAEncryption   ecdsaWithSHA256      ecdsaWithSHA256
* 被签发者的类型：   rsaEncryption             ecPublicKey + 曲线类型    rsaEncryption        ecPublicKey + 曲线类型
* 最下面的总类型：   sha256WithRSAEncryption   sha256WithRSAEncryption   ecdsaWithSHA256      ecdsaWithSHA256
*           注意：签发者类型和总类型俩字段总是相同的，被签发者的类型不包括散列算法
*
*           逐级验证证书时，是拿父证书里的被签发者公钥来验证子证书的内容的 Hash 是否与子证书的签名内容是否对得上号
* 开发平台：WinXP + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2021.12.09 V1.5
*               加入 SM2/SM3 证书类型的解析支持
*           2020.04.17 V1.4
*               支持 ECC/RSA 证书父子校验
*           2020.04.08 V1.3
*               支持 ECC 证书请求、自签名、自签名校验与证书签发等
*           2019.05.06 V1.2
*               支持 Win32/Win64/MacOS
*           2018.07.22 V1.1
*               初步完成签发证书的功能
*           2018.06.15 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, TypInfo, {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  CnBigNumber, CnRSA, CnECC, CnBerUtils, CnPemUtils, CnMD5, CnSHA1, CnSHA2;

const
  CN_CRT_BASIC_VERSION_1      = 0;
  CN_CRT_BASIC_VERSION_2      = 1;
  CN_CRT_BASIC_VERSION_3      = 2;

type
  TCnCASignType = (ctMd5RSA, ctSha1RSA, ctSha256RSA, ctMd5Ecc, ctSha1Ecc,
    ctSha256Ecc, ctSM2withSM3);
  {* 证书签名使用的散列签名算法，ctSha1RSA 表示先 Sha1 再 RSA，但 ctSM2withSM3 表示先 SM3 再 SM2}
  TCnCASignTypes = set of TCnCASignType;

  TCnCertificateBaseInfo = class(TPersistent)
  {* 描述证书中包含的普通字段信息}
  private
    FCountryName: string;
    FOrganizationName: string;
    FEmailAddress: string;
    FLocalityName: string;
    FCommonName: string;
    FOrganizationalUnitName: string;
    FStateOrProvinceName: string;
  public
    procedure Assign(Source: TPersistent); override;
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
  published
    property CountryName: string read FCountryName write FCountryName;
    {* 国家名}
    property StateOrProvinceName: string read FStateOrProvinceName write FStateOrProvinceName;
    {* 州名或省名}
    property LocalityName: string read FLocalityName write FLocalityName;
    {* 地区名或城市名}
    property OrganizationName: string read FOrganizationName write FOrganizationName;
    {* 组织名}
    property OrganizationalUnitName: string read FOrganizationalUnitName write FOrganizationalUnitName;
    {* 组织单位名}
    property CommonName: string read FCommonName write FCommonName;
    {* 域名}
    property EmailAddress: string read FEmailAddress write FEmailAddress;
    {* 电子邮件地址}
  end;

  // 以下是证书请求的声明

  TCnCertificateRequestInfo = class(TCnCertificateBaseInfo);
  {* 证书请求中包含的基本信息}

  TCnCertificateRequest = class(TObject)
  {* 描述证书请求中的信息，包括普通字段、公钥、摘要类型与签名等，兼容 RSA 与 ECC}
  private
    FIsRSA: Boolean;
    FCertificateRequestInfo: TCnCertificateRequestInfo;
    FRSAPublicKey: TCnRSAPublicKey;
    FEccPublicKey: TCnEccPublicKey;
    FCASignType: TCnCASignType;
    FSignValue: Pointer;
    FSignLength: Integer;
    FDigestLength: Integer;
    FDigestValue: Pointer;
    FRSADigestType: TCnRSASignDigestType;
    FEccCurveType: TCnEccCurveType;
    FEccDigestType: TCnEccSignDigestType;
    procedure SetCertificateRequestInfo(const Value: TCnCertificateRequestInfo);
    procedure SetRSAPublicKey(const Value: TCnRSAPublicKey);
    procedure SetEccPublicKey(const Value: TCnEccPublicKey); // 签名 Length 为 Key 的 Bit 数如 2048 Bit。
  public
    constructor Create;
    destructor Destroy; override;

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property IsRSA: Boolean read FIsRSA write FIsRSA;
    {* 类型是 RSA 还是 ECC}
    property CertificateRequestInfo: TCnCertificateRequestInfo
      read FCertificateRequestInfo write SetCertificateRequestInfo;
    {* 证书 DN 信息}
    property RSAPublicKey: TCnRSAPublicKey read FRSAPublicKey write SetRSAPublicKey;
    {* 客户端 RSA 公钥}
    property EccPublicKey: TCnEccPublicKey read FEccPublicKey write SetEccPublicKey;
    {* 客户端 Ecc 公钥}
    property EccCurveType: TCnEccCurveType read FEccCurveType write FEccCurveType;
    {* ECC 曲线类型，不支持自定义曲线}
    property CASignType: TCnCASignType read FCASignType write FCASignType;
    {* 客户端使用的散列与签名算法}
    property SignValue: Pointer read FSignValue write FSignValue;
    {* 散列后签名的结果，析构时需释放}
    property SignLength: Integer read FSignLength write FSignLength;
    {* 散列后签名的结果长度}
    property RSADigestType: TCnRSASignDigestType read FRSADigestType write FRSADigestType;
    {* 客户端 RSA 散列使用的散列算法，应与 CASignType 意义相等}
    property EccDigestType: TCnEccSignDigestType read FEccDigestType write FEccDigestType;
    {* 客户端 Ecc 散列使用的散列算法，应与 CASignType 意义相等}
    property DigestValue: Pointer read FDigestValue write FDigestValue;
    {* 散列值，中间结果，不直接存储于 CSR 文件中，析构时需释放}
    property DigestLength: Integer read FDigestLength write FDigestLength;
    {* 散列值的长度}
  end;

  // 以上是证书请求的声明，以下是证书认证的声明

{
   Name ::= CHOICE
     rdnSequence  RDNSequence

   RDNSequence ::= SEQUENCE OF RelativeDistinguishedName

   RelativeDistinguishedName ::=
     SET SIZE (1..MAX) OF AttributeTypeAndValue

   AttributeTypeAndValue ::= SEQUENCE
     type     AttributeType,
     value    AttributeValue

   AttributeType ::= OBJECT IDENTIFIER

   AttributeValue ::= ANY -- DEFINED BY AttributeType

   DirectoryString ::= CHOICE
         teletexString           TeletexString (SIZE (1..MAX)),
         printableString         PrintableString (SIZE (1..MAX)),
         universalString         UniversalString (SIZE (1..MAX)),
         utf8String              UTF8String (SIZE (1..MAX)),
         bmpString               BMPString (SIZE (1..MAX))
}

  TCnCertificateNameInfo = class(TCnCertificateBaseInfo)
  {* 用于 Subject 与 Issuer 的基本信息描述类}
  private
    FSurName: string;
    FTitle: string;
    FGivenName: string;
    FInitials: string;
    FSerialNumber: string;
    FPseudonym: string;
    FGenerationQualifier: string;
  public
    property SerialNumber: string read FSerialNumber write FSerialNumber;
    property Title: string read FTitle write FTitle;
    property SurName: string read FSurName write FSurName;
    property GivenName: string read FGivenName write FGivenName;
    property Initials: string read FInitials write FInitials;
    property Pseudonym: string read FPseudonym write FPseudonym;
    property GenerationQualifier: string read FGenerationQualifier write FGenerationQualifier;
  end;

  TCnCertificateSubjectInfo = class(TCnCertificateNameInfo);
  {* 证书请求中包含的被签发者的基本信息，也即上面的 Name}

  TCnCertificateIssuerInfo = class(TCnCertificateNameInfo);
  {* 证书请求中包含的签发者的基本信息，也即上面的 Name}

  TCnUTCTime = class(TObject)
  {* 证书中代表过期时间的解析类}
  private
    FUTCTimeString: string;
    FDateTime: TDateTime;
    procedure SetDateTime(const Value: TDateTime);
    procedure SetUTCTimeString(const Value: string);
  public
    property DateTime: TDateTime read FDateTime write SetDateTime;
    property UTCTimeString: string read FUTCTimeString write SetUTCTimeString;
  end;

{
   Extension  ::=  SEQUENCE
        extnID      OBJECT IDENTIFIER,
        critical    BOOLEAN DEFAULT FALSE,
        extnValue   OCTET STRING
                    -- contains the DER encoding of an ASN.1 value
                    -- corresponding to the extension type identified
                    -- by extnID
}

  TCnCerKeyUsage = (kuDigitalSignature, kuContentCommitment, kuKeyEncipherment,
    kuDataEncipherment, kuKeyAgreement, kuKeyCertSign, kuCRLSign, kuEncipherOnly,
    kuDecipherOnly);
  TCnCerKeyUsages = set of TCnCerKeyUsage;

  TCnExtendedKeyUsage = (ekuServerAuth, ekuClientAuth, ekuCodeSigning, ekuEmailProtection,
    ekuTimeStamping, ekuOCSPSigning);
  TCnExtendedKeyUsages = set of TCnExtendedKeyUsage;

{
  标准扩展包括以下内容：
  // Authority Key Identifier       签发者密钥标识符 array of Byte
  // Subject Key Identifier         被签发者公钥标记 array of Byte
  // Key Usage                      密钥用法集合 TCnCerKeyUsages
  // Certificate Policies
  // Policy Mappings
  // Subject Alternative Name       被签发者的替代名称，字符串列表
  // Issuer Alternative Name        签发者的替代名称，字符串列表
  // Subject Directory Attributes
  // Basic Constraints              基本限制：是否 CA 以及嵌套层数
  // Name Constraints
  // Policy Constraints
  // Extended Key Usage             增强型密钥用法集合
  // CRL Distribution Points        CRL 发布 URL，字符串列表
  // Inhibit anyPolicy
  // Freshest CRL (a.k.a. Delta CRL Distribution Point)
}
  TCnCertificateStandardExtensions = class(TObject)
  {* 证书标准扩展内容}
  private
    FKeyUsage: TCnCerKeyUsages;
    FSubjectAltName: TStrings;
    FIssuerAltName: TStrings;
    FAuthorityKeyIdentifier: AnsiString;
    FSubjectKeyIdentifier: AnsiString;
    FCRLDistributionPoints: TStrings;
    FExtendedKeyUsage: TCnExtendedKeyUsages;
    FBasicConstraintsCA: Boolean;
    FBasicConstraintsPathLen: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property KeyUsage: TCnCerKeyUsages read FKeyUsage write FKeyUsage;
    property ExtendedKeyUsage: TCnExtendedKeyUsages read FExtendedKeyUsage write FExtendedKeyUsage;
    property BasicConstraintsCA: Boolean read FBasicConstraintsCA write FBasicConstraintsCA;
    property BasicConstraintsPathLen: Integer read FBasicConstraintsPathLen write FBasicConstraintsPathLen;
    property SubjectAltName: TStrings read FSubjectAltName;
    property IssuerAltName: TStrings read FIssuerAltName;
    property CRLDistributionPoints: TStrings read FCRLDistributionPoints;
    property AuthorityKeyIdentifier: AnsiString read FAuthorityKeyIdentifier write FAuthorityKeyIdentifier;
    property SubjectKeyIdentifier: AnsiString read FSubjectKeyIdentifier write FSubjectKeyIdentifier;
  end;

{
  私有互联网扩展包括以下内容：
  // Authority Information Access   签发者的信息，包括 ocsp 与 caIssuers 俩 URL
  // Subject Information Access     没啥东西
}

  TCnCertificatePrivateInternetExtensions = class(TObject)
  private
    FAuthorityInformationAccessCaIssuers: string;
    FAuthorityInformationAccessOcsp: string;
  public
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property AuthorityInformationAccessOcsp: string read FAuthorityInformationAccessOcsp
      write FAuthorityInformationAccessOcsp;
    {* 上级签发证书 Ocsp 的 URL}
    property AuthorityInformationAccessCaIssuers: string read FAuthorityInformationAccessCaIssuers
      write FAuthorityInformationAccessCaIssuers;
    {* 上级签发机构证书访问 URL}
  end;

{
  TBSCertificate  ::=  SEQUENCE
    version         [0]  EXPLICIT Version DEFAULT v1,
    serialNumber         CertificateSerialNumber,
    signature            AlgorithmIdentifier,
    issuer               Name,
    validity             Validity,
    subject              Name,
    subjectPublicKeyInfo SubjectPublicKeyInfo,
    issuerUniqueID  [1]  IMPLICIT UniqueIdentifier OPTIONAL,
                         -- If present, version MUST be v2 or v3
    subjectUniqueID [2]  IMPLICIT UniqueIdentifier OPTIONAL,
                         -- If present, version MUST be v2 or v3
    extensions      [3]  EXPLICIT Extensions OPTIONAL
                         -- If present, version MUST be v3
}

  TCnBasicCertificate = class(TObject)
  {* 证书中的基本信息域，包括签发者与被签发者的信息}
  private
    FSerialNumber: string;
    FNotAfter: TCnUTCTime;
    FNotBefore: TCnUTCTime;
    FVersion: Integer;
    FSubject: TCnCertificateSubjectInfo;
    FSubjectUniqueID: string;
    FIssuer: TCnCertificateIssuerInfo;
    FIssuerUniqueID: string;
    FSubjectRSAPublicKey: TCnRSAPublicKey;
    FSubjectEccPublicKey: TCnEccPublicKey;
    FPrivateInternetExtension: TCnCertificatePrivateInternetExtensions;
    FStandardExtension: TCnCertificateStandardExtensions;
    FSubjectEccCurveType: TCnEccCurveType;
    FSubjectIsRSA: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property Version: Integer read FVersion write FVersion;
    {* 版本号，值 0、1、2 表示版本号为 v1、v2、v3，默认 v1 时可省略
      有 extensions 时必须是 v3，无 extensions 但有 UniqueIdentifier 时 v2
      建议生成版本 v3 的}
    property SerialNumber: string read FSerialNumber write FSerialNumber;
    {* 序列号，本来应该是整型，但当作字符串处理}
    property SubjectIsRSA: Boolean read FSubjectIsRSA write FSubjectIsRSA;
    {* 被签发者是 RSA 还是 ECC，注意没有散列算法类型，散列算法由签发者决定}
    property Subject: TCnCertificateSubjectInfo read FSubject write FSubject;
    {* 被签发者的基本信息}
    property SubjectRSAPublicKey: TCnRSAPublicKey read FSubjectRSAPublicKey write FSubjectRSAPublicKey;
    {* 被签发者的 RSA 公钥}
    property SubjectEccPublicKey: TCnEccPublicKey read FSubjectEccPublicKey write FSubjectEccPublicKey;
    {* 被签发者的 ECC 公钥}
    property SubjectEccCurveType: TCnEccCurveType read FSubjectEccCurveType write FSubjectEccCurveType;
    {* 被签发者的 ECC 曲线类型}
    property SubjectUniqueID: string read FSubjectUniqueID write FSubjectUniqueID;
    {* v2 时被签发者的唯一 ID}
    property Issuer: TCnCertificateIssuerInfo read FIssuer write FIssuer;
    {* 签发者的基本信息}
    property IssuerUniqueID: string read FIssuerUniqueID write FIssuerUniqueID;
    {* v2 时签发者的唯一 ID}
    property NotBefore: TCnUTCTime read FNotBefore;
    {* 有效期起始}
    property NotAfter: TCnUTCTime read FNotAfter;
    {* 有效期结束}

    property StandardExtension: TCnCertificateStandardExtensions read FStandardExtension;
    {* 标准扩展对象集合}
    property PrivateInternetExtension: TCnCertificatePrivateInternetExtensions read FPrivateInternetExtension;
    {* 私有互联网扩展对象集合}
  end;

{
  Certificate  ::=  SEQUENCE
    tbsCertificate       TBSCertificate,
    signatureAlgorithm   AlgorithmIdentifier,
    signatureValue       BIT STRING
}

  TCnCertificate = class(TObject)
  {* 描述一完整的证书，注意其中并无签发者的公钥，公钥只有被签发者的}
  private
    FDigestLength: Integer;
    FSignLength: Integer;
    FDigestValue: Pointer;
    FSignValue: Pointer;
    FCASignType: TCnCASignType;
    FRSADigestType: TCnRSASignDigestType;
    FBasicCertificate: TCnBasicCertificate;
    FIsRSA: Boolean;
    FEccDigestType: TCnEccSignDigestType;
    function GetIsSelfSigned: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property IsSelfSigned: Boolean read GetIsSelfSigned;
    {* 是否自签名证书，使用签发者与被签发者信息是否相同来判断}
    property IsRSA: Boolean read FIsRSA write FIsRSA;
    {* 是否是 RSA 证书，否则是 ECC 证书。这个字段指签发者的证书类型}

    property BasicCertificate: TCnBasicCertificate read FBasicCertificate;
    {* 证书基本信息类，包括签发者与被签发者的信息}
    property CASignType: TCnCASignType read FCASignType write FCASignType;
    {* 签发者使用的散列与签名算法}
    property SignValue: Pointer read FSignValue write FSignValue;
    {* 散列后签名的结果}
    property SignLength: Integer read FSignLength write FSignLength;
    {* 散列后签名的结果长度}
    property RSADigestType: TCnRSASignDigestType read FRSADigestType write FRSADigestType;
    {* 客户端散列使用的散列算法，应与 CASignType 意义相等}
    property EccDigestType: TCnEccSignDigestType read FEccDigestType write FEccDigestType;
    {* 客户端 Ecc 散列使用的散列算法，应与 CASignType 意义相等}
    property DigestValue: Pointer read FDigestValue write FDigestValue;
    {* 散列值，中间结果，不直接存储于 CRT 文件中}
    property DigestLength: Integer read FDigestLength write FDigestLength;
    {* 散列值的长度}
  end;

function CnCANewCertificateSignRequest(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCSRFile: string; const CountryName: string; const
  StateOrProvinceName: string; const LocalityName: string; const OrganizationName:
  string; const OrganizationalUnitName: string; const CommonName: string; const
  EmailAddress: string; CASignType: TCnCASignType = ctSha1RSA): Boolean; overload;
{* 根据公私钥与一些 DN 信息以及指定散列算法生成 CSR 格式的 RSA 证书请求文件}

function CnCANewCertificateSignRequest(PrivateKey: TCnEccPrivateKey; PublicKey:
  TCnEccPublicKey; CurveType: TCnEccCurveType; const OutCSRFile: string; const CountryName: string;
  const StateOrProvinceName: string; const LocalityName: string; const OrganizationName: string;
  const OrganizationalUnitName: string; const CommonName: string; const EmailAddress: string;
  CASignType: TCnCASignType = ctSha1Ecc): Boolean; overload;
{* 根据公私钥与一些 DN 信息以及指定散列算法生成 CSR 格式的 ECC 证书请求文件}

function CnCANewSelfSignedCertificate(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCRTFile: string; const CountryName: string; const
  StateOrProvinceName: string; const LocalityName: string; const OrganizationName:
  string; const OrganizationalUnitName: string; const CommonName: string; const
  EmailAddress: string; const IntSerialNum: string; NotBefore, NotAfter: TDateTime;
  CASignType: TCnCASignType = ctSha1RSA): Boolean; overload;
{* 根据公私钥与一些 DN 信息以及指定散列算法生成 RSA CRT 格式的自签名证书，目前只支持 v1 格式}

function CnCANewSelfSignedCertificate(PrivateKey: TCnEccPrivateKey; PublicKey:
  TCnEccPublicKey; CurveType: TCnEccCurveType; const OutCRTFile: string; const CountryName: string;
  const StateOrProvinceName: string; const LocalityName: string; const OrganizationName: string;
  const OrganizationalUnitName: string; const CommonName: string; const EmailAddress: string;
  const IntSerialNum: string; NotBefore, NotAfter: TDateTime; CASignType: TCnCASignType = ctSha1RSA): Boolean; overload;
{* 根据公私钥与一些 DN 信息以及指定散列算法生成 ECC CRT 格式的自签名证书，目前只支持 v1 格式}

function CnCALoadCertificateSignRequestFromFile(const FileName: string;
  CertificateRequest: TCnCertificateRequest): Boolean;
{* 解析 PEM 格式的 CSR 文件并将内容放入 TCnCertificateRequest 对象中}

function CnCALoadCertificateSignRequestFromStream(Stream: TStream;
  CertificateRequest: TCnCertificateRequest): Boolean;
{* 解析 PEM 格式的 CSR 文件并将内容放入 TCnCertificateRequest 对象中}

function CnCAVerifyCertificateSignRequestFile(const FileName: string): Boolean;
{* 验证一 CSR 文件的内容是否合乎签名}

function CnCAVerifyCertificateSignRequestStream(Stream: TStream): Boolean;
{* 验证一 CSR 流的内容是否合乎签名}

function CnCAVerifySelfSignedCertificateFile(const FileName: string): Boolean;
{* 验证一自签名的 CRT 文件的内容是否合乎签名}

function CnCAVerifySelfSignedCertificateStream(Stream: TStream): Boolean;
{* 验证一自签名的 CRT 流的内容是否合乎签名}

function CnCAVerifyCertificateFile(const FileName: string;
  ParentPublicKey: TCnRSAPublicKey): Boolean; overload;
{* 用 RSA 签发者公钥验证一 CRT 文件的内容是否合乎签名}

function CnCAVerifyCertificateFile(const FileName: string; ParentPublicKey: TCnEccPublicKey;
  ParentCurveType: TCnEccCurveType): Boolean; overload;
{* 用 ECC 签发者公钥验证一 CRT 文件的内容是否合乎签名}

function CnCAVerifyCertificateStream(Stream: TStream;
  ParentPublicKey: TCnRSAPublicKey): Boolean; overload;
{* 用 RSA 签发者公钥验证一 CRT 流的内容是否合乎签名}

function CnCAVerifyCertificateStream(Stream: TStream; ParentPublicKey: TCnEccPublicKey;
  ParentCurveType: TCnEccCurveType): Boolean; overload;
{* 用 ECC 签发者公钥验证一 CRT 流的内容是否合乎签名}

function CnCALoadCertificateFromFile(const FileName: string;
  Certificate: TCnCertificate): Boolean;
{* 解析 PEM 格式的 CRT 证书文件并将内容放入 TCnCertificate 对象中}

function CnCALoadCertificateFromStream(Stream: TStream;
  Certificate: TCnCertificate): Boolean;
{* 解析 PEM 格式的 CRT 证书流并将内容放入 TCnCertificate 对象中}

function CnCASignCertificate(PrivateKey: TCnRSAPrivateKey; const CRTFile: string;
  const CSRFile: string; const OutCRTFile: string; const IntSerialNum: string;
  NotBefore, NotAfter: TDateTime; CASignType: TCnCASignType = ctSha1RSA): Boolean; overload;
{* 用 RSA CRT 证书内容与对应私钥签署证书请求，生成被签发证书，目前只支持 v1 格式
   兼容客户端证书请求是 ECC/RSA 的情形}

function CnCASignCertificate(PrivateKey: TCnEccPrivateKey; CurveType: TCnEccCurveType;
  const CRTFile: string; const CSRFile: string; const OutCRTFile: string; const IntSerialNum: string;
  NotBefore, NotAfter: TDateTime; CASignType: TCnCASignType = ctSha1Ecc): Boolean; overload;
{* 用 ECC CRT 证书内容与对应私钥签署证书请求，生成被签发证书，目前只支持 v1 格式
  兼容客户端证书请求是 ECC/RSA 的情形}

// =============================== 其他辅助函数 ================================

function AddCASignTypeOIDNodeToWriter(AWriter: TCnBerWriter; CASignType: TCnCASignType;
  AParent: TCnBerWriteNode): TCnBerWriteNode;
{* 将一个散列算法的 OID 写入一个 Ber 节点}

function GetCASignNameFromSignType(Sign: TCnCASignType): string;
{* 从证书的签名散列算法枚举值获取其名称}

implementation

const
  // PKCS#10
  PEM_CERTIFICATE_REQUEST_HEAD = '-----BEGIN CERTIFICATE REQUEST-----';
  PEM_CERTIFICATE_REQUEST_TAIL = '-----END CERTIFICATE REQUEST-----';
  PEM_CERTIFICATE_HEAD = '-----BEGIN CERTIFICATE-----';
  PEM_CERTIFICATE_TAIL = '-----END CERTIFICATE-----';

  OID_DN_COUNTRYNAME             : array[0..2] of Byte = ($55, $04, $06); // 2.5.4.6
  OID_DN_STATEORPROVINCENAME     : array[0..2] of Byte = ($55, $04, $08); // 2.5.4.8
  OID_DN_LOCALITYNAME            : array[0..2] of Byte = ($55, $04, $07); // 2.5.4.7
  OID_DN_ORGANIZATIONNAME        : array[0..2] of Byte = ($55, $04, $0A); // 2.5.4.10
  OID_DN_ORGANIZATIONALUNITNAME  : array[0..2] of Byte = ($55, $04, $0B); // 2.5.4.11
  OID_DN_COMMONNAME              : array[0..2] of Byte = ($55, $04, $03); // 2.5.4.3
  OID_DN_EMAILADDRESS            : array[0..8] of Byte = (
    $2A, $86, $48, $86, $F7, $0D, $01, $09, $01
  ); // 1.2.840.113549.1.9.1

  // 扩展字段们的 OID
  OID_EXT_SUBJECTKEYIDENTIFIER   : array[0..2] of Byte = ($55, $1D, $0E); // 2.5.29.14
  OID_EXT_KEYUSAGE               : array[0..2] of Byte = ($55, $1D, $0F); // 2.5.29.15
  OID_EXT_SUBJECTALTNAME         : array[0..2] of Byte = ($55, $1D, $11); // 2.5.29.17
  OID_EXT_ISSUERTALTNAME         : array[0..2] of Byte = ($55, $1D, $12); // 2.5.29.18
  OID_EXT_BASICCONSTRAINTS       : array[0..2] of Byte = ($55, $1D, $13); // 2.5.29.19
  OID_EXT_CRLDISTRIBUTIONPOINTS  : array[0..2] of Byte = ($55, $1D, $1F); // 2.5.29.31
  OID_EXT_CERTIFICATEPOLICIES    : array[0..2] of Byte = ($55, $1D, $20); // 2.5.29.32
  OID_EXT_AUTHORITYKEYIDENTIFIER : array[0..2] of Byte = ($55, $1D, $23); // 2.5.29.35
  OID_EXT_EXTKEYUSAGE            : array[0..2] of Byte = ($55, $1D, $25); // 2.5.29.37
  OID_EXT_AUTHORITYINFOACCESS    : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $01, $01
  ); // 1.3.6.1.5.5.7.1.1
  OID_EXT_AUTHORITYINFOACCESS_OCSP         : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $30, $01
  ); // 1.3.6.1.5.5.7.48.1
  OID_EXT_AUTHORITYINFOACCESS_CAISSUERS    : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $30, $02
  ); // 1.3.6.1.5.5.7.48.2

  // authorityInfoAccess Subs
  OID_EXT_EXT_AUTHORITYINFOACCESS_OCSP  : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $30, $01
  ); // 1.3.6.1.5.5.7.48.1
  OID_EXT_EXT_AUTHORITYINFOACCESS_CAISSUERS  : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $30, $02
  ); // 1.3.6.1.5.5.7.48.2

  // Extended Key Usages
  OID_EXT_EXT_KEYUSAGE_SERVERAUTH  : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $03, $01
  ); // 1.3.6.1.5.5.7.3.1
  OID_EXT_EXT_KEYUSAGE_CLIENTAUTH  : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $03, $02
  ); // 1.3.6.1.5.5.7.3.2
  OID_EXT_EXT_KEYUSAGE_CODESIGNING : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $03, $03
  ); // 1.3.6.1.5.5.7.3.3
  OID_EXT_EXT_KEYUSAGE_EMAILPROTECTION : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $03, $04
  ); // 1.3.6.1.5.5.7.3.4
  OID_EXT_EXT_KEYUSAGE_TIMESTAMPING : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $03, $08
  ); // 1.3.6.1.5.5.7.3.8
  OID_EXT_EXT_KEYUSAGE_OCSPSIGNING  : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $03, $09
  ); // 1.3.6.1.5.5.7.3.9

  // Hash Signature OIDs
  OID_SHA1_RSAENCRYPTION          : array[0..8] of Byte = (
    $2A, $86, $48, $86, $F7, $0D, $01, $01, $05
  ); // 1.2.840.113549.1.1.5

  OID_SHA256_RSAENCRYPTION        : array[0..8] of Byte = (
    $2A, $86, $48, $86, $F7, $0D, $01, $01, $0B
  ); // 1.2.840.113549.1.1.11

  OID_SHA1_ECDSA                  : array[0..6] of Byte = (
    $2A, $86, $48, $CE, $3D, $04, $01
  ); // 1.2.840.10045.4.1

  OID_SHA256_ECDSA                : array[0..7] of Byte = (
    $2A, $86, $48, $CE, $3D, $04, $03, $02
  ); // 1.2.840.10045.4.3.2

  OID_SM2_SM3ENCRYPTION           : array[0..7] of Byte = (
    $2A, $81, $1C, $CF, $55, $01, $83, $75
  ); // 1.2.156.10197.1.501

  SCRLF = #13#10;

  // 用于交换字符串数据的常量
  SDN_COUNTRYNAME                = 'CountryName';
  SDN_STATEORPROVINCENAME        = 'StateOrProvinceName';
  SDN_LOCALITYNAME               = 'LocalityName';
  SDN_ORGANIZATIONNAME           = 'OrganizationName';
  SDN_ORGANIZATIONALUNITNAME     = 'OrganizationalUnitName';
  SDN_COMMONNAME                 = 'CommonName';
  SDN_EMAILADDRESS               = 'EmailAddress';

  RSA_CA_TYPES: TCnCASignTypes = [ctMd5RSA, ctSha1RSA, ctSha256RSA];
  ECC_CA_TYPES: TCnCASignTypes = [ctMd5Ecc, ctSha1Ecc, ctSha256Ecc, ctSM2withSM3];

var
  DummyPointer: Pointer;
  DummyInteger: Integer;
//  DummyCASignType: TCnCASignType;
  DummyDigestType: TCnRSASignDigestType;

function PrintHex(const Buf: Pointer; Len: Integer): string;
var
  I: Integer;
  P: PByteArray;
const
  Digits: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
                                      '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
begin
  Result := '';
  if Len <= 0 then
    Exit;

  P := PByteArray(Buf);
  if P = nil then
    Exit;

  for I := 0 to Len - 1 do
  begin
    Result := Result + {$IFDEF UNICODE}string{$ENDIF}(Digits[(P^[I] shr 4) and $0F] +
              Digits[P^[I] and $0F]);
  end;
end;

function AddCASignTypeOIDNodeToWriter(AWriter: TCnBerWriter; CASignType: TCnCASignType;
  AParent: TCnBerWriteNode): TCnBerWriteNode;
begin
  Result := nil;
  case CASignType of
    ctSha1RSA:
      Result := AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SHA1_RSAENCRYPTION[0],
        SizeOf(OID_SHA1_RSAENCRYPTION), AParent);
    ctSha256RSA:
      Result := AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SHA256_RSAENCRYPTION[0],
        SizeOf(OID_SHA256_RSAENCRYPTION), AParent);
    ctSha1Ecc:
      Result := AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SHA1_ECDSA[0],
        SizeOf(OID_SHA1_ECDSA), AParent);
    ctSha256Ecc:
      Result := AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SHA256_ECDSA[0],
        SizeOf(OID_SHA256_ECDSA), AParent);
    ctSM2withSM3:
      Result := AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SM2_SM3ENCRYPTION[0],
        SizeOf(OID_SM2_SM3ENCRYPTION), AParent)
    // TODO: 其它算法类型支持
  end;
end;

// 根据指定数字摘要算法计算数据的二进制散列值并写入 Stream，Buffer 是指针
function CalcDigestData(const Buffer; Count: Integer; CASignType: TCnCASignType;
  outStream: TStream): Boolean;
var
  Md5: TMD5Digest;
  Sha1: TSHA1Digest;
  Sha256: TSHA256Digest;
begin
  Result := False;
  case CASignType of
    ctMd5RSA, ctMd5Ecc:
      begin
        Md5 := MD5Buffer(Buffer, Count);
        outStream.Write(Md5, SizeOf(TMD5Digest));
        Result := True;
      end;
    ctSha1RSA, ctSha1Ecc:
      begin
        Sha1 := SHA1Buffer(Buffer, Count);
        outStream.Write(Sha1, SizeOf(TSHA1Digest));
        Result := True;
      end;
    ctSha256RSA, ctSha256Ecc:
      begin
        Sha256 := SHA256Buffer(Buffer, Count);
        outStream.Write(Sha256, SizeOf(TSHA256Digest));
        Result := True;
      end;
  end;
end;

function GetRSASignTypeFromCASignType(CASignType: TCnCASignType): TCnRSASignDigestType;
begin
  Result := rsdtSHA1;
  case CASignType of
    ctMd5RSA:
      Result := rsdtMD5;
    ctSha1RSA:
      Result := rsdtSHA1;
    ctSha256RSA:
      Result := rsdtSHA256;
  end;
end;

function GetEccSignTypeFromCASignType(CASignType: TCnCASignType): TCnEccSignDigestType;
begin
  Result := esdtSHA1;
  case CASignType of
    ctMd5Ecc:
      Result := esdtMD5;
    ctSha1Ecc:
      Result := esdtSHA1;
    ctSha256Ecc:
      Result := esdtSHA256;
    ctSM2withSM3:
      Result := esdtSM3;
  end;
end;

{ 写如下格式的 RSA 公钥节点
  SEQUENCE(2 elem)    - PubNode
    SEQUENCE(2 elem)
      OBJECT IDENTIFIER 1.2.840.113549.1.1.1 rsaEncryption(PKCS #1)
      NULL
    BIT STRING(1 elem)
      SEQUENCE(2 elem)
        INTEGER
        INTEGER
}
procedure WriteRSAPublicKeyToNode(AWriter: TCnBerWriter; PubNode: TCnBerWriteNode;
  PublicKey: TCnRSAPublicKey);
var
  Node: TCnBerWriteNode;
begin
  Node := AWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, PubNode);
  AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_RSAENCRYPTION_PKCS1[0],
    SizeOf(OID_RSAENCRYPTION_PKCS1), Node);
  AWriter.AddNullNode(Node);
  Node := AWriter.AddContainerNode(CN_BER_TAG_BIT_STRING, PubNode);
  Node := AWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, Node);
  AddBigNumberToWriter(AWriter, PublicKey.PubKeyProduct, Node);
  AddBigNumberToWriter(AWriter, PublicKey.PubKeyExponent, Node);
end;

{ 写如下格式的 ECC 公钥节点
  SEQUENCE (2 elem)
    SEQUENCE (2 elem)
      OBJECT IDENTIFIER 1.2.840.10045.2.1 ecPublicKey (ANSI X9.62 public key type)
      OBJECT IDENTIFIER 1.3.132.0.10 secp256k1 (SECG (Certicom) named elliptic curve)
    BIT STRING （里头是非压缩公钥存储格式）
}
function WriteEccPublicKeyToNode(AWriter: TCnBerWriter; PubNode: TCnBerWriteNode;
  PublicKey: TCnEccPublicKey; CurveType: TCnEccCurveType): Boolean;
var
  Node: TCnBerWriteNode;
  CurvePtr: Pointer;
  CurveLen: Integer;
begin
  Result := False;
  Node := AWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, PubNode);
  AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_EC_PUBLIC_KEY[0],
    SizeOf(OID_EC_PUBLIC_KEY), Node);
  CurveLen := GetOIDFromCurveType(CurveType, CurvePtr);
  if CurveLen <= 0 then
    Exit;

  AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, CurvePtr, CurveLen, Node);
  WriteEccPublicKeyToBitStringNode(AWriter, PubNode, PublicKey);
  Result := True;
end;

{
  RSA 生成并写签名节点
  SEQUENCE (2 elem)
    OBJECT IDENTIFIER 1.2.840.113549.1.1.5 sha1WithRSAEncryption (PKCS #1)
    NULL
  BIT STRING (2048 bit)
}
function GenerateRSASignatureNode(AWriter: TCnBerWriter; Root, NodeToSign: TCnBerWriteNode;
  PrivateKey: TCnRSAPrivateKey; CASignType: TCnCASignType): Boolean;
var
  ValueStream, DigestStream: TMemoryStream;
  HashWriter: TCnBerWriter;
  HashRoot, HashNode, Node: TCnBerWriteNode;
  OutBuf: array of Byte;
  OutLen: Integer;
begin
  Result := False;
  ValueStream := nil;
  DigestStream := nil;
  HashWriter := nil;

  try
    // 拿出 InfoRoot 的数据
    ValueStream := TMemoryStream.Create;
    NodeToSign.SaveToStream(ValueStream);

    // 计算其 Hash
    DigestStream := TMemoryStream.Create;
    CalcDigestData(ValueStream.Memory, ValueStream.Size, CASignType, DigestStream);

    // 将 Hash 及其签名算法拼成 BER 编码
    HashWriter := TCnBerWriter.Create;
    HashRoot := HashWriter.AddContainerNode(CN_BER_TAG_SEQUENCE);
    Node := HashWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, HashRoot);
    AddDigestTypeOIDNodeToWriter(HashWriter, GetRSASignTypeFromCASignType(CASignType), Node);
    HashWriter.AddNullNode(Node);
    HashWriter.AddBasicNode(CN_BER_TAG_OCTET_STRING, DigestStream, HashRoot);

    // 复用此 Stream，保存生成的 BER 格式内容
    DigestStream.Clear;
    HashWriter.SaveToStream(DigestStream);

    // RSA 私钥加密此 BER 块得到签名值，加密前需要 PKCS1 补齐
    SetLength(OutBuf, PrivateKey.BitsCount div 8);
    OutLen := PrivateKey.BitsCount div 8;
    if not CnRSAEncryptData(DigestStream.Memory, DigestStream.Size,
      @OutBuf[0], PrivateKey) then
      Exit;

    // 增加 Hash 算法说明
    HashNode := AWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);
    AddCASignTypeOIDNodeToWriter(AWriter, CASignType, HashNode);
    AWriter.AddNullNode(HashNode);

    // 写入最终签名值
    AWriter.AddBasicNode(CN_BER_TAG_BIT_STRING, @OutBuf[0], OutLen, Root);
    Result := True;
  finally
    HashWriter.Free;
    DigestStream.Free;
    ValueStream.Free;
  end;
end;

{
  ECC 生成并写签名节点
  SEQUENCE (1 elem)
    OBJECT IDENTIFIER 1.2.840.10045.4.3.2 ecdsaWithSHA256 (ANSI X9.62 ECDSA algorithm with SHA256)
  BIT STRING (1 elem)
    SEQUENCE (2 elem)
      INTEGER (256 bit)
      INTEGER (256 bit)
}
function GenerateEccSignatureNode(AWriter: TCnBerWriter; Root, NodeToSign: TCnBerWriteNode;
  PrivateKey: TCnEccPrivateKey; CurveType: TCnEccCurveType; CASignType: TCnCASignType): Boolean;
var
  ValueStream, SignStream: TMemoryStream;
  HashNode: TCnBerWriteNode;
  Ecc: TCnEcc;
begin
  Result := False;
  ValueStream := nil;
  SignStream := nil;
  Ecc := nil;

  try
    // 拿出 InfoRoot 的数据
    ValueStream := TMemoryStream.Create;
    NodeToSign.SaveToStream(ValueStream);

    SignStream := TMemoryStream.Create;
    Ecc := TCnEcc.Create(CurveType);
    if not CnEccSignStream(ValueStream, SignStream, Ecc, PrivateKey,
      GetEccSignTypeFromCASignType(CASignType)) then
      Exit;

    // 增加 Hash 算法说明
    HashNode := AWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);
    AddCASignTypeOIDNodeToWriter(AWriter, CASignType, HashNode);
    AWriter.AddNullNode(HashNode);

    // 写入最终签名值
    AWriter.AddBasicNode(CN_BER_TAG_BIT_STRING, SignStream.Memory, SignStream.Size, Root);
    Result := True;
  finally
    Ecc.Free;
    SignStream.Free;
    ValueStream.Free;
  end;
end;

procedure WriteDNNameToNode(AWriter: TCnBerWriter; DNOID: Pointer; DNOIDLen: Integer;
  const DN: string; SuperParent: TCnBerWriteNode; ATag: Integer = CN_BER_TAG_PRINTABLESTRING);
var
  ANode: TCnBerWriteNode;
  AnsiDN: AnsiString;
begin
  // Superparent 是 DNRoot，下面是 Set，再 Sequence，Sequence 里加 OID 与 PrintableString
  ANode := AWriter.AddContainerNode(CN_BER_TAG_SET, SuperParent);
  ANode := AWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, ANode);
  AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, PByte(DNOID), DNOIDLen, ANode);
  AnsiDN := AnsiString(DN);
  AWriter.AddBasicNode(ATag, @AnsiDN[1], Length(AnsiDN), ANode);
end;

function CnCANewCertificateSignRequest(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCSRFile: string; const CountryName: string;
  const StateOrProvinceName: string; const LocalityName: string; const OrganizationName: string;
  const OrganizationalUnitName: string; const CommonName: string; const EmailAddress: string;
  CASignType: TCnCASignType): Boolean;
var
  B: Byte;
  OutBuf: array of Byte;
  Writer, HashWriter: TCnBerWriter;
  Stream: TMemoryStream;
  Root, DNRoot, InfoRoot, PubNode: TCnBerWriteNode;
begin
  Result := False;

  if (PrivateKey = nil) or (PublicKey = nil) or (OutCSRFile = '') then
    Exit;

  if (Length(CountryName) <> 2) or (StateOrProvinceName = '') or (LocalityName = '')
    or (OrganizationName = '') or (OrganizationalUnitName = '') or (CommonName = '')
    or (EmailAddress = '') then
    Exit;

  B := 0;
  Writer := nil;
  HashWriter := nil;
  Stream := nil;

  try
    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    InfoRoot := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);

    // 给 Info 写一排直属子节点
    Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, InfoRoot);          // 版本
    DNRoot := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, InfoRoot);  // DN
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, InfoRoot); // 公钥
    Writer.AddRawNode($A0, @B, 1, InfoRoot);                           // 结束符

    // 写 DN 节点的内容
    WriteDNNameToNode(Writer, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, DNRoot, CN_BER_TAG_IA5STRING);

    // 写公钥节点的内容
    WriteRSAPublicKeyToNode(Writer, PubNode, PublicKey);

    // 计算 InfoRoot 块的数字摘要并签名
    if not GenerateRSASignatureNode(Writer, Root, InfoRoot, PrivateKey, CASignType) then
      Exit;

    // 保存
    Stream := TMemoryStream.Create;
    Writer.SaveToStream(Stream);
    Result := SaveMemoryToPemFile(OutCSRFile, PEM_CERTIFICATE_REQUEST_HEAD,
      PEM_CERTIFICATE_REQUEST_TAIL, Stream);
  finally
    Writer.Free;
    HashWriter.Free;
    Stream.Free;
    SetLength(OutBuf, 0);
  end;
end;

function CnCANewCertificateSignRequest(PrivateKey: TCnEccPrivateKey; PublicKey:
  TCnEccPublicKey; CurveType: TCnEccCurveType; const OutCSRFile: string; const CountryName: string;
  const StateOrProvinceName: string; const LocalityName: string; const OrganizationName: string;
  const OrganizationalUnitName: string; const CommonName: string; const EmailAddress: string;
  CASignType: TCnCASignType = ctSha1Ecc): Boolean;
var
  B: Byte;
  OutBuf: array of Byte;
  Writer, HashWriter: TCnBerWriter;
  Stream: TMemoryStream;
  Root, DNRoot, InfoRoot, PubNode: TCnBerWriteNode;
begin
  Result := False;

  if (PrivateKey = nil) or (PublicKey = nil) or (OutCSRFile = '') then
    Exit;

  if (Length(CountryName) <> 2) or (StateOrProvinceName = '') or (LocalityName = '')
    or (OrganizationName = '') or (OrganizationalUnitName = '') or (CommonName = '')
    or (EmailAddress = '') then
    Exit;

  B := 0;
  Writer := nil;
  HashWriter := nil;
  Stream := nil;

  try
    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    InfoRoot := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);

    // 给 Info 写一排直属子节点
    Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, InfoRoot);          // 版本
    DNRoot := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, InfoRoot);  // DN
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, InfoRoot); // 公钥
    Writer.AddRawNode($A0, @B, 1, InfoRoot);                           // 结束符

    // 写 DN 节点的内容
    WriteDNNameToNode(Writer, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, DNRoot, CN_BER_TAG_IA5STRING);

    // 写公钥节点的内容
    if not WriteEccPublicKeyToNode(Writer, PubNode, PublicKey, CurveType) then
      Exit;

    // 计算 InfoRoot 块的数字摘要并签名
    GenerateEccSignatureNode(Writer, Root, InfoRoot, PrivateKey, CurveType, CASignType);

    // 保存
    Stream := TMemoryStream.Create;
    Writer.SaveToStream(Stream);
    Result := SaveMemoryToPemFile(OutCSRFile, PEM_CERTIFICATE_REQUEST_HEAD,
      PEM_CERTIFICATE_REQUEST_TAIL, Stream);
  finally
    Writer.Free;
    HashWriter.Free;
    Stream.Free;
    SetLength(OutBuf, 0);
  end;
end;

procedure ExtractDNValuesToList(DNRoot: TCnBerReadNode; List: TStringList);
var
  I: Integer;
  Node, StrNode: TCnBerReadNode;
begin
  if (DNRoot = nil) or (List = nil) then
    Exit;

  List.Clear;

  // 循环解析 DN 们
  for I := 0 to DNRoot.Count - 1 do
  begin
    Node := DNRoot.Items[I]; // Set
    if (Node.BerTag = CN_BER_TAG_SET) and (Node.Count = 1) then
    begin
      Node := Node.Items[0]; // Sequence
      if (Node.BerTag = CN_BER_TAG_SEQUENCE) and (Node.Count = 2) then
      begin
        StrNode := Node.Items[1];
        Node := Node.Items[0];
        if Node.BerTag = CN_BER_TAG_OBJECT_IDENTIFIER then
        begin
          if CompareObjectIdentifier(Node, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME)) then
            List.Values[SDN_COUNTRYNAME] := StrNode.AsString
          else if CompareObjectIdentifier(Node, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME)) then
            List.Values[SDN_STATEORPROVINCENAME] := StrNode.AsString
          else if CompareObjectIdentifier(Node, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME)) then
            List.Values[SDN_LOCALITYNAME] := StrNode.AsString
          else if CompareObjectIdentifier(Node, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME)) then
            List.Values[SDN_ORGANIZATIONNAME] := StrNode.AsString
          else if CompareObjectIdentifier(Node, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME)) then
            List.Values[SDN_ORGANIZATIONALUNITNAME] := StrNode.AsString
          else if CompareObjectIdentifier(Node, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME)) then
            List.Values[SDN_COMMONNAME] := StrNode.AsString
          else if CompareObjectIdentifier(Node, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS)) then
            List.Values[SDN_EMAILADDRESS] := StrNode.AsString  // Email is not PrintableString
        end;
      end;
    end;
  end;
end;

function ExtractCASignType(ObjectIdentifierNode: TCnBerReadNode): TCnCASignType;
begin
  Result := ctSha256RSA; // Default
  if CompareObjectIdentifier(ObjectIdentifierNode, @OID_SHA1_RSAENCRYPTION[0],
    SizeOf(OID_SHA1_RSAENCRYPTION)) then
    Result := ctSha1RSA
  else if CompareObjectIdentifier(ObjectIdentifierNode, @OID_SHA256_RSAENCRYPTION[0],
    SizeOf(OID_SHA256_RSAENCRYPTION)) then
    Result := ctSha256RSA
  else if CompareObjectIdentifier(ObjectIdentifierNode, @OID_SHA1_ECDSA[0],
    SizeOf(OID_SHA1_ECDSA)) then
    Result := ctSha1Ecc
  else if CompareObjectIdentifier(ObjectIdentifierNode, @OID_SHA256_ECDSA[0],
    SizeOf(OID_SHA256_ECDSA)) then
    Result := ctSha256Ecc
  else if CompareObjectIdentifier(ObjectIdentifierNode, @OID_SM2_SM3ENCRYPTION[0],
    SizeOf(OID_SM2_SM3ENCRYPTION)) then
    Result := ctSM2withSM3;
end;

// 从以下结构中解出 RSA 公钥
{
BIT STRING -- PubNode
  SEQUENCE
    INTEGER
    INTEGER 65537
}
function ExtractRSAPublicKey(PubNode: TCnBerReadNode; PublicKey: TCnRSAPublicKey): Boolean;
begin
  Result := False;
  if (PubNode.Count = 1) and (PubNode.Items[0].Count = 2) then
  begin
    PubNode := PubNode.Items[0]; // Sequence
    PublicKey.PubKeyProduct.SetBinary(PAnsiChar(
      PubNode.Items[0].BerDataAddress), PubNode.Items[0].BerDataLength);
    PublicKey.PubKeyExponent.SetBinary(PAnsiChar(
      PubNode.Items[1].BerDataAddress), PubNode.Items[1].BerDataLength);
    Result := True;
  end;
end;

// RSA: 用已知公钥从类似于以下结构中拿出签名值解密并去除 PKCS1 对齐拿到摘要值
// ECC: 直接拿到签名值的原始树结构
// RSA 如果无公钥，则只取签名值，不解开。ECC 不管有没有都解不开
{
  SEQUENCE
    OBJECT IDENTIFIER 1.2.840.113549.1.1.5　sha1WithRSAEncryption(PKCS #1) 或 sha256WithECDSA
    NULL
  BIT STRING  如果是 RSA 则此节点是对齐加密后的 Hash 值；如果是 ECC 则是一个 SEQ 子节点下面再两个 INTEGER
}
function ExtractSignaturesByPublicKey(IsRSA: Boolean; RSAPublicKey: TCnRSAPublicKey;
  EccPublicKey: TCnEccPublicKey; HashNode, SignNode: TCnBerReadNode; out CASignType: TCnCASignType;
  out RSADigestType: TCnRSASignDigestType; out SignValue, DigestValue: Pointer;
  out SignLength, DigestLength: Integer): Boolean;
var
  P: PByte;
  Reader: TCnBerReader;
  Node: TCnBerReadNode;
  OutBuf: array of Byte;
  OutLen: Integer;
begin
  Result := False;

  // 找到签名算法
  if HashNode.Count >= 1 then
    CASignType := ExtractCASignType(HashNode.Items[0]);

  // 无公钥时不解密
  if IsRSA and (RSAPublicKey = nil) then
  begin
    Result := True;
    Exit;
  end;

  if not IsRSA then // ECC 签名是树
  begin
    if SignNode.Count <> 1 then
      Exit;
    if SignNode.Items[0].Count <> 2 then
      Exit;
  end;

  // 解析出原始签名内容，跳过 BIT String 的前导对齐 0
  FreeMemory(SignValue);
  SignLength := SignNode.BerDataLength - 1;
  SignValue := GetMemory(SignLength);
  P := PByte(SignNode.BerDataAddress);
  Inc(P);
  Move(P^, SignValue^, SignLength);

  if IsRSA then // RSA 签名能解开得到原始 Hash 值，但 ECC 不行
  begin
    // 解开 RSA 签名并去除 PKCS1 补齐的内容得到 DER 编码的 Hash 值与算法
    SetLength(OutBuf, RSAPublicKey.BitsCount div 8);
    Reader := nil;

    try
      if CnRSADecryptData(SignValue, SignLength, @OutBuf[0], OutLen, RSAPublicKey) then
      begin
        Reader := TCnBerReader.Create(@OutBuf[0], OutLen);
        Reader.ParseToTree;

        if Reader.TotalCount < 5 then
          Exit;

        Node := Reader.Items[2];
        RSADigestType := GetDigestSignTypeFromBerOID(Node.BerDataAddress,
          Node.BerDataLength);
        if RSADigestType = rsdtNone then
          Exit;

        // 获取 Ber 解出的散列值
        Node := Reader.Items[4];
        FreeMemory(DigestValue);
        DigestLength := Node.BerDataLength;
        DigestValue := GetMemory(DigestLength);
        Move(Node.BerDataAddress^, DigestValue^, DigestLength);

        Result := True;
      end;
    finally
      SetLength(OutBuf, 0);
      Reader.Free;
    end;
  end
  else
    Result := True;
end;

function ExtractExtensions(Root: TCnBerReadNode; StandardExt: TCnCertificateStandardExtensions;
  PrivateInternetExt: TCnCertificatePrivateInternetExtensions): Boolean;
var
  I, J: Integer;
  ExtNode, OidNode, ValueNode: TCnBerReadNode;
  Buf: array of Byte;
  KU: TCnCerKeyUsages;
begin
  Result := False;
  if (Root = nil) or (Root.Count < 1) then
    Exit;

  for I := 0 to Root.Count - 1 do
  begin
    ExtNode := Root.Items[I];
    if ExtNode.Count > 0 then
    begin
      OidNode := ExtNode.Items[0];
      ValueNode := nil;
      if ExtNode.Count > 1 then
      begin
        if (ExtNode.Items[1].BerTag = CN_BER_TAG_BOOLEAN) and (ExtNode.Count > 2) then
          ValueNode := ExtNode.Items[2] // Critical，暂不解析
        else
          ValueNode := ExtNode.Items[1];
      end;

      if ValueNode = nil then
        Continue;
      if (ValueNode.BerTag <> CN_BER_TAG_OCTET_STRING) or (ValueNode.Count <> 1) then
        Continue;

      ValueNode := ValueNode.Items[0]; // 指向 OctetString 的子节点，Value 所在
      if CompareObjectIdentifier(OidNode, @OID_EXT_SUBJECTKEYIDENTIFIER, SizeOf(OID_EXT_SUBJECTKEYIDENTIFIER)) then
      begin
        StandardExt.SubjectKeyIdentifier := ValueNode.AsRawString;
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_KEYUSAGE, SizeOf(OID_EXT_KEYUSAGE)) then
      begin
        if ValueNode.BerTag = CN_BER_TAG_BIT_STRING then
        begin
          SetLength(Buf, ValueNode.BerDataLength);
          if Length(Buf) >= 2 then
          begin
            ValueNode.CopyDataTo(@Buf[0]);
            // Buf[1] 要 shr Buf[0] 位
            Buf[1] := Buf[1] shr Buf[0];
            Move(Buf[0], KU, 1);
            StandardExt.KeyUsage := KU;
          end;
        end;
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_SUBJECTALTNAME, SizeOf(OID_EXT_SUBJECTALTNAME)) then
      begin
        StandardExt.SubjectAltName.Clear;
        for J := 0 to ValueNode.Count - 1 do
          StandardExt.SubjectAltName.Add(ValueNode[J].AsRawString);
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_ISSUERTALTNAME, SizeOf(OID_EXT_ISSUERTALTNAME)) then
      begin
        StandardExt.IssuerAltName.Clear;
        for J := 0 to ValueNode.Count - 1 do
          StandardExt.IssuerAltName.Add(ValueNode[J].AsRawString);
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_BASICCONSTRAINTS, SizeOf(OID_EXT_BASICCONSTRAINTS)) then
      begin
        for J := 0 to ValueNode.Count - 1 do
        begin
          if ValueNode[J].BerTag = CN_BER_TAG_BOOLEAN then
            StandardExt.BasicConstraintsCA := ValueNode[J].AsBoolean
          else if ValueNode[J].BerTag = CN_BER_TAG_INTEGER then
            StandardExt.BasicConstraintsPathLen := ValueNode[J].AsInteger;
        end;
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_CRLDISTRIBUTIONPOINTS, SizeOf(OID_EXT_CRLDISTRIBUTIONPOINTS)) then
      begin
        StandardExt.CRLDistributionPoints.Clear;
        for J := 0 to ValueNode.Count - 1 do
        begin
          if ValueNode[J].Count = 1 then
            if ValueNode[J][0].Count = 1 then
              if ValueNode[J][0][0].Count = 1 then
                StandardExt.CRLDistributionPoints.Add(ValueNode[J][0][0][0].AsRawString);
        end;
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_CERTIFICATEPOLICIES, SizeOf(OID_EXT_CERTIFICATEPOLICIES)) then
      begin
        // TODO: 解析复杂的 CERTIFICATEPOLICIES
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_AUTHORITYKEYIDENTIFIER, SizeOf(OID_EXT_AUTHORITYKEYIDENTIFIER)) then
      begin
        if ValueNode.Count = 1 then
          StandardExt.AuthorityKeyIdentifier := ValueNode[0].AsRawString;
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_EXTKEYUSAGE, SizeOf(OID_EXT_EXTKEYUSAGE)) then
      begin
        StandardExt.ExtendedKeyUsage := [];
        for J := 0 to ValueNode.Count - 1 do
        begin
          if CompareObjectIdentifier(ValueNode[J], @OID_EXT_EXT_KEYUSAGE_SERVERAUTH[0], SizeOf(OID_EXT_EXT_KEYUSAGE_SERVERAUTH)) then
            StandardExt.ExtendedKeyUsage := StandardExt.ExtendedKeyUsage + [ekuServerAuth]
          else if CompareObjectIdentifier(ValueNode[J], @OID_EXT_EXT_KEYUSAGE_CLIENTAUTH[0], SizeOf(OID_EXT_EXT_KEYUSAGE_CLIENTAUTH)) then
            StandardExt.ExtendedKeyUsage := StandardExt.ExtendedKeyUsage + [ekuClientAuth]
          else if CompareObjectIdentifier(ValueNode[J], @OID_EXT_EXT_KEYUSAGE_CODESIGNING[0], SizeOf(OID_EXT_EXT_KEYUSAGE_CODESIGNING)) then
            StandardExt.ExtendedKeyUsage := StandardExt.ExtendedKeyUsage + [ekuCodeSigning]
          else if CompareObjectIdentifier(ValueNode[J], @OID_EXT_EXT_KEYUSAGE_EMAILPROTECTION[0], SizeOf(OID_EXT_EXT_KEYUSAGE_EMAILPROTECTION)) then
            StandardExt.ExtendedKeyUsage := StandardExt.ExtendedKeyUsage + [ekuEmailProtection]
          else if CompareObjectIdentifier(ValueNode[J], @OID_EXT_EXT_KEYUSAGE_TIMESTAMPING[0], SizeOf(OID_EXT_EXT_KEYUSAGE_TIMESTAMPING)) then
            StandardExt.ExtendedKeyUsage := StandardExt.ExtendedKeyUsage + [ekuTimeStamping]
          else if CompareObjectIdentifier(ValueNode[J], @OID_EXT_EXT_KEYUSAGE_OCSPSIGNING[0], SizeOf(OID_EXT_EXT_KEYUSAGE_OCSPSIGNING)) then
            StandardExt.ExtendedKeyUsage := StandardExt.ExtendedKeyUsage + [ekuOCSPSigning];
        end;
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_AUTHORITYINFOACCESS, SizeOf(OID_EXT_AUTHORITYINFOACCESS)) then
      begin
        for J := 0 to ValueNode.Count - 1 do
        begin
          if ValueNode[J].Count = 2 then
          begin
            if CompareObjectIdentifier(ValueNode[J].Items[0], @OID_EXT_EXT_AUTHORITYINFOACCESS_OCSP[0], SizeOf(OID_EXT_EXT_AUTHORITYINFOACCESS_OCSP)) then
              PrivateInternetExt.AuthorityInformationAccessOcsp := ValueNode[J].Items[1].AsRawString
            else if CompareObjectIdentifier(ValueNode[J].Items[0], @OID_EXT_EXT_AUTHORITYINFOACCESS_CAISSUERS[0], SizeOf(OID_EXT_EXT_AUTHORITYINFOACCESS_CAISSUERS)) then
              PrivateInternetExt.AuthorityInformationAccessCaIssuers := ValueNode[J].Items[1].AsRawString
          end;
        end;
      end;
    end;
  end;
  SetLength(Buf, 0);
  Result := True;
end;

function CnCALoadCertificateSignRequestFromFile(const FileName: string;
  CertificateRequest: TCnCertificateRequest): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CnCALoadCertificateSignRequestFromStream(Stream, CertificateRequest);
  finally
    Stream.Free;
  end;
end;

{
  CSR 文件的大体格式如下：

  SEQUENCE
    SEQUENCE
      INTEGER0
      SEQUENCE
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.6 countryName(X.520 DN component)
            PrintableString  CN
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.8 stateOrProvinceName(X.520 DN component)
            PrintableString  ShangHai
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.7 localityName(X.520 DN component)
            PrintableString  ShangHai
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.10 organizationName(X.520 DN component)
            PrintableString  CnPack
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.11 organizationalUnitName(X.520 DN component)
            PrintableString  CnPack Team
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.3 commonName(X.520 DN component)
            PrintableString  cnpack.org
        SET
          SEQUENCE
           OBJECT IDENTIFIER  1.2.840.113549.1.9.1 emailAddress
           IA5String  master@cnpack.org
      SEQUENCE
        SEQUENCE
          OBJECT IDENTIFIER1.2.840.113549.1.1.1 rsaEncryption(PKCS #1) 或 1.2.840.10045.2.1 ecPublicKey
          NULL                                                         或 1.3.132.0.10 secp256k1
        BIT STRING
          SEQUENCE                        RSA 公钥（俩 INTEGER）或 ECC 公钥（一个 BITSTRING，没子节点）
            INTEGER
            INTEGER 65537
      [0]
    SEQUENCE
      OBJECT IDENTIFIER 1.2.840.113549.1.1.5sha1WithRSAEncryption(PKCS #1)
      NULL
    BIT STRING  Digest 值经过 RSA 加密后的结果，或经过 ECC 加密后的坐标点（ SEQUENCE 下再俩 INTEGER）
}
function CnCALoadCertificateSignRequestFromStream(Stream: TStream;
  CertificateRequest: TCnCertificateRequest): Boolean;
var
  IsRSA, IsECC: Boolean;
  Reader: TCnBerReader;
  MemStream, HashStream: TMemoryStream;
  DNRoot, PubNode, HashNode, SignNode: TCnBerReadNode;
  List: TStringList;
  CurveType: TCnEccCurveType;
  P: PByte;
begin
  Result := False;

  Reader := nil;
  MemStream := nil;
  HashStream := nil;

  try
    MemStream := TMemoryStream.Create;
    if not LoadPemStreamToMemory(Stream, PEM_CERTIFICATE_REQUEST_HEAD,
      PEM_CERTIFICATE_REQUEST_TAIL, MemStream) then
      Exit;

    Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size, True);
    Reader.ParseToTree;
    if (Reader.TotalCount >= 42) and (Reader.Items[2].BerTag = CN_BER_TAG_INTEGER)
      and (Reader.Items[2].AsInteger = 0) then // 就是有这么多项，版本号必须为 0
    begin
      DNRoot := Reader.Items[3];
      PubNode := DNRoot.GetNextSibling;
      if PubNode = nil then
        Exit;

      HashNode := Reader.Items[1].GetNextSibling;
      if (HashNode = nil) or (HashNode.Count = 0) then // RSA 的 Hash Node 还多个 Null 子节点
        Exit;

      SignNode := HashNode.GetNextSibling;
      if (SignNode = nil) or (SignNode.BerTag <> CN_BER_TAG_BIT_STRING)
        or (SignNode.BerDataLength <= 2) then
        Exit;

      IsRSA := False;
      if (PubNode.Count = 2) and (PubNode.Items[0].Count = 2) then
        IsRSA := CompareObjectIdentifier(PubNode.Items[0].Items[0],
          @OID_RSAENCRYPTION_PKCS1[0], SizeOf(OID_RSAENCRYPTION_PKCS1));

      IsECC := False;
      if (PubNode.Count = 2) and (PubNode.Items[0].Count = 2) then
        IsECC := CompareObjectIdentifier(PubNode.Items[0].Items[0],
          @OID_EC_PUBLIC_KEY[0], SizeOf(OID_EC_PUBLIC_KEY));

      if not IsRSA and not IsECC then // 算法不是 RSA 也不是 ECC
        Exit;

      CertificateRequest.IsRSA := IsRSA;
      if not IsRSA then
      begin
        CurveType := GetCurveTypeFromOID(PubNode.Items[0].Items[1].BerAddress,
          PubNode.Items[0].Items[1].BerLength);
        if CurveType = ctCustomized then
          Exit;
        CertificateRequest.EccCurveType := CurveType;  // 获得 ECC 曲线类型
      end;

      List := TStringList.Create;
      try
        ExtractDNValuesToList(DNRoot, List);

        CertificateRequest.CertificateRequestInfo.CountryName := List.Values[SDN_COUNTRYNAME];
        CertificateRequest.CertificateRequestInfo.StateOrProvinceName := List.Values[SDN_STATEORPROVINCENAME];
        CertificateRequest.CertificateRequestInfo.LocalityName := List.Values[SDN_LOCALITYNAME];
        CertificateRequest.CertificateRequestInfo.OrganizationName := List.Values[SDN_ORGANIZATIONNAME];
        CertificateRequest.CertificateRequestInfo.OrganizationalUnitName := List.Values[SDN_ORGANIZATIONALUNITNAME];
        CertificateRequest.CertificateRequestInfo.CommonName := List.Values[SDN_COMMONNAME];
        CertificateRequest.CertificateRequestInfo.EmailAddress := List.Values[SDN_EMAILADDRESS];
      finally
        List.Free;
      end;

      // 解开 RSA 或 ECC 公钥
      PubNode := PubNode.Items[1]; // BitString
      if IsRSA then
      begin
        if not ExtractRSAPublicKey(PubNode, CertificateRequest.RSAPublicKey) then
          Exit;
      end
      else
      begin
        if not ReadEccPublicKeyFromBitStringNode(PubNode, CertificateRequest.EccPublicKey) then
          Exit;
      end;

      Result := ExtractSignaturesByPublicKey(IsRSA, CertificateRequest.RSAPublicKey,
        CertificateRequest.EccPublicKey, HashNode, SignNode, CertificateRequest.FCASignType,
        CertificateRequest.FRSADigestType, CertificateRequest.FSignValue, CertificateRequest.FDigestValue,
        CertificateRequest.FSignLength, CertificateRequest.FDigestLength);

      if Result and not IsRSA then
      begin
        // ECC 得自行计算其 Hash
        HashStream := TMemoryStream.Create;
        P := Reader.Items[1].BerAddress;
        if not CalcDigestData(P, Reader.Items[1].BerLength, CertificateRequest.CASignType, HashStream) then
          Exit;

        FreeMemory(CertificateRequest.DigestValue);
        CertificateRequest.DigestValue := GetMemory(HashStream.Size);
        CertificateRequest.DigestLength := HashStream.Size;
        Move(HashStream.Memory^, CertificateRequest.DigestValue^, HashStream.Size);

        CertificateRequest.EccDigestType := GetEccSignTypeFromCASignType(CertificateRequest.CASignType);
        Result := True;
      end;
    end;
  finally
    Reader.Free;
    HashStream.Free;
    MemStream.Free;
  end;
end;

function CnCAVerifyCertificateSignRequestFile(const FileName: string): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CnCAVerifyCertificateSignRequestStream(Stream);
  finally
    Stream.Free;
  end;
end;

function CnCAVerifyCertificateSignRequestStream(Stream: TStream): Boolean;
var
  CSR: TCnCertificateRequest;
  Reader: TCnBerReader;
  MemStream, SignStream, InfoStream: TMemoryStream;
  InfoRoot: TCnBerReadNode;
  P: Pointer;
begin
  Result := False;
  CSR := nil;
  Reader := nil;
  MemStream := nil;
  SignStream := nil;
  InfoStream := nil;

  try
    CSR := TCnCertificateRequest.Create;
    if not CnCALoadCertificateSignRequestFromStream(Stream, CSR) then
      Exit;

    MemStream := TMemoryStream.Create;
    Stream.Position := 0;
    if not LoadPemStreamToMemory(Stream, PEM_CERTIFICATE_REQUEST_HEAD,
      PEM_CERTIFICATE_REQUEST_TAIL, MemStream) then
      Exit;

    Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size, True);
    Reader.ParseToTree;

    if Reader.TotalCount > 2 then
    begin
      InfoRoot := Reader.Items[1];
      SignStream := TMemoryStream.Create;

      if CSR.IsRSA then
      begin
        // 计算其 Hash
        P := InfoRoot.BerAddress;
        CalcDigestData(P, InfoRoot.BerLength, CSR.CASignType, SignStream);

        // 并与 RSA 解密出来的签名值手工对比
        if SignStream.Size = CSR.DigestLength then
          Result := CompareMem(SignStream.Memory, CSR.DigestValue, SignStream.Size);
      end
      else // ECC 直接验证数据块的签名与 Hash 值
      begin
        SignStream.Write(CSR.SignValue^, CSR.SignLength);
        InfoStream := TMemoryStream.Create;
        InfoStream.Write(InfoRoot.BerAddress^, InfoRoot.BerLength);
        InfoStream.Position := 0;
        SignStream.Position := 0;

        Result := CnEccVerifyStream(InfoStream, SignStream, CSR.EccCurveType, CSR.EccPublicKey,
          GetEccSignTypeFromCASignType(CSR.CASignType));
      end;
    end;
  finally
    CSR.Free;
    Reader.Free;
    MemStream.Free;
    SignStream.Free;
    InfoStream.Free;
  end;
end;

function CnCAVerifySelfSignedCertificateFile(const FileName: string): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CnCAVerifySelfSignedCertificateStream(Stream);
  finally
    Stream.Free;
  end;
end;

function CnCAVerifySelfSignedCertificateStream(Stream: TStream): Boolean;
var
  CRT: TCnCertificate;
  Reader: TCnBerReader;
  MemStream, SignStream, InfoStream: TMemoryStream;
  InfoRoot: TCnBerReadNode;
  P: Pointer;
begin
  Result := False;
  CRT := nil;
  Reader := nil;
  MemStream := nil;
  SignStream := nil;
  InfoStream := nil;

  try
    CRT := TCnCertificate.Create;
    if not CnCALoadCertificateFromStream(Stream, CRT) then
      Exit;

    if not CRT.IsSelfSigned then
      raise Exception.Create('NOT Self-Sign. Can NOT Verify.');

    MemStream := TMemoryStream.Create;
    Stream.Position := 0;
    if not LoadPemStreamToMemory(Stream, PEM_CERTIFICATE_HEAD,
      PEM_CERTIFICATE_TAIL, MemStream) then
      Exit;

    Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size, True);
    Reader.ParseToTree;

    if Reader.TotalCount > 2 then
    begin
      InfoRoot := Reader.Items[1];

      // 计算其 Hash
      SignStream := TMemoryStream.Create;

      if CRT.IsRSA then // RSA 自签名证书的散列值是能从证书里解密出来的，对比计算值即可
      begin
        P := InfoRoot.BerAddress;
        CalcDigestData(P, InfoRoot.BerLength, CRT.CASignType, SignStream);
        if SignStream.Size = CRT.DigestLength then
          Result := CompareMem(SignStream.Memory, CRT.DigestValue, SignStream.Size);
      end
      else // ECC 自签名证书里没有散列值，字段里的散列值是我们计算出来的没有对比意义，需要按 ECC 的方式验证签名值
      begin
        SignStream.Write(CRT.SignValue^, CRT.SignLength);
        InfoStream := TMemoryStream.Create;
        InfoStream.Write(InfoRoot.BerAddress^, InfoRoot.BerLength);
        InfoStream.Position := 0;
        SignStream.Position := 0;

        Result := CnEccVerifyStream(InfoStream, SignStream, CRT.BasicCertificate.SubjectEccCurveType,
          CRT.BasicCertificate.SubjectEccPublicKey, GetEccSignTypeFromCASignType(CRT.CASignType));
      end;
    end;
  finally
    CRT.Free;
    Reader.Free;
    MemStream.Free;
    SignStream.Free;
    InfoStream.Free;
  end;
end;

function CnCAVerifyCertificateFile(const FileName: string;
  ParentPublicKey: TCnRSAPublicKey): Boolean; overload;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CnCAVerifyCertificateStream(Stream, ParentPublicKey);
  finally
    Stream.Free;
  end;
end;

function CnCAVerifyCertificateFile(const FileName: string;
  ParentPublicKey: TCnEccPublicKey; ParentCurveType: TCnEccCurveType): Boolean; overload;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CnCAVerifyCertificateStream(Stream, ParentPublicKey, ParentCurveType);
  finally
    Stream.Free;
  end;
end;

function CnCAVerifyCertificateStream(Stream: TStream;
  ParentPublicKey: TCnRSAPublicKey): Boolean; overload;
var
  CRT: TCnCertificate;
  Reader: TCnBerReader;
  Root, InfoRoot, SignAlgNode, SignValueNode: TCnBerReadNode;
  MemStream, SignStream, InfoStream: TMemoryStream;
  P: Pointer;
begin
  Result := False;
  if ParentPublicKey = nil then
    Exit;

  CRT := nil;
  Reader := nil;
  MemStream := nil;
  SignStream := nil;
  InfoStream := nil;

  try
    CRT := TCnCertificate.Create;
    if not CnCALoadCertificateFromStream(Stream, CRT) then
      Exit;

    if not CRT.IsRSA then
      raise Exception.Create('NOT RSA. Can NOT Verify using RSA Key.');

    MemStream := TMemoryStream.Create;
    Stream.Position := 0;
    if not LoadPemStreamToMemory(Stream, PEM_CERTIFICATE_HEAD,
      PEM_CERTIFICATE_TAIL, MemStream) then
      Exit;

    Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size, True);
    Reader.ParseToTree;

    if Reader.TotalCount > 2 then
    begin
      Root := Reader.Items[0];
      SignAlgNode := Root.Items[1];
      SignValueNode := Root.Items[2];

      // 计算其 Hash
      InfoRoot := Reader.Items[1];
      SignStream := TMemoryStream.Create;
      P := InfoRoot.BerAddress;
      CalcDigestData(P, InfoRoot.BerLength, CRT.CASignType, SignStream);

      // RSA 证书的散列值要用父公钥才能从证书里解密出来
      if not ExtractSignaturesByPublicKey(True, ParentPublicKey,
        nil, SignAlgNode, SignValueNode,
        CRT.FCASignType, CRT.FRSADigestType, CRT.FSignValue,
        CRT.FDigestValue, CRT.FSignLength, CRT.FDigestLength) then
        Exit;

      // 对比计算值
      if SignStream.Size = CRT.DigestLength then
        Result := CompareMem(SignStream.Memory, CRT.DigestValue, SignStream.Size);
    end;
  finally
    CRT.Free;
    Reader.Free;
    MemStream.Free;
    SignStream.Free;
    InfoStream.Free;
  end;
end;

function CnCAVerifyCertificateStream(Stream: TStream;
  ParentPublicKey: TCnEccPublicKey; ParentCurveType: TCnEccCurveType): Boolean; overload;
var
  CRT: TCnCertificate;
  Reader: TCnBerReader;
  MemStream, SignStream, InfoStream: TMemoryStream;
  InfoRoot: TCnBerReadNode;
begin
  Result := False;
  if (ParentPublicKey = nil) or (ParentCurveType = ctCustomized) then
    Exit;

  CRT := nil;
  Reader := nil;
  MemStream := nil;
  SignStream := nil;
  InfoStream := nil;

  try
    CRT := TCnCertificate.Create;
    if not CnCALoadCertificateFromStream(Stream, CRT) then
      Exit;

    if CRT.IsRSA then
      raise Exception.Create('NOT ECC. Can NOT Verify.');

    MemStream := TMemoryStream.Create;
    Stream.Position := 0;
    if not LoadPemStreamToMemory(Stream, PEM_CERTIFICATE_HEAD,
      PEM_CERTIFICATE_TAIL, MemStream) then
      Exit;

    Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size, True);
    Reader.ParseToTree;

    if Reader.TotalCount > 2 then
    begin
      InfoRoot := Reader.Items[1];

      // ECC 证书里没有散列值，字段里的散列值是我们计算出来的没有对比意义，需要按 ECC 的方式把原始数据塞进去验证签名值
      SignStream := TMemoryStream.Create;
      SignStream.Write(CRT.SignValue^, CRT.SignLength);
      InfoStream := TMemoryStream.Create;
      InfoStream.Write(InfoRoot.BerAddress^, InfoRoot.BerLength);
      InfoStream.Position := 0;
      SignStream.Position := 0;

      Result := CnEccVerifyStream(InfoStream, SignStream, ParentCurveType,
        ParentPublicKey, GetEccSignTypeFromCASignType(CRT.CASignType));
    end;
  finally
    CRT.Free;
    Reader.Free;
    MemStream.Free;
    SignStream.Free;
    InfoStream.Free;
  end;
end;

{ TCnCertificateBasicInfo }

procedure TCnCertificateBaseInfo.Assign(Source: TPersistent);
begin
  if Source is TCnCertificateBaseInfo then
  begin
    FCountryName := (Source as TCnCertificateBaseInfo).CountryName;
    FOrganizationName := (Source as TCnCertificateBaseInfo).OrganizationName;
    FEmailAddress := (Source as TCnCertificateBaseInfo).EmailAddress;
    FLocalityName := (Source as TCnCertificateBaseInfo).LocalityName;
    FCommonName := (Source as TCnCertificateBaseInfo).CommonName;
    FOrganizationalUnitName := (Source as TCnCertificateBaseInfo).OrganizationalUnitName;
    FStateOrProvinceName := (Source as TCnCertificateBaseInfo).StateOrProvinceName;
  end
  else
    inherited;
end;

function TCnCertificateBaseInfo.ToString: string;
begin
  Result := 'CountryName: ' + FCountryName;
  Result := Result + SCRLF + 'StateOrProvinceName: ' + FStateOrProvinceName;
  Result := Result + SCRLF + 'LocalityName: ' + FLocalityName;
  Result := Result + SCRLF + 'OrganizationName: ' + FOrganizationName;
  Result := Result + SCRLF + 'OrganizationalUnitName: ' + FOrganizationalUnitName;
  Result := Result + SCRLF + 'CommonName: ' + FCommonName;
  Result := Result + SCRLF + 'EmailAddress: ' + FEmailAddress;
end;

{ TCnCertificateRequest }

constructor TCnCertificateRequest.Create;
begin
  inherited;
  FCertificateRequestInfo := TCnCertificateRequestInfo.Create;
  FRSAPublicKey := TCnRSAPublicKey.Create;
  FEccPublicKey := TCnEccPublicKey.Create;
end;

destructor TCnCertificateRequest.Destroy;
begin
  FCertificateRequestInfo.Free;
  FEccPublicKey.Free;
  FRSAPublicKey.Free;
  FreeMemory(FSignValue);
  FreeMemory(FDigestValue);
  inherited;
end;

procedure TCnCertificateRequest.SetCertificateRequestInfo(
  const Value: TCnCertificateRequestInfo);
begin
  FCertificateRequestInfo.Assign(Value);
end;

procedure TCnCertificateRequest.SetEccPublicKey(
  const Value: TCnEccPublicKey);
begin
  FEccPublicKey.Assign(Value);
end;

procedure TCnCertificateRequest.SetRSAPublicKey(
  const Value: TCnRSAPublicKey);
begin
  FRSAPublicKey.Assign(Value);
end;

function TCnCertificateRequest.ToString: string;
begin
  Result := FCertificateRequestInfo.ToString;
  if IsRSA then
  begin
    Result := Result + SCRLF + 'RSA:';
    Result := Result + SCRLF + 'RSA Public Key Modulus: ' + FRSAPublicKey.PubKeyProduct.ToDec;
    Result := Result + SCRLF + 'RSA Public Key Exponent: ' + FRSAPublicKey.PubKeyExponent.ToDec;
  end
  else
  begin
    Result := Result + SCRLF + 'ECC: ' + GetEnumName(TypeInfo(TCnEccCurveType), Ord(FEccCurveType));
    Result := Result + SCRLF + 'Ecc Public Key: ' + CnEccPointToString(FEccPublicKey);
  end;

  Result := Result + SCRLF + 'CA Signature Type: ' + GetCASignNameFromSignType(FCASignType);
  Result := Result + SCRLF + 'Signature: ' + PrintHex(FSignValue, FSignLength);
  if FIsRSA then
    Result := Result + SCRLF + 'Signature Hash: ' + GetRSADigestNameFromSignDigestType(FRSADigestType)
  else
    Result := Result + SCRLF + 'Signature Hash: ' + GetEccDigestNameFromSignDigestType(FEccDigestType);
  Result := Result + SCRLF + 'Digest: ' + PrintHex(FDigestValue, FDigestLength);
end;

function GetCASignNameFromSignType(Sign: TCnCASignType): string;
begin
  case Sign of
    ctMd5RSA: Result := 'MD5 RSA';
    ctSha1RSA: Result := 'SHA1 RSA';
    ctSha256RSA: Result := 'SHA256 RSA';
    ctMd5Ecc: Result := 'MD5 ECDSA';
    ctSha1Ecc: Result := 'SHA1 ECDSA';
    ctSha256Ecc: Result := 'SHA256 ECDSA';
    ctSM2withSM3: Result := 'SM2 with SM3';
  else
    Result := '<Unknown>';
  end;
end;

{ TCnUTCTime }

procedure TCnUTCTime.SetDateTime(const Value: TDateTime);
var
  Year, Month, Day, Hour, Minute, Sec, MSec: Word;
begin
  FDateTime := Value;
  
  // 将时间日期转换成字符串并给 FUTCTimeString，使用 YYMMDDhhmm[ss]Z 的格式
  DecodeDate(FDateTime, Year, Month, Day);
  DecodeTime(FDateTime, Hour, Minute, Sec, MSec);

  Year := Year mod 100; // 只取后两位
  FUTCTimeString := Format('%2.2d%2.2d%2.2d%2.2d%2.2d', [Year, Month, Day, Hour, Minute]);
  if Sec <> 0 then
    FUTCTimeString := FUTCTimeString + Format('%2.2d', [Sec]);
  FUTCTimeString := FUTCTimeString + 'Z';
end;

procedure TCnUTCTime.SetUTCTimeString(const Value: string);
var
  Year, Month, Day, Hour, Minute, Sec, DeltaHour, DeltaMin: Word;
  Idx: Integer;
  Plus: Boolean;
  DeltaTime: TDateTime;
begin
  FUTCTimeString := Value;
  //  解析 String 到时间并给 FDateTime，格式是 YYMMDDhhmm[ss]Z 或 YYMMDDhhmm[ss](+|-)hhmm
  if Length(FUTCTimeString) > 10 then // 至少得有 11 个
  begin
    Idx := 1;
    Year := StrToInt(Copy(FUTCTimeString, Idx, 2)) + 2000;  // 1
    Inc(Idx, 2);
    Month := StrToInt(Copy(FUTCTimeString, Idx, 2));        // 3
    Inc(Idx, 2);
    Day := StrToInt(Copy(FUTCTimeString, Idx, 2));          // 5
    Inc(Idx, 2);
    Hour := StrToInt(Copy(FUTCTimeString, Idx, 2));         // 7
    Inc(Idx, 2);
    Minute := StrToInt(Copy(FUTCTimeString, Idx, 2));       // 9
    Inc(Idx, 2);

    Sec := 0;
    if FUTCTimeString[Idx] in ['0'..'9'] then   // 有 ss    // 11
    begin
      Sec := StrToInt(Copy(FUTCTimeString, Idx, 2));
      Inc(Idx, 2);
    end;

    if Idx <= Length(FUTCTimeString) then
    begin
      // 此时 Idx 直接（或越过可能的 ss）指向 Z 或 +-
      if FUTCTimeString[Idx] in ['+', '-'] then
      begin
        Plus := FUTCTimeString[Idx] = '+';
        Inc(Idx);
        DeltaHour := 0;
        DeltaMin := 0;
        if Idx <= Length(FUTCTimeString) then
        begin
          DeltaHour := StrToInt(Copy(FUTCTimeString, Idx, 2));
          Inc(Idx, 2);
          if Idx <= Length(FUTCTimeString) then
            DeltaMin := StrToInt(Copy(FUTCTimeString, Idx, 2));
        end;

        FDateTime := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Sec, 0);
        DeltaTime := EncodeTime(DeltaHour, DeltaMin, 0, 0);

        if Plus then
          FDateTime := FDateTime + DeltaTime
        else
          FDateTime := FDateTime - DeltaTime;
      end
      else if FUTCTimeString[Idx] = 'Z' then
        FDateTime := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Sec, 0);
    end;
  end;
end;

function CnCALoadCertificateFromFile(const FileName: string;
  Certificate: TCnCertificate): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CnCALoadCertificateFromStream(Stream, Certificate);
  finally
    Stream.Free;
  end;
end;

function CnCALoadCertificateFromStream(Stream: TStream;
  Certificate: TCnCertificate): Boolean;
var
  Mem, HashStream: TMemoryStream;
  Reader: TCnBerReader;
  SerialNum: TCnBigNumber;
  Root, Node, VerNode, SerialNode: TCnBerReadNode;
  BSCNode, SignAlgNode, SignValueNode: TCnBerReadNode;
  List: TStringList;
  IsRSA, IsEcc: Boolean;
  P: Pointer;
  CurveType: TCnEccCurveType;
begin
  Result := False;

  Mem := nil;
  HashStream := nil;
  Reader := nil;

  try
    Mem := TMemoryStream.Create;
    if not LoadPemStreamToMemory(Stream, PEM_CERTIFICATE_HEAD, PEM_CERTIFICATE_TAIL, Mem) then
      Exit;

    Reader := TCnBerReader.Create(PByte(Mem.Memory), Mem.Size, True);
    Reader.ParseToTree;

    Root := Reader.Items[0];
    if Root.Count <> 3 then
      Exit;

    // 得到仨主要根节点
    BSCNode := Root.Items[0];
    SignAlgNode := Root.Items[1];
    SignValueNode := Root.Items[2];

    // BSC 内容
    if BSCNode.Count < 6 then
      Exit;

    // 判断 Version，可能没有
    Certificate.BasicCertificate.Version := CN_CRT_BASIC_VERSION_1;
    if (BSCNode.Items[0].BerTag = 0) and (BSCNode.Items[0].Count = 1) then
    begin
      SerialNode := BSCNode.Items[1];

      // A0 字节开头的一个节点，包含了一个 Integer 节点，不是标准包含下属的节点
      VerNode := BSCNode.Items[0].Items[0];
      Certificate.BasicCertificate.Version := VerNode.AsByte;
    end
    else
      SerialNode := BSCNode.Items[0];

    // 序列号
    SerialNum := TCnBigNumber.Create;
    try
      SerialNode.AsBigNumber(SerialNum);
      Certificate.BasicCertificate.SerialNumber := SerialNum.ToDec;
    finally
      FreeAndNil(SerialNum);
    end;

    // 基本信息中的签名算法字段
    Node := SerialNode.GetNextSibling;
    if (Node <> nil) and (Node.Count >= 1) then
    begin
      Certificate.CASignType := ExtractCASignType(Node.Items[0]);
      Certificate.IsRSA := Certificate.CASignType in RSA_CA_TYPES;
    end
    else
      Exit;

    // 解析众多其它字段
    List := TStringList.Create;
    try
      Node := Node.GetNextSibling; // 签名算法节点后的同级节点是 Issuer
      ExtractDNValuesToList(Node, List);
      Certificate.BasicCertificate.Issuer.CountryName := List.Values[SDN_COUNTRYNAME];
      Certificate.BasicCertificate.Issuer.StateOrProvinceName := List.Values[SDN_STATEORPROVINCENAME];
      Certificate.BasicCertificate.Issuer.LocalityName := List.Values[SDN_LOCALITYNAME];
      Certificate.BasicCertificate.Issuer.OrganizationName := List.Values[SDN_ORGANIZATIONNAME];
      Certificate.BasicCertificate.Issuer.OrganizationalUnitName := List.Values[SDN_ORGANIZATIONALUNITNAME];
      Certificate.BasicCertificate.Issuer.CommonName := List.Values[SDN_COMMONNAME];
      Certificate.BasicCertificate.Issuer.EmailAddress := List.Values[SDN_EMAILADDRESS];

      Node := Node.GetNextSibling; // Issuer 节点后的同级节点是俩 UTC Time
      if Node.Count = 2 then
      begin
        Certificate.BasicCertificate.NotBefore.UTCTimeString := Node.Items[0].AsString;
        Certificate.BasicCertificate.NotAfter.UTCTimeString := Node.Items[1].AsString;
      end;

      Node := Node.GetNextSibling; // UTC Time 节点后的同级节点是 Subject
      ExtractDNValuesToList(Node, List);
      Certificate.BasicCertificate.Subject.CountryName := List.Values[SDN_COUNTRYNAME];
      Certificate.BasicCertificate.Subject.StateOrProvinceName := List.Values[SDN_STATEORPROVINCENAME];
      Certificate.BasicCertificate.Subject.LocalityName := List.Values[SDN_LOCALITYNAME];
      Certificate.BasicCertificate.Subject.OrganizationName := List.Values[SDN_ORGANIZATIONNAME];
      Certificate.BasicCertificate.Subject.OrganizationalUnitName := List.Values[SDN_ORGANIZATIONALUNITNAME];
      Certificate.BasicCertificate.Subject.CommonName := List.Values[SDN_COMMONNAME];
      Certificate.BasicCertificate.Subject.EmailAddress := List.Values[SDN_EMAILADDRESS];
    finally
      List.Free;
    end;

    Node := Node.GetNextSibling; // Subject 节点后的同级节点是被签发者的公钥
    IsRSA := False;
    if (Node.Count = 2) and (Node.Items[0].Count = 2) then
      IsRSA := CompareObjectIdentifier(Node.Items[0].Items[0],
        @OID_RSAENCRYPTION_PKCS1[0], SizeOf(OID_RSAENCRYPTION_PKCS1));

    IsECC := False;
    if (Node.Count = 2) and (Node.Items[0].Count = 2) then
      IsECC := CompareObjectIdentifier(Node.Items[0].Items[0],
        @OID_EC_PUBLIC_KEY[0], SizeOf(OID_EC_PUBLIC_KEY));

    if not IsRSA and not IsECC then // 被签发者的算法不是 RSA 也不是 ECC
      Exit;

    Certificate.BasicCertificate.SubjectIsRSA := IsRSA;
    if not IsRSA then
    begin
      CurveType := GetCurveTypeFromOID(Node.Items[0].Items[1].BerAddress,
        Node.Items[0].Items[1].BerLength);
      if CurveType = ctCustomized then
        Exit;
      Certificate.BasicCertificate.SubjectEccCurveType := CurveType;  // 获得 ECC 曲线类型
    end;

    // 解开被签发者的公钥
    Node := Node.Items[1]; // 指向 BitString
    if IsRSA then
    begin
      if not ExtractRSAPublicKey(Node, Certificate.BasicCertificate.SubjectRSAPublicKey) then
        Exit;
    end
    else
    begin
      if not ReadEccPublicKeyFromBitStringNode(Node, Certificate.BasicCertificate.SubjectEccPublicKey) then
        Exit;
    end;

    // RSA 自签名证书可以解开散列值，ECC 的没有
    if Certificate.IsSelfSigned then
    begin
      Result := ExtractSignaturesByPublicKey(IsRSA, Certificate.BasicCertificate.SubjectRSAPublicKey,
        Certificate.BasicCertificate.SubjectEccPublicKey, SignAlgNode, SignValueNode,
        Certificate.FCASignType, Certificate.FRSADigestType, Certificate.FSignValue,
        Certificate.FDigestValue, Certificate.FSignLength, Certificate.FDigestLength);

      if Result and not IsRSA then
      begin
        // ECC 得自行计算其 Hash
        HashStream := TMemoryStream.Create;
        P := Reader.Items[1].BerAddress;
        if not CalcDigestData(P, Reader.Items[1].BerLength, Certificate.CASignType, HashStream) then
          Exit;

        FreeMemory(Certificate.DigestValue);
        Certificate.DigestValue := GetMemory(HashStream.Size);
        Certificate.DigestLength := HashStream.Size;
        Move(HashStream.Memory^, Certificate.DigestValue^, HashStream.Size);

        Certificate.EccDigestType := GetEccSignTypeFromCASignType(Certificate.CASignType);
        Result := True;
      end;
    end
    else
    begin
      // 解开签名。注意证书不带签发机构的公钥，因此这儿无法解密拿到真正散列值
      Result := ExtractSignaturesByPublicKey(IsRSA, nil, nil, SignAlgNode, SignValueNode, Certificate.FCASignType,
        DummyDigestType, Certificate.FSignValue, DummyPointer, Certificate.FSignLength,
        DummyInteger);
    end;

    // 解开标准扩展与私有互联网扩展节点
    if Result then
    begin
      Node := (Node.Parent as TCnBerReadNode).GetNextSibling;
      if (Node <> nil) then  // BITString 又无需跳过了
      begin
        Reader.ManualParseNodeData(Node);
        if Node.Count = 1 then
          Node := Node.Items[0];

        Result := ExtractExtensions(Node, Certificate.BasicCertificate.StandardExtension,
          Certificate.BasicCertificate.PrivateInternetExtension);
      end;
    end;
  finally
    Mem.Free;
    HashStream.Free;
    Reader.Free;
  end;
end;

{ TCnCertificate }

constructor TCnCertificate.Create;
begin
  FBasicCertificate := TCnBasicCertificate.Create;
end;

destructor TCnCertificate.Destroy;
begin
  FBasicCertificate.Free;
  inherited;
end;

function TCnCertificate.GetIsSelfSigned: Boolean;
begin
  Result := (FBasicCertificate.Subject.CountryName = FBasicCertificate.Issuer.CountryName)
    and (FBasicCertificate.Subject.OrganizationName = FBasicCertificate.Issuer.OrganizationName)
    and (FBasicCertificate.Subject.CommonName = FBasicCertificate.Issuer.CommonName);
end;

function TCnCertificate.ToString: string;
begin
  if IsSelfSigned then
    Result := 'Self-Signature ';
  Result := Result + FBasicCertificate.ToString;
  Result := Result + SCRLF + 'CA Signature Type: ' + GetCASignNameFromSignType(FCASignType);
  Result := Result + SCRLF + 'Signature: ' + PrintHex(FSignValue, FSignLength);
  if FDigestValue <> nil then
  begin
    if FIsRSA then
      Result := Result + SCRLF + 'Hash: ' + GetRSADigestNameFromSignDigestType(FRSADigestType)
    else
      Result := Result + SCRLF + 'Hash: ' + GetEccDigestNameFromSignDigestType(FEccDigestType);
    Result := Result + SCRLF + 'Digest: ' + PrintHex(FDigestValue, FDigestLength);
  end
  else
    Result := Result + SCRLF + '<No Digest>';
end;

{ TCnRSABasicCertificate }

constructor TCnBasicCertificate.Create;
begin
  FNotBefore := TCnUTCTime.Create;
  FNotAfter := TCnUTCTime.Create;
  FIssuer := TCnCertificateIssuerInfo.Create;
  FSubject := TCnCertificateSubjectInfo.Create;
  FSubjectRSAPublicKey := TCnRSAPublicKey.Create;
  FSubjectEccPublicKey := TCnEccPublicKey.Create;
  FStandardExtension := TCnCertificateStandardExtensions.Create;
  FPrivateInternetExtension := TCnCertificatePrivateInternetExtensions.Create;
end;

destructor TCnBasicCertificate.Destroy;
begin
  FPrivateInternetExtension.Free;
  FStandardExtension.Free;
  FIssuer.Free;
  FSubjectEccPublicKey.Free;
  FSubjectRSAPublicKey.Free;
  FSubject.Free;
  FNotBefore.Free;
  FNotAfter.Free;
  inherited;
end;

function TCnBasicCertificate.ToString: string;
begin
  Result := 'Version: ' + IntToStr(FVersion);
  Result := Result + SCRLF + 'SerialNumber: ' + FSerialNumber;
  Result := Result + SCRLF + 'Issuer: ';
  Result := Result + SCRLF + FIssuer.ToString;
  Result := Result + SCRLF + 'IssuerUniqueID: ' + FIssuerUniqueID;
  Result := Result + SCRLF + 'Validity From: ' + DateTimeToStr(FNotBefore.DateTime) + ' To: ' + DateTimeToStr(FNotAfter.DateTime);
  Result := Result + SCRLF + 'Subject: ';
  Result := Result + SCRLF + FSubject.ToString;
  Result := Result + SCRLF + 'SubjectUniqueID: ' + FSubjectUniqueID;
  Result := Result + SCRLF + 'Subject Public Key Modulus: ' + SubjectRSAPublicKey.PubKeyProduct.ToDec;
  Result := Result + SCRLF + 'Subject Public Key Exponent: ' + SubjectRSAPublicKey.PubKeyExponent.ToDec;
  Result := Result + SCRLF + FStandardExtension.ToString;
  Result := Result + SCRLF + FPrivateInternetExtension.ToString;
end;

{ TCnCertificatePrivateInternetExtensions }

function TCnCertificatePrivateInternetExtensions.ToString: string;
begin
  Result := 'AuthorityInformationAccess Ocsp: ' + FAuthorityInformationAccessOcsp;
  Result := Result + SCRLF + 'AuthorityInformationAccess CaIssusers: ' + FAuthorityInformationAccessCaIssuers;
end;

{ TCnCertificateStandardExtensions }

constructor TCnCertificateStandardExtensions.Create;
begin
  inherited;
  FSubjectAltName := TStringList.Create;
  FIssuerAltName := TStringList.Create;
  FCRLDistributionPoints := TStringList.Create;
end;

destructor TCnCertificateStandardExtensions.Destroy;
begin
  FCRLDistributionPoints.Free;
  FIssuerAltName.Free;
  FSubjectAltName.Free;
  inherited;
end;

function TCnCertificateStandardExtensions.ToString: string;
var
  SetVal: Integer;
begin
  SetVal := 0;
  Move(FKeyUsage, SetVal, SizeOf(FKeyUsage));
  Result := 'Standard Extension Key Usage: ' + IntToHex(SetVal, 2);
  SetVal := 0;
  Move(FExtendedKeyUsage, SetVal, SizeOf(FExtendedKeyUsage));
  Result := Result + SCRLF + 'Extended Key Usage: ' + IntToHex(SetVal, 2);
  Result := Result + SCRLF + 'Basic Constraints is CA: ' + InttoStr(Integer(FBasicConstraintsCA));
  Result := Result + SCRLF + 'Basic Constraints Path Len: ' + InttoStr(FBasicConstraintsPathLen);
  Result := Result + SCRLF + 'Authority Key Identifier: ' + PrintHex(Pointer(FAuthorityKeyIdentifier), Length(FAuthorityKeyIdentifier));
  Result := Result + SCRLF + 'Subject Key Identifier: ' + PrintHex(Pointer(FSubjectKeyIdentifier), Length(FSubjectKeyIdentifier));
  Result := Result + SCRLF + 'Subject Alternative Names: ' + SCRLF + FSubjectAltName.Text;
  Result := Result + SCRLF + 'Issuer Alternative Names: '+ SCRLF + FIssuerAltName.Text;
  Result := Result + SCRLF + 'CRL Distribution Points: '+ SCRLF + FCRLDistributionPoints.Text;
end;

{
  SET(1 elem)
    SEQUENCE(2 elem)
      OBJECT IDENTIFIER (X.520 DN component)
      PrintableString
}
function AddDNOidValueToWriter(AWriter: TCnBerWriter; DNRoot: TCnBerWriteNode;
  AOID: PByte; AOIDLen: Integer; const DN: string; BerTag: Integer = CN_BER_TAG_PRINTABLESTRING): TCnBerWriteNode;
begin
  Result := AWriter.AddContainerNode(CN_BER_TAG_SET, DNRoot);
  Result := AWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, Result);
  AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, AOID, AOIDLen, Result);
  AWriter.AddAnsiStringNode(BerTag, DN, Result);
end;

function CnCANewSelfSignedCertificate(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCRTFile: string; const CountryName: string; const
  StateOrProvinceName: string; const LocalityName: string; const OrganizationName:
  string; const OrganizationalUnitName: string; const CommonName: string; const
  EmailAddress: string; const IntSerialNum: string; NotBefore, NotAfter: TDateTime;
  CASignType: TCnCASignType = ctSha1RSA): Boolean;
var
  Writer: TCnBerWriter;
  Root, BasicNode, SubjectNode: TCnBerWriteNode;
  ValidNode, PubNode, IssuerNode, Node: TCnBerWriteNode;
  SerialNum: TCnBigNumber;
  UTCTime: TCnUTCTime;
  Stream: TMemoryStream;
  Buf: array of Byte;
begin
  Result := False;
  if NotAfter <= NotBefore then
    Exit;

  Writer := nil;
  SerialNum := nil;
  UTCTime := nil;
  Stream := nil;

  try
    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    BasicNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);

    // 版本忽略，写序列号
    SerialNum := TCnBigNumber.Create;
    SerialNum.SetDec(IntSerialNum);
    SetLength(Buf, SerialNum.GetBytesCount);
    SerialNum.ToBinary(@Buf[0]);
    Writer.AddBasicNode(CN_BER_TAG_INTEGER, @Buf[0], Length(Buf), BasicNode);

    // 写算法
    Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    AddCASignTypeOIDNodeToWriter(Writer, CASignType, Node);
    Writer.AddNullNode(Node);

    IssuerNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    ValidNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    SubjectNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);

    // 写签发者
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, CN_BER_TAG_IA5STRING);

    // 写有效时间
    UTCTime := TCnUTCTime.Create;
    UTCTime.SetDateTime(NotBefore);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, UTCTime.UTCTimeString, ValidNode);
    UTCTime.SetDateTime(NotAfter);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, UTCTime.UTCTimeString, ValidNode);

    // 写被签发者
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, CN_BER_TAG_IA5STRING);

    // 写公钥节点内容
    WriteRSAPublicKeyToNode(Writer, PubNode, PublicKey);

    // 计算并写签名值
    if not GenerateRSASignatureNode(Writer, Root, BasicNode, PrivateKey, CASignType) then
      Exit;

    // 保存
    Stream := TMemoryStream.Create;
    Writer.SaveToStream(Stream);
    Result := SaveMemoryToPemFile(OutCRTFile, PEM_CERTIFICATE_HEAD,
      PEM_CERTIFICATE_TAIL, Stream);
    Result := True;
  finally
    Writer.Free;
    Stream.Free;
    SerialNum.Free;
    UTCTime.Free;
  end;
end;

function CnCANewSelfSignedCertificate(PrivateKey: TCnEccPrivateKey; PublicKey:
  TCnEccPublicKey; CurveType: TCnEccCurveType; const OutCRTFile: string; const CountryName: string;
  const StateOrProvinceName: string; const LocalityName: string; const OrganizationName: string;
  const OrganizationalUnitName: string; const CommonName: string; const EmailAddress: string;
  const IntSerialNum: string; NotBefore, NotAfter: TDateTime; CASignType: TCnCASignType = ctSha1RSA): Boolean; overload;
var
  Writer: TCnBerWriter;
  Root, BasicNode, SubjectNode: TCnBerWriteNode;
  ValidNode, PubNode, IssuerNode, Node: TCnBerWriteNode;
  SerialNum: TCnBigNumber;
  UTCTime: TCnUTCTime;
  Stream: TMemoryStream;
  Buf: array of Byte;
begin
  Result := False;
  if NotAfter <= NotBefore then
    Exit;
  if CurveType = ctCustomized then
    Exit;

  Writer := nil;
  SerialNum := nil;
  UTCTime := nil;
  Stream := nil;

  try
    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    BasicNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);

    // 版本忽略，写序列号
    SerialNum := TCnBigNumber.Create;
    SerialNum.SetDec(IntSerialNum);
    SetLength(Buf, SerialNum.GetBytesCount);
    SerialNum.ToBinary(@Buf[0]);
    Writer.AddBasicNode(CN_BER_TAG_INTEGER, @Buf[0], Length(Buf), BasicNode);

    // 写算法
    Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    AddCASignTypeOIDNodeToWriter(Writer, CASignType, Node);
    Writer.AddNullNode(Node);

    IssuerNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    ValidNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    SubjectNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);

    // 写签发者
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, CN_BER_TAG_IA5STRING);

    // 写有效时间
    UTCTime := TCnUTCTime.Create;
    UTCTime.SetDateTime(NotBefore);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, UTCTime.UTCTimeString, ValidNode);
    UTCTime.SetDateTime(NotAfter);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, UTCTime.UTCTimeString, ValidNode);

    // 写被签发者
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, CN_BER_TAG_IA5STRING);

    // 写公钥节点内容
    if not WriteEccPublicKeyToNode(Writer, PubNode, PublicKey, CurveType) then
      Exit;

    // 计算并写签名值
    if not GenerateEccSignatureNode(Writer, Root, BasicNode, PrivateKey, CurveType, CASignType) then
      Exit;

    // 保存
    Stream := TMemoryStream.Create;
    Writer.SaveToStream(Stream);
    Result := SaveMemoryToPemFile(OutCRTFile, PEM_CERTIFICATE_HEAD,
      PEM_CERTIFICATE_TAIL, Stream);
    Result := True;
  finally
    Writer.Free;
    Stream.Free;
    SerialNum.Free;
    UTCTime.Free;
  end;
end;

{
   用 RSA CRT 证书签发 CSR 请求生成子证书 CRT。客户端证书文件中先写签发者再写被签发者
}
function CnCASignCertificate(PrivateKey: TCnRSAPrivateKey; const CRTFile: string;
  const CSRFile: string; const OutCRTFile: string; const IntSerialNum: string;
  NotBefore, NotAfter: TDateTime; CASignType: TCnCASignType = ctSha1RSA): Boolean;
var
  Writer: TCnBerWriter;
  Root, BasicNode, SubjectNode: TCnBerWriteNode;
  ValidNode, PubNode, IssuerNode, Node: TCnBerWriteNode;
  SerialNum: TCnBigNumber;
  UTCTime: TCnUTCTime;
  Stream: TMemoryStream;
  Buf: array of Byte;
  CSR: TCnCertificateRequest;
  CRT: TCnCertificate;
begin
  Result := False;
  if (PrivateKey = nil) or not FileExists(CRTFile) or not FileExists(CSRFile) then
    Exit;

  if not (CASignType in RSA_CA_TYPES) then
    Exit;

  CSR := nil;
  CRT := nil;
  Writer := nil;
  SerialNum := nil;
  UTCTime := nil;
  Stream := nil;

  try
    CSR := TCnCertificateRequest.Create;
    if not CnCAVerifyCertificateSignRequestFile(CSRFile) or not
      CnCALoadCertificateSignRequestFromFile(CSRFile, CSR) then
      Exit;

    CRT := TCnCertificate.Create;
    if not CnCALoadCertificateFromFile(CRTFile, CRT) then
      Exit;

    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    BasicNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);

    // 版本忽略，写序列号
    SerialNum := TCnBigNumber.Create;
    SerialNum.SetDec(IntSerialNum);
    SetLength(Buf, SerialNum.GetBytesCount);
    SerialNum.ToBinary(@Buf[0]);
    Writer.AddBasicNode(CN_BER_TAG_INTEGER, @Buf[0], Length(Buf), BasicNode);

    // 写算法。这个值或许取决于 CRT 对应的 Key 类型而无需单独参数传入？
    Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    AddCASignTypeOIDNodeToWriter(Writer, CASignType, Node);
    Writer.AddNullNode(Node);

    IssuerNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    ValidNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    SubjectNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);

    // 写签发者
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COUNTRYNAME[0],
      SizeOf(OID_DN_COUNTRYNAME), CRT.BasicCertificate.Issuer.CountryName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_STATEORPROVINCENAME[0],
      SizeOf(OID_DN_STATEORPROVINCENAME), CRT.BasicCertificate.Issuer.StateOrProvinceName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_LOCALITYNAME[0],
      SizeOf(OID_DN_LOCALITYNAME), CRT.BasicCertificate.Issuer.LocalityName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONNAME[0],
      SizeOf(OID_DN_ORGANIZATIONNAME), CRT.BasicCertificate.Issuer.OrganizationName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONALUNITNAME[0],
      SizeOf(OID_DN_ORGANIZATIONALUNITNAME), CRT.BasicCertificate.Issuer.OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COMMONNAME[0],
      SizeOf(OID_DN_COMMONNAME), CRT.BasicCertificate.Issuer.CommonName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_EMAILADDRESS[0],
      SizeOf(OID_DN_EMAILADDRESS), CRT.BasicCertificate.Issuer.EmailAddress, CN_BER_TAG_IA5STRING);

    // 写有效时间
    UTCTime := TCnUTCTime.Create;
    UTCTime.SetDateTime(NotBefore);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, UTCTime.UTCTimeString, ValidNode);
    UTCTime.SetDateTime(NotAfter);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, UTCTime.UTCTimeString, ValidNode);

    // 写被签发者
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COUNTRYNAME[0],
      SizeOf(OID_DN_COUNTRYNAME), CSR.CertificateRequestInfo.CountryName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_STATEORPROVINCENAME[0],
      SizeOf(OID_DN_STATEORPROVINCENAME), CSR.CertificateRequestInfo.StateOrProvinceName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_LOCALITYNAME[0],
      SizeOf(OID_DN_LOCALITYNAME), CSR.CertificateRequestInfo.LocalityName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONNAME[0],
      SizeOf(OID_DN_ORGANIZATIONNAME), CSR.CertificateRequestInfo.OrganizationName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONALUNITNAME[0],
      SizeOf(OID_DN_ORGANIZATIONALUNITNAME), CSR.CertificateRequestInfo.OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COMMONNAME[0],
      SizeOf(OID_DN_COMMONNAME), CSR.CertificateRequestInfo.CommonName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_EMAILADDRESS[0],
      SizeOf(OID_DN_EMAILADDRESS), CSR.CertificateRequestInfo.EmailAddress, CN_BER_TAG_IA5STRING);

    // 写被签发者的公钥节点内容，兼容被签发者 RSA/ECC 的情形
    if CSR.IsRSA then
      WriteRSAPublicKeyToNode(Writer, PubNode, CSR.RSAPublicKey)
    else
      WriteEccPublicKeyToNode(Writer, PubNode, CSR.EccPublicKey, CSR.EccCurveType);

    // 计算并写签名值
    if not GenerateRSASignatureNode(Writer, Root, BasicNode, PrivateKey, CASignType) then
      Exit;

    // 保存
    Stream := TMemoryStream.Create;
    Writer.SaveToStream(Stream);
    Result := SaveMemoryToPemFile(OutCRTFile, PEM_CERTIFICATE_HEAD,
      PEM_CERTIFICATE_TAIL, Stream);
    Result := True;
  finally
    Writer.Free;
    Stream.Free;
    SerialNum.Free;
    UTCTime.Free;
    CSR.Free;
    CRT.Free;
  end;
end;

{
   用 ECC CRT 证书签发 CSR 请求生成子证书 CRT。客户端证书文件中先写签发者再写被签发者
}
function CnCASignCertificate(PrivateKey: TCnEccPrivateKey; CurveType: TCnEccCurveType; const CRTFile: string;
  const CSRFile: string; const OutCRTFile: string; const IntSerialNum: string;
  NotBefore, NotAfter: TDateTime; CASignType: TCnCASignType = ctSha1Ecc): Boolean;
var
  Writer: TCnBerWriter;
  Root, BasicNode, SubjectNode: TCnBerWriteNode;
  ValidNode, PubNode, IssuerNode, Node: TCnBerWriteNode;
  SerialNum: TCnBigNumber;
  UTCTime: TCnUTCTime;
  Stream: TMemoryStream;
  Buf: array of Byte;
  CSR: TCnCertificateRequest;
  CRT: TCnCertificate;
begin
  Result := False;
  if (PrivateKey = nil) or not FileExists(CRTFile) or not FileExists(CSRFile) then
    Exit;

  if not (CASignType in ECC_CA_TYPES) then
    Exit;

  CSR := nil;
  CRT := nil;
  Writer := nil;
  SerialNum := nil;
  UTCTime := nil;
  Stream := nil;

  try
    CSR := TCnCertificateRequest.Create;
    if not CnCAVerifyCertificateSignRequestFile(CSRFile) or not
      CnCALoadCertificateSignRequestFromFile(CSRFile, CSR) then
      Exit;

    CRT := TCnCertificate.Create;
    if not CnCALoadCertificateFromFile(CRTFile, CRT) then
      Exit;

    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    BasicNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);

    // 版本忽略，写序列号
    SerialNum := TCnBigNumber.Create;
    SerialNum.SetDec(IntSerialNum);
    SetLength(Buf, SerialNum.GetBytesCount);
    SerialNum.ToBinary(@Buf[0]);
    Writer.AddBasicNode(CN_BER_TAG_INTEGER, @Buf[0], Length(Buf), BasicNode);

    // 写算法。这个值或许取决于 CRT 对应的 Key 类型而无需单独参数传入？
    Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    AddCASignTypeOIDNodeToWriter(Writer, CASignType, Node);
    Writer.AddNullNode(Node);

    IssuerNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    ValidNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    SubjectNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);

    // 写签发者
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COUNTRYNAME[0],
      SizeOf(OID_DN_COUNTRYNAME), CRT.BasicCertificate.Issuer.CountryName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_STATEORPROVINCENAME[0],
      SizeOf(OID_DN_STATEORPROVINCENAME), CRT.BasicCertificate.Issuer.StateOrProvinceName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_LOCALITYNAME[0],
      SizeOf(OID_DN_LOCALITYNAME), CRT.BasicCertificate.Issuer.LocalityName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONNAME[0],
      SizeOf(OID_DN_ORGANIZATIONNAME), CRT.BasicCertificate.Issuer.OrganizationName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONALUNITNAME[0],
      SizeOf(OID_DN_ORGANIZATIONALUNITNAME), CRT.BasicCertificate.Issuer.OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COMMONNAME[0],
      SizeOf(OID_DN_COMMONNAME), CRT.BasicCertificate.Issuer.CommonName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_EMAILADDRESS[0],
      SizeOf(OID_DN_EMAILADDRESS), CRT.BasicCertificate.Issuer.EmailAddress, CN_BER_TAG_IA5STRING);

    // 写有效时间
    UTCTime := TCnUTCTime.Create;
    UTCTime.SetDateTime(NotBefore);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, UTCTime.UTCTimeString, ValidNode);
    UTCTime.SetDateTime(NotAfter);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, UTCTime.UTCTimeString, ValidNode);

    // 写被签发者
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COUNTRYNAME[0],
      SizeOf(OID_DN_COUNTRYNAME), CSR.CertificateRequestInfo.CountryName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_STATEORPROVINCENAME[0],
      SizeOf(OID_DN_STATEORPROVINCENAME), CSR.CertificateRequestInfo.StateOrProvinceName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_LOCALITYNAME[0],
      SizeOf(OID_DN_LOCALITYNAME), CSR.CertificateRequestInfo.LocalityName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONNAME[0],
      SizeOf(OID_DN_ORGANIZATIONNAME), CSR.CertificateRequestInfo.OrganizationName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONALUNITNAME[0],
      SizeOf(OID_DN_ORGANIZATIONALUNITNAME), CSR.CertificateRequestInfo.OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COMMONNAME[0],
      SizeOf(OID_DN_COMMONNAME), CSR.CertificateRequestInfo.CommonName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_EMAILADDRESS[0],
      SizeOf(OID_DN_EMAILADDRESS), CSR.CertificateRequestInfo.EmailAddress, CN_BER_TAG_IA5STRING);

    // 写被签发者的公钥节点内容，兼容被签发者 RSA/ECC 的情形
    if CSR.IsRSA then
      WriteRSAPublicKeyToNode(Writer, PubNode, CSR.RSAPublicKey)
    else
      WriteEccPublicKeyToNode(Writer, PubNode, CSR.EccPublicKey, CSR.EccCurveType);

    // 计算并写签名值
    if not GenerateEccSignatureNode(Writer, Root, BasicNode, PrivateKey, CurveType, CASignType) then
      Exit;

    // 保存
    Stream := TMemoryStream.Create;
    Writer.SaveToStream(Stream);
    Result := SaveMemoryToPemFile(OutCRTFile, PEM_CERTIFICATE_HEAD,
      PEM_CERTIFICATE_TAIL, Stream);
    Result := True;
  finally
    Writer.Free;
    Stream.Free;
    SerialNum.Free;
    UTCTime.Free;
    CSR.Free;
    CRT.Free;
  end;
end;

end.
