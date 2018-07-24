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

unit CnCertificateAuthority;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：CA 证书认证单元
* 单元作者：刘啸
* 备    注：生成客户端 CSR 文件做证书签名请求，类似于命令：
*               openssl req -new -key clientkey.pem -out client.csr -config /c/Program\ Files/Git/ssl/openssl.cnf
*               其中 clientkey.pem 是预先生成的 RSA 私钥
*           一次性生成自签名的 crt 证书：
*               openssl req -new -x509 -keyout ca.key -out ca.crt -config /c/Program\ Files/Git/ssl/openssl.cnf
*           或利用现有 Key 对此 Key 生成的 CSR 请求文件进行自签名：
*               openssl x509 -req -days 365 -in client.csr -signkey clientkey.pem -out selfsigned.crt
* 开发平台：WinXP + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2018.07.22 V1.1
*               初步完成签发证书的功能
*           2018.06.15 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, Consts,
  CnBigNumber, CnRSA, CnBerUtils, CnMD5, CnSHA1, CnSHA2;

const
  CN_CRT_BASIC_VERSION_1      = 0;
  CN_CRT_BASIC_VERSION_2      = 1;
  CN_CRT_BASIC_VERSION_3      = 2;

type
  TCnCASignType = (ctMd5RSA, ctSha1RSA, ctSha256RSA);
  {* 证书签名使用的散列签名算法，ctSha1RSA 表示先 Sha1 再 RSA}

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

  TCnRSACertificateRequest = class(TObject)
  {* 描述证书请求中的信息，包括普通字段、公钥、摘要类型与签名等}
  private
    FCertificateRequestInfo: TCnCertificateRequestInfo;
    FPublicKey: TCnRSAPublicKey;
    FCASignType: TCnCASignType;
    FSignValue: Pointer;
    FSignLength: Integer;
    FDigestLength: Integer;
    FDigestValue: Pointer;
    FDigestType: TCnRSASignDigestType;
    procedure SetCertificateRequestInfo(const Value: TCnCertificateRequestInfo);
    procedure SetPublicKey(const Value: TCnRSAPublicKey); // 签名 Length 为 Key 的 Bit 数如 2048 Bit。
  public
    constructor Create;
    destructor Destroy; override;

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property CertificateRequestInfo: TCnCertificateRequestInfo
      read FCertificateRequestInfo write SetCertificateRequestInfo;
    {* 证书 DN 信息}
    property PublicKey: TCnRSAPublicKey read FPublicKey write SetPublicKey;
    {* 客户端公钥}
    property CASignType: TCnCASignType read FCASignType write FCASignType;
    {* 客户端使用的散列与签名算法}
    property SignValue: Pointer read FSignValue write FSignValue;
    {* 散列后签名的结果}
    property SignLength: Integer read FSignLength write FSignLength;
    {* 散列后签名的结果长度}
    property DigestType: TCnRSASignDigestType read FDigestType write FDigestType;
    {* 客户端散列使用的散列算法，应与 CASignType 意义相等}
    property DigestValue: Pointer read FDigestValue write FDigestValue;
    {* 散列值，中间结果，不直接存储于 CSR 文件中}
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

  TCnRSABasicCertificate = class(TObject)
  {* 证书中的基本信息域}
  private
    FSerialNumber: string;
    FNotAfter: TCnUTCTime;
    FNotBefore: TCnUTCTime;
    FVersion: Integer;
    FSubject: TCnCertificateSubjectInfo;
    FSubjectUniqueID: string;
    FIssuer: TCnCertificateIssuerInfo;
    FIssuerUniqueID: string;
    FSubjectPublicKey: TCnRSAPublicKey;
    FCASignType: TCnCASignType;
    FPrivateInternetExtension: TCnCertificatePrivateInternetExtensions;
    FStandardExtension: TCnCertificateStandardExtensions;
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
    property CASignType: TCnCASignType read FCASignType write FCASignType;
    {* 客户端使用的散列与签名算法，应该与证书外层的保持一直}
    property Subject: TCnCertificateSubjectInfo read FSubject write FSubject;
    {* 被签发者的基本信息}
    property SubjectPublicKey: TCnRSAPublicKey read FSubjectPublicKey write FSubjectPublicKey;
    {* 被签发者的公钥}
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

  TCnRSACertificate = class(TObject)
  {* 描述一完整的证书，注意其中并无签发者的公钥，公钥只有被签发者的}
  private
    FDigestLength: Integer;
    FSignLength: Integer;
    FDigestValue: Pointer;
    FSignValue: Pointer;
    FCASignType: TCnCASignType;
    FDigestType: TCnRSASignDigestType;
    FBasicCertificate: TCnRSABasicCertificate;
    function GetIsSelfSigned: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property IsSelfSigned: Boolean read GetIsSelfSigned;
    {* 是否自签名证书，使用签发者与被签发者信息是否相同来判断}

    property BasicCertificate: TCnRSABasicCertificate read FBasicCertificate;
    {* 证书基本信息类}
    property CASignType: TCnCASignType read FCASignType write FCASignType;
    {* 客户端使用的散列与签名算法}
    property SignValue: Pointer read FSignValue write FSignValue;
    {* 散列后签名的结果}
    property SignLength: Integer read FSignLength write FSignLength;
    {* 散列后签名的结果长度}
    property DigestType: TCnRSASignDigestType read FDigestType write FDigestType;
    {* 客户端散列使用的散列算法，应与 CASignType 意义相等}
    property DigestValue: Pointer read FDigestValue write FDigestValue;
    {* 散列值，中间结果，不直接存储于 CRT 文件中}
    property DigestLength: Integer read FDigestLength write FDigestLength;
    {* 散列值的长度}
  end;

function CnCANewCertificateSignRequest(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCSRFile: string; const CountryName: string; const
  StateOrProvinceName: string; const LocalityName: string; const OrganizationName:
  string; const OrganizationalUnitName: string; const CommonName: string; const
  EmailAddress: string; CASignType: TCnCASignType = ctSha1RSA): Boolean;
{* 根据公私钥与一些 DN 信息以及指定散列算法生成 CSR 格式的证书请求文件}

function CnCANewSelfSignedCertificate(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCRTFile: string; const CountryName: string; const
  StateOrProvinceName: string; const LocalityName: string; const OrganizationName:
  string; const OrganizationalUnitName: string; const CommonName: string; const
  EmailAddress: string; const IntSerialNum: string; NotBefore, NotAfter: TDateTime;
  CASignType: TCnCASignType = ctSha1RSA): Boolean;
{* 根据公私钥与一些 DN 信息以及指定散列算法生成 CRT 格式的自签名证书，目前只支持 v1 格式}

function CnCALoadCertificateSignRequestFromFile(const FileName: string;
  CertificateRequest: TCnRSACertificateRequest): Boolean;
{* 解析 PEM 格式的 CSR 文件并将内容放入 TCnRSACertificateRequest 对象中}

function CnCALoadCertificateSignRequestFromStream(Stream: TStream;
  CertificateRequest: TCnRSACertificateRequest): Boolean;
{* 解析 PEM 格式的 CSR 文件并将内容放入 TCnRSACertificateRequest 对象中}

function CnCAVerifyCertificateSignRequestFile(const FileName: string): Boolean;
{* 验证一 CSR 文件的内容是否合乎签名}

function CnCAVerifyCertificateSignRequestStream(Stream: TStream): Boolean;
{* 验证一 CSR 流的内容是否合乎签名}

function CnCAVerifySelfSignedCertificateFile(const FileName: string): Boolean;
{* 验证一自签名的 CRT 文件的内容是否合乎签名}

function CnCAVerifySelfSignedCertificateStream(Stream: TStream): Boolean;
{* 验证一自签名的 CRT 流的内容是否合乎签名}

function CnCALoadCertificateFromFile(const FileName: string;
  Certificate: TCnRSACertificate): Boolean;
{* 解析 PEM 格式的 CRT 证书文件并将内容放入 TCnRSACertificate 中}

function CnCALoadCertificateFromStream(Stream: TStream;
  Certificate: TCnRSACertificate): Boolean;
{* 解析 PEM 格式的 CRT 证书流并将内容放入 TCnRSACertificate 中}

function CnCASignCertificate(PrivateKey: TCnRSAPrivateKey; const CRTFile: string;
  const CSRFile: string; const OutCRTFile: string; const IntSerialNum: string;
  NotBefore, NotAfter: TDateTime; CASignType: TCnCASignType = ctSha1RSA): Boolean;
{* 用 CRT 证书内容与对应私钥签署证书请求，生成被签发证书，目前只支持 v1 格式}

// 其他辅助函数

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

  OID_SHA1_RSAENCRYPTION          : array[0..8] of Byte = (
    $2A, $86, $48, $86, $F7, $0D, $01, $01, $05
  ); // 1.2.840.113549.1.1.5
  OID_SHA256_RSAENCRYPTION        : array[0..8] of Byte = (
    $2A, $86, $48, $86, $F7, $0D, $01, $01, $0B
  ); // 1.2.840.113549.1.1.11

  SCRLF = #13#10;

  // 用于交换字符串数据的常量
  SDN_COUNTRYNAME                = 'CountryName';
  SDN_STATEORPROVINCENAME        = 'StateOrProvinceName';
  SDN_LOCALITYNAME               = 'LocalityName';
  SDN_ORGANIZATIONNAME           = 'OrganizationName';
  SDN_ORGANIZATIONALUNITNAME     = 'OrganizationalUnitName';
  SDN_COMMONNAME                 = 'CommonName';
  SDN_EMAILADDRESS               = 'EmailAddress';

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
    Result := Result + {$IFDEF UNICODE}string{$ENDIF}(Digits[(P[I] shr 4) and $0F] +
              Digits[P[I] and $0F]);
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
    ctMd5RSA:
      begin
        Md5 := MD5Buffer(Buffer, Count);
        outStream.Write(Md5, SizeOf(TMD5Digest));
        Result := True;
      end;
    ctSha1RSA:
      begin
        Sha1 := SHA1Buffer(Buffer, Count);
        outStream.Write(Sha1, SizeOf(TSHA1Digest));
        Result := True;
      end;
    ctSha256RSA:
      begin
        Sha256 := SHA256Buffer(Buffer, Count);
        outStream.Write(Sha256, SizeOf(TSHA256Digest));
        Result := True;
      end;
  end;
end;

function GetRSASignTypeFromCASignType(CASignType: TCnCASignType): TCnRSASignDigestType;
begin
  Result := sdtSHA1;
  case CASignType of
    ctMd5RSA:
      Result := sdtMD5;
    ctSha1RSA:
      Result := sdtSHA1;
    ctSha256RSA:
      Result := sdtSHA256;
  end;
end;

{ 写如下格式的公钥节点
  SEQUENCE(2 elem)    - PubNode
    SEQUENCE(2 elem)
      OBJECT IDENTIFIER 1.2.840.113549.1.1.1 rsaEncryption(PKCS #1)
      NULL
    BIT STRING(1 elem)
      SEQUENCE(2 elem)
        INTEGER
        INTEGER
}
procedure WritePublicKeyToNode(AWriter: TCnBerWriter; PubNode: TCnBerWriteNode;
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

function GenerateSignatureNode(AWriter: TCnBerWriter; Root, NodeToSign: TCnBerWriteNode;
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

function CnCANewCertificateSignRequest(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCSRFile: string; const CountryName: string; const
  StateOrProvinceName: string; const LocalityName: string; const OrganizationName:
  string; const OrganizationalUnitName: string; const CommonName: string; const
  EmailAddress: string; CASignType: TCnCASignType): Boolean;
var
  B: Byte;
  OutBuf: array of Byte;
  Writer, HashWriter: TCnBerWriter;
  Stream, DigestStream, ValueStream: TMemoryStream;
  Root, DNRoot, InfoRoot, PubNode: TCnBerWriteNode;

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
  DigestStream := nil;
  ValueStream := nil;
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
    WritePublicKeyToNode(Writer, PubNode, PublicKey);

    // 计算 InfoRoot 块的数字摘要并签名
    GenerateSignatureNode(Writer, Root, InfoRoot, PrivateKey, CASignType);

    // 保存
    Stream := TMemoryStream.Create;
    Writer.SaveToStream(Stream);
    Result := SaveMemoryToPemFile(OutCSRFile, PEM_CERTIFICATE_REQUEST_HEAD,
      PEM_CERTIFICATE_REQUEST_TAIL, Stream);
  finally
    Writer.Free;
    HashWriter.Free;
    Stream.Free;
    ValueStream.Free;
    DigestStream.Free;
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
            List.Values[SDN_COUNTRYNAME] := StrNode.AsPrintableString
          else if CompareObjectIdentifier(Node, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME)) then
            List.Values[SDN_STATEORPROVINCENAME] := StrNode.AsPrintableString
          else if CompareObjectIdentifier(Node, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME)) then
            List.Values[SDN_LOCALITYNAME] := StrNode.AsPrintableString
          else if CompareObjectIdentifier(Node, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME)) then
            List.Values[SDN_ORGANIZATIONNAME] := StrNode.AsPrintableString
          else if CompareObjectIdentifier(Node, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME)) then
            List.Values[SDN_ORGANIZATIONALUNITNAME] := StrNode.AsPrintableString
          else if CompareObjectIdentifier(Node, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME)) then
            List.Values[SDN_COMMONNAME] := StrNode.AsPrintableString
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
    Result := ctSha256RSA;
end;

// 从以下结构中解出公钥
{
BIT STRING -- PubNode
  SEQUENCE
    INTEGER
    INTEGER 65537
}
function ExtractPublicKey(PubNode: TCnBerReadNode; PublicKey: TCnRSAPublicKey): Boolean;
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

// 用已知公钥从类似于以下结构中拿出签名值解密并去除 PKCS1 对齐拿到摘要值
// 如果无公钥，则只取签名值，不解开
{
  SEQUENCE
    OBJECT IDENTIFIER 1.2.840.113549.1.1.5sha1WithRSAEncryption(PKCS #1)
    NULL
  BIT STRING
}
function ExtractSignaturesByPublicKey(PublicKey: TCnRSAPublicKey;
  HashNode, SignNode: TCnBerReadNode; out CASignType: TCnCASignType;
  out DigestType: TCnRSASignDigestType; out SignValue, DigestValue: Pointer;
  out SignLength, DigestLength: Integer): Boolean;
var
  P: Pointer;
  Reader: TCnBerReader;
  Node: TCnBerReadNode;
  OutBuf: array of Byte;
  OutLen: Integer;
begin
  Result := False;

  // 找到签名算法
  if HashNode.Count = 2 then
    CASignType := ExtractCASignType(HashNode.Items[0]);

  // 复制签名内容，跳过 BIT String 的前导对齐 0
  FreeMemory(SignValue);
  SignLength := SignNode.BerDataLength - 1;
  SignValue := GetMemory(SignLength);
  P := Pointer(Integer(SignNode.BerDataAddress) + 1);
  CopyMemory(SignValue, P, SignLength);

  // 无公钥时不解密，只把
  if PublicKey = nil then
  begin
    Result := True;
    Exit;
  end;

  // 解开 RSA 签名并去除 PKCS1 补齐的内容得到 DER 编码的 Hash 值与算法
  SetLength(OutBuf, PublicKey.BitsCount div 8);
  Reader := nil;

  try
    if CnRSADecryptData(SignValue, SignLength, @OutBuf[0], OutLen, PublicKey) then
    begin
      Reader := TCnBerReader.Create(@OutBuf[0], OutLen);
      Reader.ParseToTree;

      if Reader.TotalCount < 5 then
        Exit;

      Node := Reader.Items[2];
      DigestType := GetDigestSignTypeFromBerOID(Node.BerDataAddress,
        Node.BerDataLength);
      if DigestType = sdtNone then
        Exit;

      // 获取 Ber 解出的散列值
      Node := Reader.Items[4];
      FreeMemory(DigestValue);
      DigestLength := Node.BerDataLength;
      DigestValue := GetMemory(DigestLength);
      CopyMemory(DigestValue, Node.BerDataAddress, DigestLength);

      Result := True;
    end;
  finally
    SetLength(OutBuf, 0);
    Reader.Free;
  end;
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
        StandardExt.SubjectKeyIdentifier := ValueNode.AsString;
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
          StandardExt.SubjectAltName.Add(ValueNode[J].AsString);
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_ISSUERTALTNAME, SizeOf(OID_EXT_ISSUERTALTNAME)) then
      begin
        StandardExt.IssuerAltName.Clear;
        for J := 0 to ValueNode.Count - 1 do
          StandardExt.IssuerAltName.Add(ValueNode[J].AsString);
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
                StandardExt.CRLDistributionPoints.Add(ValueNode[J][0][0][0].AsString);
        end;
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_CERTIFICATEPOLICIES, SizeOf(OID_EXT_CERTIFICATEPOLICIES)) then
      begin
        // TODO: 解析复杂的 CERTIFICATEPOLICIES
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_AUTHORITYKEYIDENTIFIER, SizeOf(OID_EXT_AUTHORITYKEYIDENTIFIER)) then
      begin
        StandardExt.AuthorityKeyIdentifier := ValueNode.AsString;
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
              PrivateInternetExt.AuthorityInformationAccessOcsp := ValueNode[J].Items[1].AsString
            else if CompareObjectIdentifier(ValueNode[J].Items[0], @OID_EXT_EXT_AUTHORITYINFOACCESS_CAISSUERS[0], SizeOf(OID_EXT_EXT_AUTHORITYINFOACCESS_CAISSUERS)) then
              PrivateInternetExt.AuthorityInformationAccessCaIssuers := ValueNode[J].Items[1].AsString
          end;
        end;
      end;
    end;
  end;
  SetLength(Buf, 0);
  Result := True;
end;


function CnCALoadCertificateSignRequestFromFile(const FileName: string;
  CertificateRequest: TCnRSACertificateRequest): Boolean;
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
            OBJECT IDENTIFIER 2.5.4.6countryName(X.520 DN component)
            PrintableString  CN
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.8stateOrProvinceName(X.520 DN component)
            PrintableString  ShangHai
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.7localityName(X.520 DN component)
            PrintableString  ShangHai
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.10organizationName(X.520 DN component)
            PrintableString  CnPack
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.11organizationalUnitName(X.520 DN component)
            PrintableString  CnPack Team
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.3commonName(X.520 DN component)
            PrintableString  cnpack.org
        SET
          SEQUENCE
           OBJECT IDENTIFIER  1.2.840.113549.1.9.1 emailAddress
           IA5String  master@cnpack.org
      SEQUENCE
        SEQUENCE
          OBJECT IDENTIFIER1.2.840.113549.1.1.1rsaEncryption(PKCS #1)
          NULL
        BIT STRING
          SEQUENCE
            INTEGER
            INTEGER 65537
      [0]
    SEQUENCE
      OBJECT IDENTIFIER 1.2.840.113549.1.1.5sha1WithRSAEncryption(PKCS #1)
      NULL
    BIT STRING  Digest 值经过 RSA 加密后的结果
}
function CnCALoadCertificateSignRequestFromStream(Stream: TStream;
  CertificateRequest: TCnRSACertificateRequest): Boolean;
var
  IsRSA: Boolean;
  Reader: TCnBerReader;
  MemStream: TMemoryStream;
  DNRoot, PubNode, HashNode, SignNode: TCnBerReadNode;
  List: TStringList;
begin
  Result := False;

  Reader := nil;
  MemStream := nil;
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
      if (HashNode = nil) or (HashNode.Count <> 2) then
        Exit;

      SignNode := HashNode.GetNextSibling;
      if (SignNode = nil) or (SignNode.BerTag <> CN_BER_TAG_BIT_STRING)
        or (SignNode.BerDataLength <= 2) then
        Exit;

      IsRSA := False;
      if (PubNode.Count = 2) and (PubNode.Items[0].Count = 2) then
        IsRSA := CompareObjectIdentifier(PubNode.Items[0].Items[0],
          @OID_RSAENCRYPTION_PKCS1[0], SizeOf(OID_RSAENCRYPTION_PKCS1));

      if not IsRSA then // 算法不是 RSA
        Exit;

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

      // 解开公钥
      PubNode := PubNode.Items[1]; // BitString
      if not ExtractPublicKey(PubNode, CertificateRequest.PublicKey) then
        Exit;

      Result := ExtractSignaturesByPublicKey(CertificateRequest.PublicKey,
        HashNode, SignNode, CertificateRequest.FCASignType, CertificateRequest.FDigestType,
        CertificateRequest.FSignValue, CertificateRequest.FDigestValue,
        CertificateRequest.FSignLength, CertificateRequest.FDigestLength);
    end;
  finally
    Reader.Free;
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
  CSR: TCnRSACertificateRequest;
  Reader: TCnBerReader;
  MemStream, DigestStream: TMemoryStream;
  InfoRoot: TCnBerReadNode;
  P: Pointer;
begin
  Result := False;
  CSR := nil;
  Reader := nil;
  MemStream := nil;
  DigestStream := nil;

  try
    CSR := TCnRSACertificateRequest.Create;
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

      // 计算其 Hash
      DigestStream := TMemoryStream.Create;
      P := InfoRoot.BerAddress;
      CalcDigestData(P, InfoRoot.BerLength, CSR.CASignType, DigestStream);

      if DigestStream.Size = CSR.DigestLength then
        Result := CompareMem(DigestStream.Memory, CSR.DigestValue, DigestStream.Size);
    end;
  finally
    CSR.Free;
    Reader.Free;
    MemStream.Free;
    DigestStream.Free;
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
  CRT: TCnRSACertificate;
  Reader: TCnBerReader;
  MemStream, DigestStream: TMemoryStream;
  InfoRoot: TCnBerReadNode;
  P: Pointer;
begin
  Result := False;
  CRT := nil;
  Reader := nil;
  MemStream := nil;
  DigestStream := nil;

  try
    CRT := TCnRSACertificate.Create;
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
      DigestStream := TMemoryStream.Create;
      P := InfoRoot.BerAddress;
      CalcDigestData(P, InfoRoot.BerLength, CRT.CASignType, DigestStream);

      if DigestStream.Size = CRT.DigestLength then
        Result := CompareMem(DigestStream.Memory, CRT.DigestValue, DigestStream.Size);
    end;
  finally
    CRT.Free;
    Reader.Free;
    MemStream.Free;
    DigestStream.Free;
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

{ TCnRSACertificateRequest }

constructor TCnRSACertificateRequest.Create;
begin
  inherited;
  FCertificateRequestInfo := TCnCertificateRequestInfo.Create;
  FPublicKey := TCnRSAPublicKey.Create;
end;

destructor TCnRSACertificateRequest.Destroy;
begin
  FCertificateRequestInfo.Free;
  FPublicKey.Free;
  FreeMemory(FSignValue);
  FreeMemory(FDigestValue);
  inherited;
end;

procedure TCnRSACertificateRequest.SetCertificateRequestInfo(
  const Value: TCnCertificateRequestInfo);
begin
  FCertificateRequestInfo.Assign(Value);
end;

procedure TCnRSACertificateRequest.SetPublicKey(
  const Value: TCnRSAPublicKey);
begin
  FPublicKey.Assign(Value);
end;

function TCnRSACertificateRequest.ToString: string;
begin
  Result := FCertificateRequestInfo.ToString;
  Result := Result + SCRLF + 'Public Key Modulus: ' + FPublicKey.PubKeyProduct.ToDec;
  Result := Result + SCRLF + 'Public Key Exponent: ' + FPublicKey.PubKeyExponent.ToDec;
  Result := Result + SCRLF + 'CA Signature Type: ' + GetCASignNameFromSignType(FCASignType);
  Result := Result + SCRLF + 'Signature: ' + PrintHex(FSignValue, FSignLength);
  Result := Result + SCRLF + 'Signature Hash: ' + GetDigestNameFromSignDigestType(FDigestType);
  Result := Result + SCRLF + 'Digest: ' + PrintHex(FDigestValue, FDigestLength);
end;

function GetCASignNameFromSignType(Sign: TCnCASignType): string;
begin
  case Sign of
    ctMd5RSA: Result := 'MD5 RSA';
    ctSha1RSA: Result := 'SHA1 RSA';
    ctSha256RSA: Result := 'SHA256 RSA';
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

{ TCnRSACertificate }

function CnCALoadCertificateFromFile(const FileName: string;
  Certificate: TCnRSACertificate): Boolean;
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
  Certificate: TCnRSACertificate): Boolean;
var
  Mem: TMemoryStream;
  Reader: TCnBerReader;
  SerialNum: TCnBigNumber;
  Root, Node, VerNode, SerialNode: TCnBerReadNode;
  BSCNode, SignAlgNode, SignValueNode: TCnBerReadNode;
  List: TStringList;
  IsRSA: Boolean;
begin
  Result := False;

  Mem := nil;
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
    if (Node <> nil) and (Node.Count = 2) then
      Certificate.BasicCertificate.CASignType := ExtractCASignType(Node.Items[0]);

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

    Node := Node.GetNextSibling; // Subject 节点后的同级节点是公钥
    IsRSA := False;
    if (Node.Count = 2) and (Node.Items[0].Count = 2) then
      IsRSA := CompareObjectIdentifier(Node.Items[0].Items[0],
        @OID_RSAENCRYPTION_PKCS1[0], SizeOf(OID_RSAENCRYPTION_PKCS1));

    if not IsRSA then // 算法不是 RSA
      Exit;

    // 解开公钥
    Node := Node.Items[1]; // 指向 BitString
    if not ExtractPublicKey(Node, Certificate.BasicCertificate.SubjectPublicKey) then
      Exit;

    // 自签名证书可以解开散列值
    if Certificate.IsSelfSigned then
      Result := ExtractSignaturesByPublicKey(Certificate.BasicCertificate.SubjectPublicKey,
        SignAlgNode, SignValueNode, Certificate.FCASignType, Certificate.FDigestType,
        Certificate.FSignValue, Certificate.FDigestValue, Certificate.FSignLength,
        Certificate.FDigestLength)
    else
      // 解开签名。注意证书不带签发机构的公钥，因此这儿无法解密拿到真正散列值
      Result := ExtractSignaturesByPublicKey(nil, SignAlgNode, SignValueNode, Certificate.FCASignType,
        DummyDigestType, Certificate.FSignValue, DummyPointer, Certificate.FSignLength,
        DummyInteger);

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
    Reader.Free;
  end;
end;

{ TCnRSACertificate }

constructor TCnRSACertificate.Create;
begin
  FBasicCertificate := TCnRSABasicCertificate.Create;
end;

destructor TCnRSACertificate.Destroy;
begin
  FBasicCertificate.Free;
  inherited;
end;

function TCnRSACertificate.GetIsSelfSigned: Boolean;
begin
  Result := (FBasicCertificate.Subject.CountryName = FBasicCertificate.Issuer.CountryName)
    and (FBasicCertificate.Subject.OrganizationName = FBasicCertificate.Issuer.OrganizationName)
    and (FBasicCertificate.Subject.CommonName = FBasicCertificate.Issuer.CommonName);
end;

function TCnRSACertificate.ToString: string;
begin
  if IsSelfSigned then
    Result := 'Self-Signature ';
  Result := Result + FBasicCertificate.ToString;
  Result := Result + SCRLF + 'CA Signature Type: ' + GetCASignNameFromSignType(FCASignType);
  Result := Result + SCRLF + 'Signature: ' + PrintHex(FSignValue, FSignLength);
  if FDigestValue <> nil then
  begin
    Result := Result + SCRLF + 'Hash: ' + GetDigestNameFromSignDigestType(FDigestType);
    Result := Result + SCRLF + 'Digest: ' + PrintHex(FDigestValue, FDigestLength);
  end
  else
    Result := Result + SCRLF + '<No Digest>';
end;

{ TCnRSABasicCertificate }

constructor TCnRSABasicCertificate.Create;
begin
  FNotBefore := TCnUTCTime.Create;
  FNotAfter := TCnUTCTime.Create;
  FIssuer := TCnCertificateIssuerInfo.Create;
  FSubject := TCnCertificateSubjectInfo.Create;
  FSubjectPublicKey := TCnRSAPublicKey.Create;
  FStandardExtension := TCnCertificateStandardExtensions.Create;
  FPrivateInternetExtension := TCnCertificatePrivateInternetExtensions.Create;
end;

destructor TCnRSABasicCertificate.Destroy;
begin
  FPrivateInternetExtension.Free;
  FStandardExtension.Free;
  FIssuer.Free;
  FSubjectPublicKey.Free;
  FSubject.Free;
  FNotBefore.Free;
  FNotAfter.Free;
  inherited;
end;

function TCnRSABasicCertificate.ToString: string;
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
  Result := Result + SCRLF + 'Subject Public Key Modulus: ' + SubjectPublicKey.PubKeyProduct.ToDec;
  Result := Result + SCRLF + 'Subject Public Key Exponent: ' + SubjectPublicKey.PubKeyExponent.ToDec;
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

    SubjectNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    ValidNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    IssuerNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);

    // 写被签发者
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, CN_BER_TAG_IA5STRING);

    // 写有效时间
    UTCTime := TCnUTCTime.Create;
    UTCTime.SetDateTime(NotBefore);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, UTCTime.UTCTimeString, ValidNode);
    UTCTime.SetDateTime(NotAfter);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, UTCTime.UTCTimeString, ValidNode);

    // 写签发者
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, CN_BER_TAG_IA5STRING);

    // 写公钥节点内容
    WritePublicKeyToNode(Writer, PubNode, PublicKey);

    // 计算并写签名值
    if not GenerateSignatureNode(Writer, Root, BasicNode, PrivateKey, CASignType) then
      Exit;

    // 保存
    FreeAndNil(Stream);
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
  CSR: TCnRSACertificateRequest;
  CRT: TCnRSACertificate;
begin
  Result := False;
  if (PrivateKey = nil) or not FileExists(CRTFile) or not FileExists(CSRFile) then
    Exit;

  CSR := nil;
  CRT := nil;
  Writer := nil;
  SerialNum := nil;
  UTCTime := nil;
  Stream := nil;

  try
    CSR := TCnRSACertificateRequest.Create;
    if not CnCAVerifyCertificateSignRequestFile(CSRFile) or not
      CnCALoadCertificateSignRequestFromFile(CSRFile, CSR) then
      Exit;

    CRT := TCnRSACertificate.Create;
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

    // 写算法
    Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    AddCASignTypeOIDNodeToWriter(Writer, CASignType, Node);
    Writer.AddNullNode(Node);

    SubjectNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    ValidNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    IssuerNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);

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

    // 写有效时间
    UTCTime := TCnUTCTime.Create;
    UTCTime.SetDateTime(NotBefore);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, UTCTime.UTCTimeString, ValidNode);
    UTCTime.SetDateTime(NotAfter);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, UTCTime.UTCTimeString, ValidNode);

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

    // 写公钥节点内容
    WritePublicKeyToNode(Writer, PubNode, CRT.BasicCertificate.SubjectPublicKey);

    // 计算并写签名值
    if not GenerateSignatureNode(Writer, Root, BasicNode, PrivateKey, CASignType) then
      Exit;

    // 保存
    FreeAndNil(Stream);
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
