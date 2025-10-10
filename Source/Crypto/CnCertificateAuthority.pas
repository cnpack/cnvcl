{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnCertificateAuthority;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ����� RSA �� ECC �� CA ֤����֤��Ԫ
* ��Ԫ���ߣ�CnPack ������
* ��    ע�����ɿͻ��� CSR �ļ���֤��ǩ���������������
*
*               openssl req -new -key clientkey.pem -out client.csr -config /c/Program\ Files/Git/ssl/openssl.cnf
*
*               ���� clientkey.pem ��Ԥ�����ɵ� RSA �� ECC ˽Կ
*
*           һ����������ǩ���� crt ֤�飺
*
*               openssl req -new -x509 -keyout ca.key -out ca.crt -config /c/Program\ Files/Git/ssl/openssl.cnf
*
*           ���������� Key �Դ� Key ���ɵ� CSR �����ļ�������ǩ����
*
*               openssl x509 -req -days 365 -in client.csr -signkey clientkey.pem -out selfsigned.crt
*
*           ������ openssl ca ����ø�˽Կ���֤��ǩ�������� CSR ���� CRT ֤��
*
*           ֤�� CRT �ļ������ֶ�˵�����Ӵ��㷨�� SHA256 Ϊ����
*                        RSA ǩ RSA                RSA ǩ ECC                ECC ǩ RSA           ECC ǩ ECC
*     ����ǩ���ߵ����ͣ� sha256WithRSAEncryption   sha256WithRSAEncryption   ecdsaWithSHA256      ecdsaWithSHA256
*     ��ǩ���ߵ����ͣ�   rsaEncryption             ecPublicKey + ��������    rsaEncryption        ecPublicKey + ��������
*     ������������ͣ�   sha256WithRSAEncryption   sha256WithRSAEncryption   ecdsaWithSHA256      ecdsaWithSHA256
*
*           ע�⣺ǩ�������ͺ����������ֶ�������ͬ�ģ���ǩ���ߵ����Ͳ������Ӵ��㷨
*
*           ����֤֤��ʱ�����ø�֤����ı�ǩ���߹�Կ����֤��֤������ݵ��Ӵ�ֵ����֤���ǩ�������Ƿ�Ե��Ϻ�
*           ע����֧�� PKCS12 �淶��֤�鼰��Կ��װ��ʽ
*
* ����ƽ̨��WinXP + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2023.11.27 V1.6
*               �� PEM ��ʽ�� CRT ֤��ʱҲ֧�ֶ����� ASN.1 ��ʽ�� CER ֤��
*           2021.12.09 V1.5
*               ���� SM2/SM3 ֤�����͵Ľ���֧��
*           2020.04.17 V1.4
*               ֧�� ECC/RSA ֤�鸸��У��
*           2020.04.08 V1.3
*               ֧�� ECC ֤��������ǩ������ǩ��У����֤��ǩ����
*           2019.05.06 V1.2
*               ֧�� Win32/Win64/MacOS
*           2018.07.22 V1.1
*               �������ǩ��֤��Ĺ���
*           2018.06.15 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, TypInfo, {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  CnBigNumber, CnRSA, CnECC, CnBerUtils, CnPemUtils, CnMD5, CnSHA1, CnSHA2;

const
  CN_CRT_BASIC_VERSION_1      = 0;
  {* ֤��淶�汾�� 0}
  CN_CRT_BASIC_VERSION_2      = 1;
  {* ֤��淶�汾�� 1}
  CN_CRT_BASIC_VERSION_3      = 2;
  {* ֤��淶�汾�� 2}

type
  ECnCAException = class(Exception);
  {* ֤������쳣}

  TCnCASignType = (ctMd5RSA, ctSha1RSA, ctSha256RSA, ctMd5Ecc, ctSha1Ecc,
    ctSha256Ecc, ctSM2withSM3);
  {* ֤��ǩ��ʹ�õ��Ӵ�ǩ���㷨��ctSha1RSA ��ʾ�� Sha1 �� RSA���� ctSM2withSM3 ��ʾ�� SM3 �� SM2}

  TCnCASignTypes = set of TCnCASignType;
  {* ֤��ǩ��ʹ�õ��Ӵ�ǩ���㷨����}

  TCnCertificateBaseInfo = class(TPersistent)
  {* ����֤���а�������ͨ�ֶ���Ϣ}
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
    {* ����������ֵ������

       ������
         Source: TPersistent              - ����֮��ֵ��Դ����

       ����ֵ�����ޣ�
    }

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ת��Ϊ�ַ���

       ������
         ���ޣ�

       ����ֵ��string                     - ����֤���ֶ��ַ���
    }

  published
    property CountryName: string read FCountryName write FCountryName;
    {* ������}
    property StateOrProvinceName: string read FStateOrProvinceName write FStateOrProvinceName;
    {* ������ʡ��}
    property LocalityName: string read FLocalityName write FLocalityName;
    {* �������������}
    property OrganizationName: string read FOrganizationName write FOrganizationName;
    {* ��֯��}
    property OrganizationalUnitName: string read FOrganizationalUnitName write FOrganizationalUnitName;
    {* ��֯��λ��}
    property CommonName: string read FCommonName write FCommonName;
    {* ����}
    property EmailAddress: string read FEmailAddress write FEmailAddress;
    {* �����ʼ���ַ}
  end;

  // ������֤�����������

  TCnCertificateRequestInfo = class(TCnCertificateBaseInfo);
  {* ֤�������а����Ļ�����Ϣ}

  TCnCertificateRequest = class(TObject)
  {* ����֤�������е���Ϣ��������ͨ�ֶΡ���Կ��ժҪ������ǩ���ȣ����� RSA �� ECC}
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
    procedure SetEccPublicKey(const Value: TCnEccPublicKey);
    // ǩ�� Length Ϊ Key �� Bit ���� 2048 Bit��
  public
    constructor Create; virtual;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ת��Ϊ�ַ���

       ������
         ���ޣ�

       ����ֵ��string                     - ����֤�������ַ���
    }

    property IsRSA: Boolean read FIsRSA write FIsRSA;
    {* ������ RSA ���� ECC}
    property CertificateRequestInfo: TCnCertificateRequestInfo
      read FCertificateRequestInfo write SetCertificateRequestInfo;
    {* ֤�� DN ��Ϣ}
    property RSAPublicKey: TCnRSAPublicKey read FRSAPublicKey write SetRSAPublicKey;
    {* �ͻ��� RSA ��Կ}
    property EccPublicKey: TCnEccPublicKey read FEccPublicKey write SetEccPublicKey;
    {* �ͻ��� Ecc ��Կ}
    property EccCurveType: TCnEccCurveType read FEccCurveType write FEccCurveType;
    {* ECC �������ͣ���֧���Զ�������}
    property CASignType: TCnCASignType read FCASignType write FCASignType;
    {* �ͻ���ʹ�õ��Ӵ���ǩ���㷨}
    property SignValue: Pointer read FSignValue write FSignValue;
    {* �Ӵպ�ǩ���Ľ��������ʱ���ͷ�}
    property SignLength: Integer read FSignLength write FSignLength;
    {* �Ӵպ�ǩ���Ľ������}
    property RSADigestType: TCnRSASignDigestType read FRSADigestType write FRSADigestType;
    {* �ͻ��� RSA �Ӵ�ʹ�õ��Ӵ��㷨��Ӧ�� CASignType �������}
    property EccDigestType: TCnEccSignDigestType read FEccDigestType write FEccDigestType;
    {* �ͻ��� Ecc �Ӵ�ʹ�õ��Ӵ��㷨��Ӧ�� CASignType �������}
    property DigestValue: Pointer read FDigestValue write FDigestValue;
    {* �Ӵ�ֵ���м�������ֱ�Ӵ洢�� CSR �ļ��У�����ʱ���ͷ�}
    property DigestLength: Integer read FDigestLength write FDigestLength;
    {* �Ӵ�ֵ�ĳ���}
  end;

  // ������֤�������������������֤����֤������

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
  {* ���� Subject �� Issuer �Ļ�����Ϣ������}
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
    {* ���к�}
    property Title: string read FTitle write FTitle;
    {* ����}
    property SurName: string read FSurName write FSurName;
    {* ��}
    property GivenName: string read FGivenName write FGivenName;
    {* ��}
    property Initials: string read FInitials write FInitials;
    {* ����ĸ}
    property Pseudonym: string read FPseudonym write FPseudonym;
    {* ����}
    property GenerationQualifier: string read FGenerationQualifier write FGenerationQualifier;
    {* ������Ϣ}
  end;

  TCnCertificateSubjectInfo = class(TCnCertificateNameInfo);
  {* ֤�������а����ı�ǩ���ߵĻ�����Ϣ��Ҳ������� Name}

  TCnCertificateIssuerInfo = class(TCnCertificateNameInfo);
  {* ֤�������а�����ǩ���ߵĻ�����Ϣ��Ҳ������� Name}

  TCnUTCTime = class(TObject)
  {* ֤���д������ʱ��Ľ�����}
  private
    FUTCTimeString: string;
    FDateTime: TDateTime;
    procedure SetDateTime(const Value: TDateTime);
    procedure SetUTCTimeString(const Value: string);
  public
    property DateTime: TDateTime read FDateTime write SetDateTime;
    {* ����ʱ������}
    property UTCTimeString: string read FUTCTimeString write SetUTCTimeString;
    {* UTC ����ʱ��}
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
  {* ��Կ�÷�}
  TCnCerKeyUsages = set of TCnCerKeyUsage;
  {* ��Կ�÷�����}

  TCnExtendedKeyUsage = (ekuServerAuth, ekuClientAuth, ekuCodeSigning, ekuEmailProtection,
    ekuTimeStamping, ekuOCSPSigning);
  {* ��չ��Կ�÷�}
  TCnExtendedKeyUsages = set of TCnExtendedKeyUsage;
  {* ��չ��Կ�÷�����}

{
  ��׼��չ�����������ݣ�
  // Authority Key Identifier       ǩ������Կ��ʶ�����ֽ�����
  // Subject Key Identifier         ��ǩ���߹�Կ��ǣ��ֽ�����
  // Key Usage                      ��Կ�÷����� TCnCerKeyUsages
  // Certificate Policies
  // Policy Mappings
  // Subject Alternative Name       ��ǩ���ߵ�������ƣ��ַ����б�
  // Issuer Alternative Name        ǩ���ߵ�������ƣ��ַ����б�
  // Subject Directory Attributes
  // Basic Constraints              �������ƣ��Ƿ� CA �Լ�Ƕ�ײ���
  // Name Constraints
  // Policy Constraints
  // Extended Key Usage             ��ǿ����Կ�÷�����
  // CRL Distribution Points        CRL ���� URL���ַ����б�
  // Inhibit anyPolicy
  // Freshest CRL (a.k.a. Delta CRL Distribution Point)
}
  TCnCertificateStandardExtensions = class(TObject)
  {* ֤���׼��չ����}
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
    constructor Create; virtual;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ת��Ϊ�ַ���

       ������
         ���ޣ�

       ����ֵ��string                     - ����֤���׼��չ�ַ���
    }

    property KeyUsage: TCnCerKeyUsages read FKeyUsage write FKeyUsage;
    {* �޶��Ŀ�ִ�в���}
    property ExtendedKeyUsage: TCnExtendedKeyUsages read FExtendedKeyUsage write FExtendedKeyUsage;
    {* ��չ���޶���ִ�в���}
    property BasicConstraintsCA: Boolean read FBasicConstraintsCA write FBasicConstraintsCA;
    {* �ܷ�����ǩ������֤��}
    property BasicConstraintsPathLen: Integer read FBasicConstraintsPathLen write FBasicConstraintsPathLen;
    {* ������ǩ������֤��ʱ�����¼�֤���·������}
    property SubjectAltName: TStrings read FSubjectAltName;
    {* ��ǩ�������������}
    property IssuerAltName: TStrings read FIssuerAltName;
    {* ǩ�����������}
    property CRLDistributionPoints: TStrings read FCRLDistributionPoints;
    {* ֤������б�ַ���}
    property AuthorityKeyIdentifier: AnsiString read FAuthorityKeyIdentifier write FAuthorityKeyIdentifier;
    {* ��ʶ�䷢֤���֤��䷢�����Ĺ�Կ}
    property SubjectKeyIdentifier: AnsiString read FSubjectKeyIdentifier write FSubjectKeyIdentifier;
    {* Ψһ��ʶ֤���а����Ĺ�Կ���Ӵ�ֵ}
  end;

{
  ˽�л�������չ�����������ݣ�
  // Authority Information Access   ǩ���ߵ���Ϣ������ ocsp �� caIssuers �� URL
  // Subject Information Access     ûɶ����
}

  TCnCertificatePrivateInternetExtensions = class(TObject)
  {* ֤��˽�л�������չ}
  private
    FAuthorityInformationAccessCaIssuers: string;
    FAuthorityInformationAccessOcsp: string;
  public
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ת��Ϊ�ַ���

       ������
         ���ޣ�

       ����ֵ��string                     - ����֤��˽�л�������չ�ַ���
    }

    property AuthorityInformationAccessOcsp: string read FAuthorityInformationAccessOcsp
      write FAuthorityInformationAccessOcsp;
    {* �ϼ�ǩ��֤�� Ocsp �� URL}

    property AuthorityInformationAccessCaIssuers: string read FAuthorityInformationAccessCaIssuers
      write FAuthorityInformationAccessCaIssuers;
    {* �ϼ�ǩ������֤����� URL}
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
  {* ֤���еĻ�����Ϣ�򣬰���ǩ�����뱻ǩ���ߵ���Ϣ}
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
    constructor Create; virtual;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ת��Ϊ�ַ���

       ������
         ���ޣ�

       ����ֵ��string                     - ����֤�������Ϣ�ַ���
    }

    property Version: Integer read FVersion write FVersion;
    {* �汾�ţ�ֵ 0��1��2 ��ʾ�汾��Ϊ v1��v2��v3��Ĭ�� v1 ʱ��ʡ��
       �� extensions ʱ������ v3���� extensions ���� UniqueIdentifier ʱ v2
       �������ɰ汾 v3 ��}
    property SerialNumber: string read FSerialNumber write FSerialNumber;
    {* ���кţ�����Ӧ�������ͣ��������ַ�������}
    property SubjectIsRSA: Boolean read FSubjectIsRSA write FSubjectIsRSA;
    {* ��ǩ������ RSA ���� ECC��ע��û���Ӵ��㷨���ͣ��Ӵ��㷨��ǩ���߾���}
    property Subject: TCnCertificateSubjectInfo read FSubject write FSubject;
    {* ��ǩ���ߵĻ�����Ϣ}
    property SubjectRSAPublicKey: TCnRSAPublicKey read FSubjectRSAPublicKey write FSubjectRSAPublicKey;
    {* ��ǩ���ߵ� RSA ��Կ}
    property SubjectEccPublicKey: TCnEccPublicKey read FSubjectEccPublicKey write FSubjectEccPublicKey;
    {* ��ǩ���ߵ� ECC ��Կ}
    property SubjectEccCurveType: TCnEccCurveType read FSubjectEccCurveType write FSubjectEccCurveType;
    {* ��ǩ���ߵ� ECC ��������}
    property SubjectUniqueID: string read FSubjectUniqueID write FSubjectUniqueID;
    {* v2 ʱ��ǩ���ߵ�Ψһ ID}
    property Issuer: TCnCertificateIssuerInfo read FIssuer write FIssuer;
    {* ǩ���ߵĻ�����Ϣ}
    property IssuerUniqueID: string read FIssuerUniqueID write FIssuerUniqueID;
    {* v2 ʱǩ���ߵ�Ψһ ID}
    property NotBefore: TCnUTCTime read FNotBefore;
    {* ��Ч����ʼ}
    property NotAfter: TCnUTCTime read FNotAfter;
    {* ��Ч�ڽ���}

    property StandardExtension: TCnCertificateStandardExtensions read FStandardExtension;
    {* ��׼��չ���󼯺�}
    property PrivateInternetExtension: TCnCertificatePrivateInternetExtensions read FPrivateInternetExtension;
    {* ˽�л�������չ���󼯺�}
  end;

{
  Certificate  ::=  SEQUENCE
    tbsCertificate       TBSCertificate,
    signatureAlgorithm   AlgorithmIdentifier,
    signatureValue       BIT STRING
}

  TCnCertificate = class(TObject)
  {* ����һ������֤�飬ע�����в���ǩ���ߵĹ�Կ����Կֻ�б�ǩ���ߵ�}
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
    constructor Create; virtual;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ת��Ϊ�ַ���

       ������
         ���ޣ�

       ����ֵ��string                     - ����֤���ַ���
    }

    property IsSelfSigned: Boolean read GetIsSelfSigned;
    {* �Ƿ���ǩ��֤�飬ʹ��ǩ�����뱻ǩ������Ϣ�Ƿ���ͬ���ж�}
    property IsRSA: Boolean read FIsRSA write FIsRSA;
    {* �Ƿ��� RSA ֤�飬������ ECC ֤�顣����ֶ�ָǩ���ߵ�֤������}

    property BasicCertificate: TCnBasicCertificate read FBasicCertificate;
    {* ֤�������Ϣ�࣬����ǩ�����뱻ǩ���ߵ���Ϣ}
    property CASignType: TCnCASignType read FCASignType write FCASignType;
    {* ǩ����ʹ�õ��Ӵ���ǩ���㷨}
    property SignValue: Pointer read FSignValue write FSignValue;
    {* �Ӵպ�ǩ���Ľ��}
    property SignLength: Integer read FSignLength write FSignLength;
    {* �Ӵպ�ǩ���Ľ������}
    property RSADigestType: TCnRSASignDigestType read FRSADigestType write FRSADigestType;
    {* �ͻ����Ӵ�ʹ�õ��Ӵ��㷨��Ӧ�� CASignType �������}
    property EccDigestType: TCnEccSignDigestType read FEccDigestType write FEccDigestType;
    {* �ͻ��� Ecc �Ӵ�ʹ�õ��Ӵ��㷨��Ӧ�� CASignType �������}
    property DigestValue: Pointer read FDigestValue write FDigestValue;
    {* �Ӵ�ֵ���м�������ֱ�Ӵ洢�� CRT �ļ���}
    property DigestLength: Integer read FDigestLength write FDigestLength;
    {* �Ӵ�ֵ�ĳ���}
  end;

function CnCANewCertificateSignRequest(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCSRFile: string; const CountryName: string; const
  StateOrProvinceName: string; const LocalityName: string; const OrganizationName:
  string; const OrganizationalUnitName: string; const CommonName: string; const
  EmailAddress: string; CASignType: TCnCASignType = ctSha1RSA): Boolean; overload;
{* ���ݹ�˽Կ��һЩ DN ��Ϣ�Լ�ָ���Ӵ��㷨���� CSR ��ʽ�� RSA ֤�������ļ���

   ������
     PrivateKey: TCnRSAPrivateKey         - ��������֤�������ļ��� RSA ˽Կ
     PublicKey: TCnRSAPublicKey           - ��������֤�������ļ��� RSA ��Կ
     const OutCSRFile: string             - �����֤�������ļ���
     const CountryName: string            - ������
     const StateOrProvinceName: string    - ������ʡ��
     const LocalityName: string           - �������������
     const OrganizationName: string       - ��֯��
     const OrganizationalUnitName: string - ��֯��λ��
     const CommonName: string             - ����
     const EmailAddress: string           - �����ʼ���ַ
     CASignType: TCnCASignType            - ǩ���Ӵ��㷨����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function CnCANewCertificateSignRequest(PrivateKey: TCnEccPrivateKey; PublicKey:
  TCnEccPublicKey; CurveType: TCnEccCurveType; const OutCSRFile: string; const CountryName: string;
  const StateOrProvinceName: string; const LocalityName: string; const OrganizationName: string;
  const OrganizationalUnitName: string; const CommonName: string; const EmailAddress: string;
  CASignType: TCnCASignType = ctSha1Ecc): Boolean; overload;
{* ���ݹ�˽Կ��һЩ DN ��Ϣ�Լ�ָ���Ӵ��㷨���� CSR ��ʽ�� ECC ֤�������ļ���

   ������
     PrivateKey: TCnEccPrivateKey         - ��������֤�������ļ��� ECC ˽Կ
     PublicKey: TCnEccPublicKey           - ��������֤�������ļ��� ECC ��Կ
     CurveType: TCnEccCurveType           - ��������֤�������ļ�����Բ��������
     const OutCSRFile: string             - �����֤�������ļ���
     const CountryName: string            - ������
     const StateOrProvinceName: string    - ������ʡ��
     const LocalityName: string           - �������������
     const OrganizationName: string       - ��֯��
     const OrganizationalUnitName: string - ��֯��λ��
     const CommonName: string             - ����
     const EmailAddress: string           - �����ʼ���ַ
     CASignType: TCnCASignType            - ǩ���Ӵ��㷨����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function CnCANewSelfSignedCertificate(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCRTFile: string; const CountryName: string; const
  StateOrProvinceName: string; const LocalityName: string; const OrganizationName:
  string; const OrganizationalUnitName: string; const CommonName: string; const
  EmailAddress: string; const IntSerialNum: string; NotBefore: TDateTime; NotAfter: TDateTime;
  CASignType: TCnCASignType = ctSha1RSA): Boolean; overload;
{* ���ݹ�˽Կ��һЩ DN ��Ϣ�Լ�ָ���Ӵ��㷨���� RSA CRT ��ʽ����ǩ��֤�飬Ŀǰֻ֧�� v1 ��ʽ��

   ������
     PrivateKey: TCnRSAPrivateKey         - ����������ǩ��֤��� RSA ˽Կ
     PublicKey: TCnRSAPublicKey           - ����������ǩ��֤��� RSA ��Կ
     const OutCRTFile: string             - �����֤���ļ���
     const CountryName: string            - ������
     const StateOrProvinceName: string    - ������ʡ��
     const LocalityName: string           - �������������
     const OrganizationName: string       - ��֯��
     const OrganizationalUnitName: string - ��֯��λ��
     const CommonName: string             - ����
     const EmailAddress: string           - �����ʼ���ַ
     const IntSerialNum: string           - ���к�
     NotBefore: TDateTime                 - ֤����Ч�ڿ�ʼ������ʱ��
     NotAfter: TDateTime                  - ֤����Ч�ڽ���������ʱ��
     CASignType: TCnCASignType            - ǩ���Ӵ��㷨����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function CnCANewSelfSignedCertificate(PrivateKey: TCnEccPrivateKey; PublicKey:
  TCnEccPublicKey; CurveType: TCnEccCurveType; const OutCRTFile: string; const CountryName: string;
  const StateOrProvinceName: string; const LocalityName: string; const OrganizationName: string;
  const OrganizationalUnitName: string; const CommonName: string; const EmailAddress: string;
  const IntSerialNum: string; NotBefore: TDateTime; NotAfter: TDateTime;
  CASignType: TCnCASignType = ctSha1RSA): Boolean; overload;
{* ���ݹ�˽Կ��һЩ DN ��Ϣ�Լ�ָ���Ӵ��㷨���� ECC CRT ��ʽ����ǩ��֤�飬Ŀǰֻ֧�� v1 ��ʽ��

   ������
     PrivateKey: TCnEccPrivateKey         - ����������ǩ��֤��� ECC ˽Կ
     PublicKey: TCnEccPublicKey           - ����������ǩ��֤��� ECC ��Կ
     CurveType: TCnEccCurveType           - ����������ǩ��֤�����Բ��������
     const OutCRTFile: string             - �����֤���ļ���
     const CountryName: string            - ������
     const StateOrProvinceName: string    - ������ʡ��
     const LocalityName: string           - �������������
     const OrganizationName: string       - ��֯��
     const OrganizationalUnitName: string - ��֯��λ��
     const CommonName: string             - ����
     const EmailAddress: string           - �����ʼ���ַ
     const IntSerialNum: string           - ���к�
     NotBefore: TDateTime                 - ֤����Ч�ڿ�ʼ������ʱ��
     NotAfter: TDateTime                  - ֤����Ч�ڽ���������ʱ��
     CASignType: TCnCASignType            - ǩ���Ӵ��㷨����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function CnCALoadCertificateSignRequestFromFile(const FileName: string;
  CertificateRequest: TCnCertificateRequest): Boolean;
{* ���� PEM ��ʽ�� CSR �ļ��������ݼ����� TCnCertificateRequest �����С�

   ������
     const FileName: string                               - �������� PEM �ļ���
     CertificateRequest: TCnCertificateRequest            - ���ص�֤���������

   ����ֵ��Boolean                                        - ���ؼ����Ƿ�ɹ�
}

function CnCALoadCertificateSignRequestFromStream(Stream: TStream;
  CertificateRequest: TCnCertificateRequest): Boolean;
{* ���� PEM ��ʽ�� CSR ���������ݼ����� TCnCertificateRequest �����С�

   ������
     Stream: TStream                                      - �������� PEM ��
     CertificateRequest: TCnCertificateRequest            - ���ص�֤���������

   ����ֵ��Boolean                                        - ���ؼ����Ƿ�ɹ�
}

function CnCAVerifyCertificateSignRequestFile(const FileName: string): Boolean;
{* ��֤һ CSR �ļ��������Ƿ�Ϻ�ǩ����

   ������
     const FileName: string               - ����֤��֤�������ļ�

   ����ֵ��Boolean                        - �����Ƿ�Ϻ�ǩ��
}

function CnCAVerifyCertificateSignRequestStream(Stream: TStream): Boolean;
{* ��֤һ CSR ���������Ƿ�Ϻ�ǩ����

   ������
     Stream: TStream                      - ����֤��֤��������

   ����ֵ��Boolean                        - �����Ƿ�Ϻ�ǩ��
}

function CnCAVerifySelfSignedCertificateFile(const FileName: string): Boolean;
{* ��֤һ��ǩ���� CRT �ļ��������Ƿ�Ϻ�ǩ����

   ������
     const FileName: string               - ����֤����ǩ��֤���ļ�

   ����ֵ��Boolean                        - �����Ƿ�Ϻ�ǩ��
}

function CnCAVerifySelfSignedCertificateStream(Stream: TStream): Boolean;
{* ��֤һ��ǩ���� CRT ���������Ƿ�Ϻ�ǩ����

   ������
     Stream: TStream                      - ����֤����ǩ��֤����

   ����ֵ��Boolean                        - �����Ƿ�Ϻ�ǩ��
}

function CnCAVerifyCertificateFile(const FileName: string;
  ParentPublicKey: TCnRSAPublicKey): Boolean; overload;
{* �� RSA ǩ���߹�Կ��֤һ CRT �ļ��������Ƿ�Ϻ�ǩ����

   ������
     const FileName: string               - ����֤��֤���ļ�
     ParentPublicKey: TCnRSAPublicKey     - ������֤�� RSA ǩ���߹�Կ

   ����ֵ��Boolean                        - �����Ƿ�Ϻ�ǩ��
}

function CnCAVerifyCertificateFile(const FileName: string; ParentPublicKey: TCnEccPublicKey;
  ParentCurveType: TCnEccCurveType): Boolean; overload;
{* �� ECC ǩ���߹�Կ��֤һ CRT �ļ��������Ƿ�Ϻ�ǩ����

   ������
     const FileName: string               - ����֤��֤���ļ�
     ParentPublicKey: TCnEccPublicKey     - ������֤�� ECC ǩ���߹�Կ
     ParentCurveType: TCnEccCurveType     - ������֤��ǩ���ߵ���Բ��������

   ����ֵ��Boolean                        - �����Ƿ�Ϻ�ǩ��
}

function CnCAVerifyCertificateStream(Stream: TStream;
  ParentPublicKey: TCnRSAPublicKey): Boolean; overload;
{* �� RSA ǩ���߹�Կ��֤һ CRT ���������Ƿ�Ϻ�ǩ����

   ������
     Stream: TStream                      - ����֤��֤����
     ParentPublicKey: TCnRSAPublicKey     - ������֤�� RSA ǩ���߹�Կ

   ����ֵ��Boolean                        - �����Ƿ�Ϻ�ǩ��
}

function CnCAVerifyCertificateStream(Stream: TStream; ParentPublicKey: TCnEccPublicKey;
  ParentCurveType: TCnEccCurveType): Boolean; overload;
{* �� ECC ǩ���߹�Կ��֤һ CRT ���������Ƿ�Ϻ�ǩ����

   ������
     Stream: TStream                      - ����֤��֤����
     ParentPublicKey: TCnEccPublicKey     - ������֤�� ECC ǩ���߹�Կ
     ParentCurveType: TCnEccCurveType     - ������֤��ǩ���ߵ���Բ��������

   ����ֵ��Boolean                        - �����Ƿ�Ϻ�ǩ��
}

function CnCALoadCertificateFromFile(const FileName: string;
  Certificate: TCnCertificate; const Password: string = ''): Boolean;
{* ���� PEM ��ʽ�� CRT ֤���ļ���ԭʼ�Ķ����� CER �ļ����������ݷ��� TCnCertificate �����С�

   ������
     const FileName: string               - ���������ļ���
     Certificate: TCnCertificate          - ���ص�֤�����
     const Password: string               - ֤������ܣ��˴��ṩ��Ӧ����

   ����ֵ��Boolean                        - ���ؼ����Ƿ�ɹ�
}

function CnCALoadCertificateFromStream(Stream: TStream;
  Certificate: TCnCertificate; const Password: string = ''): Boolean;
{* ���� PEM ��ʽ�� CRT ֤������ԭʼ�Ķ����� CER �����������ݷ��� TCnCertificate �����С�

   ������
     Stream: TStream                      - ����������
     Certificate: TCnCertificate          - ���ص�֤�����
     const Password: string               - ֤������ܣ��˴��ṩ��Ӧ����

   ����ֵ��Boolean                        - ���ؼ����Ƿ�ɹ�
}

function CnCASignCertificate(PrivateKey: TCnRSAPrivateKey; const CRTFile: string;
  const CSRFile: string; const OutCRTFile: string; const IntSerialNum: string;
  NotBefore: TDateTime; NotAfter: TDateTime; CASignType: TCnCASignType = ctSha1RSA): Boolean; overload;
{* �� RSA CRT ֤���������Ӧ˽Կǩ��֤���������ɱ�ǩ��֤�飬Ŀǰֻ֧�� v1 ��ʽ��
   ���ݿͻ���֤�������� ECC/RSA �����Ρ�

   ������
     PrivateKey: TCnRSAPrivateKey         - ����ǩ��֤����ϼ� RSA ˽Կ
     const CRTFile: string                - ����ǩ�����ϼ�֤��
     const CSRFile: string                - ��ǩ����֤�������ļ�
     const OutCRTFile: string             - �����ǩ��֤��
     const IntSerialNum: string           - ���к�
     NotBefore: TDateTime                 - ֤����Ч�ڿ�ʼ������ʱ��
     NotAfter: TDateTime                  - ֤����Ч�ڽ���������ʱ��
     CASignType: TCnCASignType            - ǩ���Ӵ��㷨����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function CnCASignCertificate(PrivateKey: TCnEccPrivateKey; CurveType: TCnEccCurveType;
  const CRTFile: string; const CSRFile: string; const OutCRTFile: string; const IntSerialNum: string;
  NotBefore: TDateTime; NotAfter: TDateTime; CASignType: TCnCASignType = ctSha1Ecc): Boolean; overload;
{* �� ECC CRT ֤���������Ӧ˽Կǩ��֤���������ɱ�ǩ��֤�飬Ŀǰֻ֧�� v1 ��ʽ��
   ���ݿͻ���֤�������� ECC/RSA �����Ρ�

   ������
     PrivateKey: TCnEccPrivateKey         - ����ǩ��֤����ϼ� ECC ˽Կ
     CurveType: TCnEccCurveType           - ����ǩ��֤����ϼ���Բ��������
     const CRTFile: string                - ����ǩ�����ϼ�֤��
     const CSRFile: string                - ��ǩ����֤�������ļ�
     const OutCRTFile: string             - �����ǩ��֤��
     const IntSerialNum: string           - ���к�
     NotBefore: TDateTime                 - ֤����Ч�ڿ�ʼ������ʱ��
     NotAfter: TDateTime                  - ֤����Ч�ڽ���������ʱ��
     CASignType: TCnCASignType            - ǩ���Ӵ��㷨����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

// =============================== ������������ ================================

function AddCASignTypeOIDNodeToWriter(AWriter: TCnBerWriter; CASignType: TCnCASignType;
  AParent: TCnBerWriteNode): TCnBerWriteNode;
{* ��һ���Ӵ��㷨�� OID д��һ�� BER �ڵ���ӽڵ㡣

   ������
     AWriter: TCnBerWriter                - BER д������ʵ��
     CASignType: TCnCASignType            - ֤���ǩ���Ӵ��㷨
     AParent: TCnBerWriteNode             - ��д��ڵ�ĸ��ڵ�

   ����ֵ��TCnBerWriteNode                - ���������Ľڵ�
}

function GetCASignNameFromSignType(Sign: TCnCASignType): string;
{* ��֤���ǩ���Ӵ��㷨ö��ֵ��ȡ�����ơ�

   ������
     Sign: TCnCASignType                  - ֤���ǩ���Ӵ��㷨

   ����ֵ��string                         - ����֤���ǩ���Ӵ��㷨
}

implementation

uses
  CnNative;

resourcestring
  SCnErrorNotSelfSignCanNotVerify = 'NOT Self-Sign. Can NOT Verify.';
  SCnErrorNotRsaCanNotVerify = 'NOT RSA. Can NOT Verify using RSA Key.';
  SCnErrorNotEccCanNotVerify = 'NOT ECC. Can NOT Verify.';

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

  // ��չ�ֶ��ǵ� OID
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

  // ���ڽ����ַ������ݵĳ���
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
    // TODO: �����㷨����֧��
  end;
end;

// ����ָ������ժҪ�㷨�������ݵĶ������Ӵ�ֵ��д�� Stream��Buffer ��ָ��
function CalcDigestData(const Buffer; Count: Integer; CASignType: TCnCASignType;
  outStream: TStream): Boolean;
var
  Md5: TCnMD5Digest;
  Sha1: TCnSHA1Digest;
  Sha256: TCnSHA256Digest;
begin
  Result := False;
  case CASignType of
    ctMd5RSA, ctMd5Ecc:
      begin
        Md5 := MD5Buffer(Buffer, Count);
        outStream.Write(Md5, SizeOf(TCnMD5Digest));
        Result := True;
      end;
    ctSha1RSA, ctSha1Ecc:
      begin
        Sha1 := SHA1Buffer(Buffer, Count);
        outStream.Write(Sha1, SizeOf(TCnSHA1Digest));
        Result := True;
      end;
    ctSha256RSA, ctSha256Ecc:
      begin
        Sha256 := SHA256Buffer(Buffer, Count);
        outStream.Write(Sha256, SizeOf(TCnSHA256Digest));
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

{ д���¸�ʽ�� RSA ��Կ�ڵ�
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
  AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @CN_OID_RSAENCRYPTION_PKCS1[0],
    SizeOf(CN_OID_RSAENCRYPTION_PKCS1), Node);
  AWriter.AddNullNode(Node);
  Node := AWriter.AddContainerNode(CN_BER_TAG_BIT_STRING, PubNode);
  Node := AWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, Node);
  AddBigNumberToWriter(AWriter, PublicKey.PubKeyProduct, Node);
  AddBigNumberToWriter(AWriter, PublicKey.PubKeyExponent, Node);
end;

{ д���¸�ʽ�� ECC ��Կ�ڵ�
  SEQUENCE (2 elem)
    SEQUENCE (2 elem)
      OBJECT IDENTIFIER 1.2.840.10045.2.1 ecPublicKey (ANSI X9.62 public key type)
      OBJECT IDENTIFIER 1.3.132.0.10 secp256k1 (SECG (Certicom) named elliptic curve)
    BIT STRING ����ͷ�Ƿ�ѹ����Կ�洢��ʽ��
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
  AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @CN_OID_EC_PUBLIC_KEY[0],
    SizeOf(CN_OID_EC_PUBLIC_KEY), Node);
  CurveLen := GetOIDFromCurveType(CurveType, CurvePtr);
  if CurveLen <= 0 then
    Exit;

  AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, CurvePtr, CurveLen, Node);
  WriteEccPublicKeyToBitStringNode(AWriter, PubNode, PublicKey);
  Result := True;
end;

{
  RSA ���ɲ�дǩ���ڵ�
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
  OutBuf: TBytes;
  OutLen: Integer;
begin
  Result := False;
  ValueStream := nil;
  DigestStream := nil;
  HashWriter := nil;

  try
    // �ó� InfoRoot ������
    ValueStream := TMemoryStream.Create;
    NodeToSign.SaveToStream(ValueStream);

    // �������Ӵ�
    DigestStream := TMemoryStream.Create;
    CalcDigestData(ValueStream.Memory, ValueStream.Size, CASignType, DigestStream);

    // �� Hash ����ǩ���㷨ƴ�� BER ����
    HashWriter := TCnBerWriter.Create;
    HashRoot := HashWriter.AddContainerNode(CN_BER_TAG_SEQUENCE);
    Node := HashWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, HashRoot);
    AddDigestTypeOIDNodeToWriter(HashWriter, GetRSASignTypeFromCASignType(CASignType), Node);
    HashWriter.AddNullNode(Node);
    HashWriter.AddBasicNode(CN_BER_TAG_OCTET_STRING, DigestStream, HashRoot);

    // ���ô� Stream���������ɵ� BER ��ʽ����
    DigestStream.Clear;
    HashWriter.SaveToStream(DigestStream);

    // RSA ˽Կ���ܴ� BER ��õ�ǩ��ֵ������ǰ��Ҫ PKCS1 ����
    SetLength(OutBuf, PrivateKey.BitsCount div 8);
    OutLen := PrivateKey.BitsCount div 8;
    if not CnRSAEncryptData(DigestStream.Memory, DigestStream.Size,
      @OutBuf[0], PrivateKey) then
      Exit;

    // �����Ӵ��㷨˵��
    HashNode := AWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);
    AddCASignTypeOIDNodeToWriter(AWriter, CASignType, HashNode);
    AWriter.AddNullNode(HashNode);

    // д������ǩ��ֵ
    AWriter.AddBasicNode(CN_BER_TAG_BIT_STRING, @OutBuf[0], OutLen, Root);
    Result := True;
  finally
    HashWriter.Free;
    DigestStream.Free;
    ValueStream.Free;
  end;
end;

{
  ECC ���ɲ�дǩ���ڵ�
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
    // �ó� InfoRoot ������
    ValueStream := TMemoryStream.Create;
    NodeToSign.SaveToStream(ValueStream);

    SignStream := TMemoryStream.Create;
    Ecc := TCnEcc.Create(CurveType);
    if not CnEccSignStream(ValueStream, SignStream, Ecc, PrivateKey,
      GetEccSignTypeFromCASignType(CASignType)) then
      Exit;

    // �����Ӵ��㷨˵��
    HashNode := AWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);
    AddCASignTypeOIDNodeToWriter(AWriter, CASignType, HashNode);
    AWriter.AddNullNode(HashNode);

    // д������ǩ��ֵ
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
  // Superparent �� DNRoot�������� Set���� Sequence��Sequence ��� OID �� PrintableString
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
  OutBuf: TBytes;
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

    // �� Info дһ��ֱ���ӽڵ�
    Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, InfoRoot);          // �汾
    DNRoot := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, InfoRoot);  // DN
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, InfoRoot); // ��Կ
    Writer.AddRawNode($A0, @B, 1, InfoRoot);                           // ������

    // д DN �ڵ������
    WriteDNNameToNode(Writer, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, DNRoot, CN_BER_TAG_IA5STRING);

    // д��Կ�ڵ������
    WriteRSAPublicKeyToNode(Writer, PubNode, PublicKey);

    // ���� InfoRoot �������ժҪ��ǩ��
    if not GenerateRSASignatureNode(Writer, Root, InfoRoot, PrivateKey, CASignType) then
      Exit;

    // ����
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
  OutBuf: TBytes;
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

    // �� Info дһ��ֱ���ӽڵ�
    Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, InfoRoot);          // �汾
    DNRoot := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, InfoRoot);  // DN
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, InfoRoot); // ��Կ
    Writer.AddRawNode($A0, @B, 1, InfoRoot);                           // ������

    // д DN �ڵ������
    WriteDNNameToNode(Writer, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, DNRoot, CN_BER_TAG_IA5STRING);

    // д��Կ�ڵ������
    if not WriteEccPublicKeyToNode(Writer, PubNode, PublicKey, CurveType) then
      Exit;

    // ���� InfoRoot �������ժҪ��ǩ��
    GenerateEccSignatureNode(Writer, Root, InfoRoot, PrivateKey, CurveType, CASignType);

    // ����
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

  // ѭ������ DN ��
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

// �����½ṹ�н�� RSA ��Կ
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

// RSA: ����֪��Կ�����������½ṹ���ó�ǩ��ֵ���ܲ�ȥ�� PKCS1 �����õ�ժҪֵ
// ECC: ֱ���õ�ǩ��ֵ��ԭʼ���ṹ
// RSA ����޹�Կ����ֻȡǩ��ֵ�����⿪��ECC ������û�ж��ⲻ��
{
  SEQUENCE
    OBJECT IDENTIFIER 1.2.840.113549.1.1.5  sha1WithRSAEncryption(PKCS #1) �� sha256WithECDSA
    NULL
  BIT STRING  ����� RSA ��˽ڵ��Ƕ�����ܺ���Ӵ�ֵ������� ECC ����һ�� SEQ �ӽڵ����������� INTEGER
}
function ExtractSignaturesByPublicKey(IsRSA: Boolean; RSAPublicKey: TCnRSAPublicKey;
  EccPublicKey: TCnEccPublicKey; HashNode, SignNode: TCnBerReadNode; out CASignType: TCnCASignType;
  out RSADigestType: TCnRSASignDigestType; out SignValue, DigestValue: Pointer;
  out SignLength, DigestLength: Integer): Boolean;
var
  P: PByte;
  Reader: TCnBerReader;
  Node: TCnBerReadNode;
  OutBuf: TBytes;
  OutLen: Integer;
begin
  Result := False;

  // �ҵ�ǩ���㷨
  if HashNode.Count >= 1 then
    CASignType := ExtractCASignType(HashNode.Items[0]);

  // �޹�Կʱ������
  if IsRSA and (RSAPublicKey = nil) then
  begin
    Result := True;
    Exit;
  end;

  if not IsRSA then // ECC ǩ������
  begin
    if SignNode.Count <> 1 then
      Exit;
    if SignNode.Items[0].Count <> 2 then
      Exit;
  end;

  // ������ԭʼǩ�����ݣ����� BIT String ��ǰ������ 0
  FreeMemory(SignValue);
  SignLength := SignNode.BerDataLength - 1;
  SignValue := GetMemory(SignLength);
  P := PByte(SignNode.BerDataAddress);
  Inc(P);
  Move(P^, SignValue^, SignLength);

  if IsRSA then // RSA ǩ���ܽ⿪�õ�ԭʼ�Ӵ�ֵ���� ECC ����
  begin
    // �⿪ RSA ǩ����ȥ�� PKCS1 ��������ݵõ� DER ������Ӵ�ֵ���㷨
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

        // ��ȡ Ber ������Ӵ�ֵ
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
  Buf: TBytes;
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
          ValueNode := ExtNode.Items[2] // Critical���ݲ�����
        else
          ValueNode := ExtNode.Items[1];
      end;

      if ValueNode = nil then
        Continue;
      if (ValueNode.BerTag <> CN_BER_TAG_OCTET_STRING) or (ValueNode.Count <> 1) then
        Continue;

      ValueNode := ValueNode.Items[0]; // ָ�� OctetString ���ӽڵ㣬Value ����
      if CompareObjectIdentifier(OidNode, @OID_EXT_SUBJECTKEYIDENTIFIER, SizeOf(OID_EXT_SUBJECTKEYIDENTIFIER)) then
      begin
        StandardExt.SubjectKeyIdentifier := ValueNode.AsAnsiString;
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_KEYUSAGE, SizeOf(OID_EXT_KEYUSAGE)) then
      begin
        if ValueNode.BerTag = CN_BER_TAG_BIT_STRING then
        begin
          SetLength(Buf, ValueNode.BerDataLength);
          if Length(Buf) >= 2 then
          begin
            ValueNode.CopyDataTo(@Buf[0]);
            // Buf[1] Ҫ shr Buf[0] λ
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
        // TODO: �������ӵ� CERTIFICATEPOLICIES
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_AUTHORITYKEYIDENTIFIER, SizeOf(OID_EXT_AUTHORITYKEYIDENTIFIER)) then
      begin
        if ValueNode.Count = 1 then
          StandardExt.AuthorityKeyIdentifier := ValueNode[0].AsAnsiString;
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
  CSR �ļ��Ĵ����ʽ���£�

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
          OBJECT IDENTIFIER1.2.840.113549.1.1.1 rsaEncryption(PKCS #1) �� 1.2.840.10045.2.1 ecPublicKey
          NULL                                                         �� 1.3.132.0.10 secp256k1
        BIT STRING
          SEQUENCE                        RSA ��Կ���� INTEGER���� ECC ��Կ��һ�� BITSTRING��û�ӽڵ㣩
            INTEGER
            INTEGER 65537
      [0]
    SEQUENCE
      OBJECT IDENTIFIER 1.2.840.113549.1.1.5sha1WithRSAEncryption(PKCS #1)
      NULL
    BIT STRING  Digest ֵ���� RSA ���ܺ�Ľ�����򾭹� ECC ���ܺ������㣨 SEQUENCE ������ INTEGER��
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
      and (Reader.Items[2].AsInteger = 0) then // ��������ô����汾�ű���Ϊ 0
    begin
      DNRoot := Reader.Items[3];
      PubNode := DNRoot.GetNextSibling;
      if PubNode = nil then
        Exit;

      HashNode := Reader.Items[1].GetNextSibling;
      if (HashNode = nil) or (HashNode.Count = 0) then // RSA �� Hash Node ����� Null �ӽڵ�
        Exit;

      SignNode := HashNode.GetNextSibling;
      if (SignNode = nil) or (SignNode.BerTag <> CN_BER_TAG_BIT_STRING)
        or (SignNode.BerDataLength <= 2) then
        Exit;

      IsRSA := False;
      if (PubNode.Count = 2) and (PubNode.Items[0].Count = 2) then
        IsRSA := CompareObjectIdentifier(PubNode.Items[0].Items[0],
          @CN_OID_RSAENCRYPTION_PKCS1[0], SizeOf(CN_OID_RSAENCRYPTION_PKCS1));

      IsECC := False;
      if (PubNode.Count = 2) and (PubNode.Items[0].Count = 2) then
        IsECC := CompareObjectIdentifier(PubNode.Items[0].Items[0],
          @CN_OID_EC_PUBLIC_KEY[0], SizeOf(CN_OID_EC_PUBLIC_KEY));

      if not IsRSA and not IsECC then // �㷨���� RSA Ҳ���� ECC
        Exit;

      CertificateRequest.IsRSA := IsRSA;
      if not IsRSA then
      begin
        CurveType := GetCurveTypeFromOID(PubNode.Items[0].Items[1].BerAddress,
          PubNode.Items[0].Items[1].BerLength);
        if CurveType = ctCustomized then
          Exit;
        CertificateRequest.EccCurveType := CurveType;  // ��� ECC ��������
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

      // �⿪ RSA �� ECC ��Կ
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
        // ECC �����м������Ӵ�ֵ
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
        // �������Ӵ�ֵ
        P := InfoRoot.BerAddress;
        CalcDigestData(P, InfoRoot.BerLength, CSR.CASignType, SignStream);

        // ���� RSA ���ܳ�����ǩ��ֵ�ֹ��Ա�
        if SignStream.Size = CSR.DigestLength then
          Result := CompareMem(SignStream.Memory, CSR.DigestValue, SignStream.Size);
      end
      else // ECC ֱ����֤���ݿ��ǩ�����Ӵ�ֵ
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
      raise ECnCAException.Create(SCnErrorNotSelfSignCanNotVerify);

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

      // �������Ӵ�ֵ
      SignStream := TMemoryStream.Create;

      if CRT.IsRSA then // RSA ��ǩ��֤����Ӵ�ֵ���ܴ�֤������ܳ����ģ��Աȼ���ֵ����
      begin
        P := InfoRoot.BerAddress;
        CalcDigestData(P, InfoRoot.BerLength, CRT.CASignType, SignStream);
        if SignStream.Size = CRT.DigestLength then
          Result := CompareMem(SignStream.Memory, CRT.DigestValue, SignStream.Size);
      end
      else // ECC ��ǩ��֤����û���Ӵ�ֵ���ֶ�����Ӵ�ֵ�����Ǽ��������û�жԱ����壬��Ҫ�� ECC �ķ�ʽ��֤ǩ��ֵ
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
      raise ECnCAException.Create(SCnErrorNotRsaCanNotVerify);

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

      // �������Ӵ�ֵ
      InfoRoot := Reader.Items[1];
      SignStream := TMemoryStream.Create;
      P := InfoRoot.BerAddress;
      CalcDigestData(P, InfoRoot.BerLength, CRT.CASignType, SignStream);

      // RSA ֤����Ӵ�ֵҪ�ø���Կ���ܴ�֤������ܳ���
      if not ExtractSignaturesByPublicKey(True, ParentPublicKey,
        nil, SignAlgNode, SignValueNode,
        CRT.FCASignType, CRT.FRSADigestType, CRT.FSignValue,
        CRT.FDigestValue, CRT.FSignLength, CRT.FDigestLength) then
        Exit;

      // �Աȼ���ֵ
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
      raise ECnCAException.Create(SCnErrorNotEccCanNotVerify);

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

      // ECC ֤����û���Ӵ�ֵ���ֶ�����Ӵ�ֵ�����Ǽ��������û�жԱ����壬��Ҫ�� ECC �ķ�ʽ��ԭʼ��������ȥ��֤ǩ��ֵ
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
  Result := Result + SCRLF + 'Signature: ' + DataToHex(FSignValue, FSignLength);
  if FIsRSA then
    Result := Result + SCRLF + 'Signature Hash: ' + GetRSADigestNameFromSignDigestType(FRSADigestType)
  else
    Result := Result + SCRLF + 'Signature Hash: ' + GetEccDigestNameFromSignDigestType(FEccDigestType);
  Result := Result + SCRLF + 'Digest: ' + DataToHex(FDigestValue, FDigestLength);
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

  // ��ʱ������ת�����ַ������� FUTCTimeString��ʹ�� YYMMDDhhmm[ss]Z �ĸ�ʽ
  DecodeDate(FDateTime, Year, Month, Day);
  DecodeTime(FDateTime, Hour, Minute, Sec, MSec);

  Year := Year mod 100; // ֻȡ����λ
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
  //  ���� String ��ʱ�䲢�� FDateTime����ʽ�� YYMMDDhhmm[ss]Z �� YYMMDDhhmm[ss](+|-)hhmm
  if Length(FUTCTimeString) > 10 then // ���ٵ��� 11 ��
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
    if FUTCTimeString[Idx] in ['0'..'9'] then   // �� ss    // 11
    begin
      Sec := StrToInt(Copy(FUTCTimeString, Idx, 2));
      Inc(Idx, 2);
    end;

    if Idx <= Length(FUTCTimeString) then
    begin
      // ��ʱ Idx ֱ�ӣ���Խ�����ܵ� ss��ָ�� Z �� +-
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
  Certificate: TCnCertificate; const Password: string): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CnCALoadCertificateFromStream(Stream, Certificate, Password);
  finally
    Stream.Free;
  end;
end;

function CnCALoadCertificateFromStream(Stream: TStream;
  Certificate: TCnCertificate; const Password: string): Boolean;
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
    if not LoadPemStreamToMemory(Stream, PEM_CERTIFICATE_HEAD, PEM_CERTIFICATE_TAIL, Mem, Password) then
      Mem.LoadFromStream(Stream); // ����� PEM ��ʽ����ʧ�ܣ�������ԭʼ�����Ʒ�ʽ����

    Reader := TCnBerReader.Create(PByte(Mem.Memory), Mem.Size, True);
    Reader.ParseToTree;

    Root := Reader.Items[0];
    if Root.Count <> 3 then
      Exit;

    // �õ�����Ҫ���ڵ�
    BSCNode := Root.Items[0];
    SignAlgNode := Root.Items[1];
    SignValueNode := Root.Items[2];

    // BSC ����
    if BSCNode.Count < 6 then
      Exit;

    // �ж� Version������û��
    Certificate.BasicCertificate.Version := CN_CRT_BASIC_VERSION_1;
    if (BSCNode.Items[0].BerTag = 0) and (BSCNode.Items[0].Count = 1) then
    begin
      SerialNode := BSCNode.Items[1];

      // A0 �ֽڿ�ͷ��һ���ڵ㣬������һ�� Integer �ڵ㣬���Ǳ�׼���������Ľڵ�
      VerNode := BSCNode.Items[0].Items[0];
      Certificate.BasicCertificate.Version := VerNode.AsByte;
    end
    else
      SerialNode := BSCNode.Items[0];

    // ���к�
    SerialNum := TCnBigNumber.Create;
    try
      SerialNode.AsBigNumber(SerialNum);
      Certificate.BasicCertificate.SerialNumber := SerialNum.ToDec;
    finally
      FreeAndNil(SerialNum);
    end;

    // ������Ϣ�е�ǩ���㷨�ֶ�
    Node := SerialNode.GetNextSibling;
    if (Node <> nil) and (Node.Count >= 1) then
    begin
      Certificate.CASignType := ExtractCASignType(Node.Items[0]);
      Certificate.IsRSA := Certificate.CASignType in RSA_CA_TYPES;
    end
    else
      Exit;

    // �����ڶ������ֶ�
    List := TStringList.Create;
    try
      Node := Node.GetNextSibling; // ǩ���㷨�ڵ���ͬ���ڵ��� Issuer
      ExtractDNValuesToList(Node, List);
      Certificate.BasicCertificate.Issuer.CountryName := List.Values[SDN_COUNTRYNAME];
      Certificate.BasicCertificate.Issuer.StateOrProvinceName := List.Values[SDN_STATEORPROVINCENAME];
      Certificate.BasicCertificate.Issuer.LocalityName := List.Values[SDN_LOCALITYNAME];
      Certificate.BasicCertificate.Issuer.OrganizationName := List.Values[SDN_ORGANIZATIONNAME];
      Certificate.BasicCertificate.Issuer.OrganizationalUnitName := List.Values[SDN_ORGANIZATIONALUNITNAME];
      Certificate.BasicCertificate.Issuer.CommonName := List.Values[SDN_COMMONNAME];
      Certificate.BasicCertificate.Issuer.EmailAddress := List.Values[SDN_EMAILADDRESS];

      Node := Node.GetNextSibling; // Issuer �ڵ���ͬ���ڵ����� UTC Time
      if Node.Count = 2 then
      begin
        Certificate.BasicCertificate.NotBefore.UTCTimeString := Node.Items[0].AsString;
        Certificate.BasicCertificate.NotAfter.UTCTimeString := Node.Items[1].AsString;
      end;

      Node := Node.GetNextSibling; // UTC Time �ڵ���ͬ���ڵ��� Subject
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

    Node := Node.GetNextSibling; // Subject �ڵ���ͬ���ڵ��Ǳ�ǩ���ߵĹ�Կ
    IsRSA := False;
    if (Node.Count = 2) and (Node.Items[0].Count = 2) then
      IsRSA := CompareObjectIdentifier(Node.Items[0].Items[0],
        @CN_OID_RSAENCRYPTION_PKCS1[0], SizeOf(CN_OID_RSAENCRYPTION_PKCS1));

    IsECC := False;
    if (Node.Count = 2) and (Node.Items[0].Count = 2) then
      IsECC := CompareObjectIdentifier(Node.Items[0].Items[0],
        @CN_OID_EC_PUBLIC_KEY[0], SizeOf(CN_OID_EC_PUBLIC_KEY));

    if not IsRSA and not IsECC then // ��ǩ���ߵ��㷨���� RSA Ҳ���� ECC
      Exit;

    Certificate.BasicCertificate.SubjectIsRSA := IsRSA;
    if not IsRSA then
    begin
      CurveType := GetCurveTypeFromOID(Node.Items[0].Items[1].BerAddress,
        Node.Items[0].Items[1].BerLength);
      if CurveType = ctCustomized then
        Exit;
      Certificate.BasicCertificate.SubjectEccCurveType := CurveType;  // ��� ECC ��������
    end;

    // �⿪��ǩ���ߵĹ�Կ
    Node := Node.Items[1]; // ָ�� BitString
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

    // RSA ��ǩ��֤����Խ⿪�Ӵ�ֵ��ECC ��û��
    if Certificate.IsSelfSigned then
    begin
      Result := ExtractSignaturesByPublicKey(IsRSA, Certificate.BasicCertificate.SubjectRSAPublicKey,
        Certificate.BasicCertificate.SubjectEccPublicKey, SignAlgNode, SignValueNode,
        Certificate.FCASignType, Certificate.FRSADigestType, Certificate.FSignValue,
        Certificate.FDigestValue, Certificate.FSignLength, Certificate.FDigestLength);

      if Result and not IsRSA then
      begin
        // ECC �����м������Ӵ�ֵ
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
      // �⿪ǩ����ע��֤�鲻��ǩ�������Ĺ�Կ���������޷������õ������Ӵ�ֵ
      Result := ExtractSignaturesByPublicKey(IsRSA, nil, nil, SignAlgNode, SignValueNode, Certificate.FCASignType,
        DummyDigestType, Certificate.FSignValue, DummyPointer, Certificate.FSignLength,
        DummyInteger);
    end;

    // �⿪��׼��չ��˽�л�������չ�ڵ�
    if Result then
    begin
      Node := (Node.Parent as TCnBerReadNode).GetNextSibling;
      if (Node <> nil) then  // BITString ������������
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
  Result := Result + SCRLF + 'Signature: ' + DataToHex(FSignValue, FSignLength);
  if FDigestValue <> nil then
  begin
    if FIsRSA then
      Result := Result + SCRLF + 'Hash: ' + GetRSADigestNameFromSignDigestType(FRSADigestType)
    else
      Result := Result + SCRLF + 'Hash: ' + GetEccDigestNameFromSignDigestType(FEccDigestType);
    Result := Result + SCRLF + 'Digest: ' + DataToHex(FDigestValue, FDigestLength);
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
  Result := Result + SCRLF + 'Authority Key Identifier: ' + DataToHex(Pointer(FAuthorityKeyIdentifier), Length(FAuthorityKeyIdentifier));
  Result := Result + SCRLF + 'Subject Key Identifier: ' + DataToHex(Pointer(FSubjectKeyIdentifier), Length(FSubjectKeyIdentifier));
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
  AWriter.AddAnsiStringNode(BerTag, AnsiString(DN), Result);
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
  Buf: TBytes;
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

    // �汾���ԣ�д���к�
    SerialNum := TCnBigNumber.Create;
    SerialNum.SetDec(AnsiString(IntSerialNum));
    SetLength(Buf, SerialNum.GetBytesCount);
    SerialNum.ToBinary(@Buf[0]);
    Writer.AddBasicNode(CN_BER_TAG_INTEGER, @Buf[0], Length(Buf), BasicNode);

    // д�㷨
    Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    AddCASignTypeOIDNodeToWriter(Writer, CASignType, Node);
    Writer.AddNullNode(Node);

    IssuerNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    ValidNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    SubjectNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);

    // дǩ����
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, CN_BER_TAG_IA5STRING);

    // д��Чʱ��
    UTCTime := TCnUTCTime.Create;
    UTCTime.SetDateTime(NotBefore);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, AnsiString(UTCTime.UTCTimeString), ValidNode);
    UTCTime.SetDateTime(NotAfter);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, AnsiString(UTCTime.UTCTimeString), ValidNode);

    // д��ǩ����
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, CN_BER_TAG_IA5STRING);

    // д��Կ�ڵ�����
    WriteRSAPublicKeyToNode(Writer, PubNode, PublicKey);

    // ���㲢дǩ��ֵ
    if not GenerateRSASignatureNode(Writer, Root, BasicNode, PrivateKey, CASignType) then
      Exit;

    // ����
    Stream := TMemoryStream.Create;
    Writer.SaveToStream(Stream);
    Result := SaveMemoryToPemFile(OutCRTFile, PEM_CERTIFICATE_HEAD,
      PEM_CERTIFICATE_TAIL, Stream);
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
  Buf: TBytes;
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

    // �汾���ԣ�д���к�
    SerialNum := TCnBigNumber.Create;
    SerialNum.SetDec(AnsiString(IntSerialNum));
    SetLength(Buf, SerialNum.GetBytesCount);
    SerialNum.ToBinary(@Buf[0]);
    Writer.AddBasicNode(CN_BER_TAG_INTEGER, @Buf[0], Length(Buf), BasicNode);

    // д�㷨
    Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    AddCASignTypeOIDNodeToWriter(Writer, CASignType, Node);
    Writer.AddNullNode(Node);

    IssuerNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    ValidNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    SubjectNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);

    // дǩ����
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, CN_BER_TAG_IA5STRING);

    // д��Чʱ��
    UTCTime := TCnUTCTime.Create;
    UTCTime.SetDateTime(NotBefore);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, AnsiString(UTCTime.UTCTimeString), ValidNode);
    UTCTime.SetDateTime(NotAfter);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, AnsiString(UTCTime.UTCTimeString), ValidNode);

    // д��ǩ����
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, CN_BER_TAG_IA5STRING);

    // д��Կ�ڵ�����
    if not WriteEccPublicKeyToNode(Writer, PubNode, PublicKey, CurveType) then
      Exit;

    // ���㲢дǩ��ֵ
    if not GenerateEccSignatureNode(Writer, Root, BasicNode, PrivateKey, CurveType, CASignType) then
      Exit;

    // ����
    Stream := TMemoryStream.Create;
    Writer.SaveToStream(Stream);
    Result := SaveMemoryToPemFile(OutCRTFile, PEM_CERTIFICATE_HEAD,
      PEM_CERTIFICATE_TAIL, Stream);
  finally
    Writer.Free;
    Stream.Free;
    SerialNum.Free;
    UTCTime.Free;
  end;
end;

{
   �� RSA CRT ֤��ǩ�� CSR ����������֤�� CRT���ͻ���֤���ļ�����дǩ������д��ǩ����
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
  Buf: TBytes;
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

    // �汾���ԣ�д���к�
    SerialNum := TCnBigNumber.Create;
    SerialNum.SetDec(AnsiString(IntSerialNum));
    SetLength(Buf, SerialNum.GetBytesCount);
    SerialNum.ToBinary(@Buf[0]);
    Writer.AddBasicNode(CN_BER_TAG_INTEGER, @Buf[0], Length(Buf), BasicNode);

    // д�㷨�����ֵ����ȡ���� CRT ��Ӧ�� Key ���Ͷ����赥���������룿
    Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    AddCASignTypeOIDNodeToWriter(Writer, CASignType, Node);
    Writer.AddNullNode(Node);

    IssuerNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    ValidNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    SubjectNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);

    // дǩ����
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

    // д��Чʱ��
    UTCTime := TCnUTCTime.Create;
    UTCTime.SetDateTime(NotBefore);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, AnsiString(UTCTime.UTCTimeString), ValidNode);
    UTCTime.SetDateTime(NotAfter);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, AnsiString(UTCTime.UTCTimeString), ValidNode);

    // д��ǩ����
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

    // д��ǩ���ߵĹ�Կ�ڵ����ݣ����ݱ�ǩ���� RSA/ECC ������
    if CSR.IsRSA then
      WriteRSAPublicKeyToNode(Writer, PubNode, CSR.RSAPublicKey)
    else
      WriteEccPublicKeyToNode(Writer, PubNode, CSR.EccPublicKey, CSR.EccCurveType);

    // ���㲢дǩ��ֵ
    if not GenerateRSASignatureNode(Writer, Root, BasicNode, PrivateKey, CASignType) then
      Exit;

    // ����
    Stream := TMemoryStream.Create;
    Writer.SaveToStream(Stream);
    Result := SaveMemoryToPemFile(OutCRTFile, PEM_CERTIFICATE_HEAD,
      PEM_CERTIFICATE_TAIL, Stream);
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
   �� ECC CRT ֤��ǩ�� CSR ����������֤�� CRT���ͻ���֤���ļ�����дǩ������д��ǩ����
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
  Buf: TBytes;
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

    // �汾���ԣ�д���к�
    SerialNum := TCnBigNumber.Create;
    SerialNum.SetDec(AnsiString(IntSerialNum));
    SetLength(Buf, SerialNum.GetBytesCount);
    SerialNum.ToBinary(@Buf[0]);
    Writer.AddBasicNode(CN_BER_TAG_INTEGER, @Buf[0], Length(Buf), BasicNode);

    // д�㷨�����ֵ����ȡ���� CRT ��Ӧ�� Key ���Ͷ����赥���������룿
    Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    AddCASignTypeOIDNodeToWriter(Writer, CASignType, Node);
    Writer.AddNullNode(Node);

    IssuerNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    ValidNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    SubjectNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);

    // дǩ����
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

    // д��Чʱ��
    UTCTime := TCnUTCTime.Create;
    UTCTime.SetDateTime(NotBefore);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, AnsiString(UTCTime.UTCTimeString), ValidNode);
    UTCTime.SetDateTime(NotAfter);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, AnsiString(UTCTime.UTCTimeString), ValidNode);

    // д��ǩ����
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

    // д��ǩ���ߵĹ�Կ�ڵ����ݣ����ݱ�ǩ���� RSA/ECC ������
    if CSR.IsRSA then
      WriteRSAPublicKeyToNode(Writer, PubNode, CSR.RSAPublicKey)
    else
      WriteEccPublicKeyToNode(Writer, PubNode, CSR.EccPublicKey, CSR.EccCurveType);

    // ���㲢дǩ��ֵ
    if not GenerateEccSignatureNode(Writer, Root, BasicNode, PrivateKey, CurveType, CASignType) then
      Exit;

    // ����
    Stream := TMemoryStream.Create;
    Writer.SaveToStream(Stream);
    Result := SaveMemoryToPemFile(OutCRTFile, PEM_CERTIFICATE_HEAD,
      PEM_CERTIFICATE_TAIL, Stream);
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
