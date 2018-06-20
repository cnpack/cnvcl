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
* 备    注：
* 开发平台：WinXP + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2018.06.15 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, CnRSA, CnBerUtils;

type
  TCnCertificateInfo = class(TPersistent)
  {* 描述证书中包含的普通字段信息，也叫 DN}
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

  TCnRSACertificateRequest = class(TObject)
  {* 描述证书请求中的信息，包括普通字段、公钥、摘要类型与签名等}
  private
    FCertificateInfo: TCnCertificateInfo;
    FPublicKey: TCnRSAPublicKey;
    FSignDigestType: TCnRSASignDigestType;
    FSignValue: Pointer;
    FSignLength: Integer;
    procedure SetCertificateInfo(const Value: TCnCertificateInfo);
    procedure SetPublicKey(const Value: TCnRSAPublicKey); // 签名 Length 为 Key 的 Bit 数如 2048 Bit。
  public
    constructor Create;
    destructor Destroy; override;

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property CertificateInfo: TCnCertificateInfo read FCertificateInfo write SetCertificateInfo;
    {* 证书 DN 信息}
    property PublicKey: TCnRSAPublicKey read FPublicKey write SetPublicKey;
    {* 客户端公钥}
    property SignDigestType: TCnRSASignDigestType read FSignDigestType write FSignDigestType;
    {* 客户端签名的散列算法}
    property SignValue: Pointer read FSignValue write FSignValue;
    {* 散列后签名的结果}
    property SignLength: Integer read FSignLength write FSignLength;
    {* 散列后签名的结果长度}
  end;

function CnCANewCertificateSignRequest(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCSRFile: string; const CountryName: string; const
  StateOrProvinceName: string; const LocalityName: string; const OrganizationName:
  string; const OrganizationalUnitName: string; const CommonName: string; const
  EmailAddress: string; SignType: TCnRSASignDigestType = sdtSHA1): Boolean;

function CnCALoadCertificateSignRequestFromFile(const FileName: string;
  CertificateRequest: TCnRSACertificateRequest): Boolean;
{* 解析 PEM 格式的 CSR 文件并将内容放入 TCnRSACertificateRequest 对象中}

implementation

const
  // PKCS#10
  PEM_CERTIFICATE_REQUEST_HEAD = '-----BEGIN CERTIFICATE REQUEST-----';
  PEM_CERTIFICATE_REQUEST_TAIL = '-----END CERTIFICATE REQUEST-----';

  OID_DN_COUNTRYNAME            : array[0..2] of Byte = ($55, $04, $06); // 2.5.4.6
  OID_DN_STATEORPROVINCENAME    : array[0..2] of Byte = ($55, $04, $08); // 2.5.4.8
  OID_DN_LOCALITYNAME           : array[0..2] of Byte = ($55, $04, $07); // 2.5.4.7
  OID_DN_ORGANIZATIONNAME       : array[0..2] of Byte = ($55, $04, $0A); // 2.5.4.10
  OID_DN_ORGANIZATIONALUNITNAME : array[0..2] of Byte = ($55, $04, $0B); // 2.5.4.11
  OID_DN_COMMONNAME             : array[0..2] of Byte = ($55, $04, $03); // 2.5.4.3
  OID_DN_EMAILADDRESS           : array[0..8] of Byte = (
    $2A, $86, $48, $86, $F7, $0D, $01, $09, $01
  ); // 1.2.840.113549.1.9.1

  OID_SHA1_RSAENCRYPTION        : array[0..8] of Byte = (
    $2A, $86, $48, $86, $F7, $0D, $01, $01, $05
  ); // 1.2.840.113549.1.1.5

  SCRLF = #13#10;

function PrintHex(const Buf: Pointer; Len: Integer): string;
var
  I: Integer;
  P: PByteArray;
const
  Digits: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
                                  '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
begin
  Result := '';
  P := PByteArray(Buf);
  for I := 0 to Len - 1 do
  begin
    Result := Result + {$IFDEF UNICODE}string{$ENDIF}(Digits[(P[I] shr 4) and $0F] +
              Digits[P[I] and $0F]);
  end;
end;

function CnCANewCertificateSignRequest(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCSRFile: string; const CountryName: string; const
  StateOrProvinceName: string; const LocalityName: string; const OrganizationName:
  string; const OrganizationalUnitName: string; const CommonName: string; const
  EmailAddress: string; SignType: TCnRSASignDigestType): Boolean;
begin
  Result := False;
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
    BIT STRING  Digest值
}
function CnCALoadCertificateSignRequestFromFile(const FileName: string;
  CertificateRequest: TCnRSACertificateRequest): Boolean;
var
  I: Integer;
  P: Pointer;
  IsRSA, HasPub: Boolean;
  Reader: TCnBerReader;
  MemStream: TMemoryStream;
  DNRoot, PubNode, HashNode, SignNode, Node, StrNode: TCnBerReadNode;
begin
  Result := False;
  if FileExists(FileName) then
  begin
    Reader := nil;
    MemStream := nil;
    try
      MemStream := TMemoryStream.Create;
      if not LoadPemFileToMemory(FileName, PEM_CERTIFICATE_REQUEST_HEAD,
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
                  CertificateRequest.CertificateInfo.CountryName := StrNode.AsPrintableString
                else if CompareObjectIdentifier(Node, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME)) then
                  CertificateRequest.CertificateInfo.StateOrProvinceName := StrNode.AsPrintableString
                else if CompareObjectIdentifier(Node, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME)) then
                  CertificateRequest.CertificateInfo.LocalityName := StrNode.AsPrintableString
                else if CompareObjectIdentifier(Node, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME)) then
                  CertificateRequest.CertificateInfo.OrganizationName := StrNode.AsPrintableString
                else if CompareObjectIdentifier(Node, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME)) then
                  CertificateRequest.CertificateInfo.OrganizationalUnitName := StrNode.AsPrintableString
                else if CompareObjectIdentifier(Node, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME)) then
                  CertificateRequest.CertificateInfo.CommonName := StrNode.AsPrintableString
                else if CompareObjectIdentifier(Node, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS)) then
                  CertificateRequest.CertificateInfo.EmailAddress := StrNode.AsPrintableString
              end;
            end;
          end;
        end;

        // 解开公钥
        HasPub := False;
        PubNode := PubNode.Items[1]; // BitString
        if (PubNode.Count = 1) and (PubNode.Items[0].Count = 2) then
        begin
          PubNode := PubNode.Items[0]; // Sequence
          CertificateRequest.PublicKey.PubKeyProduct.SetBinary(PAnsiChar(
            PubNode.Items[0].BerDataAddress), PubNode.Items[0].BerDataLength);
          CertificateRequest.PublicKey.PubKeyExponent.SetBinary(PAnsiChar(
            PubNode.Items[1].BerDataAddress), PubNode.Items[1].BerDataLength);
          HasPub := True;
        end;

        if not HasPub then
          Exit;

        // 找到签名算法
        if HashNode.Count = 2 then
        begin
          if CompareObjectIdentifier(HashNode.Items[0], @OID_SHA1_RSAENCRYPTION[0],
            SizeOf(OID_SHA1_RSAENCRYPTION)) then
            CertificateRequest.SignDigestType := sdtSHA1;
          // TODO: 支持更多算法
        end;

        // 复制签名内容，跳过 BIT String 的前导对齐 0
        FreeMemory(CertificateRequest.SignValue);
        CertificateRequest.SignLength := SignNode.BerDataLength - 1;
        CertificateRequest.SignValue := GetMemory(CertificateRequest.SignLength);
        P := Pointer(Integer(SignNode.BerDataAddress) + 1);
        CopyMemory(CertificateRequest.SignValue, P, CertificateRequest.SignLength);

        Result := True;
      end;
    finally
      Reader.Free;
      MemStream.Free;
    end;
  end;
end;

{ TCnCertificateInfo }

procedure TCnCertificateInfo.Assign(Source: TPersistent);
begin
  if Source is TCnCertificateInfo then
  begin
    FCountryName := (Source as TCnCertificateInfo).CountryName;
    FOrganizationName := (Source as TCnCertificateInfo).OrganizationName;
    FEmailAddress := (Source as TCnCertificateInfo).EmailAddress;
    FLocalityName := (Source as TCnCertificateInfo).LocalityName;
    FCommonName := (Source as TCnCertificateInfo).CommonName;
    FOrganizationalUnitName := (Source as TCnCertificateInfo).OrganizationalUnitName;
    FStateOrProvinceName := (Source as TCnCertificateInfo).StateOrProvinceName;
  end
  else
    inherited;
end;

function TCnCertificateInfo.ToString: string;
begin
  Result := 'CountryName: ' + FCountryName + SCRLF;
  Result := Result + 'StateOrProvinceName: ' + FStateOrProvinceName + SCRLF;
  Result := Result + 'LocalityName: ' + FLocalityName + SCRLF;
  Result := Result + 'OrganizationName: ' + FOrganizationName + SCRLF;
  Result := Result + 'OrganizationalUnitName: ' + FOrganizationalUnitName + SCRLF;
  Result := Result + 'CommonName: ' + FCommonName + SCRLF;
  Result := Result + 'EmailAddress: ' + FEmailAddress + SCRLF;
end;

{ TCnRSACertificateRequest }

constructor TCnRSACertificateRequest.Create;
begin
  inherited;
  FCertificateInfo := TCnCertificateInfo.Create;
  FPublicKey := TCnRSAPublicKey.Create;
end;

destructor TCnRSACertificateRequest.Destroy;
begin
  FCertificateInfo.Free;
  FPublicKey.Free;
  FreeMemory(FSignValue);
  inherited;
end;

procedure TCnRSACertificateRequest.SetCertificateInfo(
  const Value: TCnCertificateInfo);
begin
  FCertificateInfo.Assign(Value);
end;

procedure TCnRSACertificateRequest.SetPublicKey(
  const Value: TCnRSAPublicKey);
begin
  FPublicKey.Assign(Value);
end;

function TCnRSACertificateRequest.ToString: string;
begin
  Result := FCertificateInfo.ToString;
  Result := Result + SCRLF + 'Public Key Modulus: ' + FPublicKey.PubKeyProduct.ToDec;
  Result := Result + SCRLF + 'Public Key Exponent: ' + FPublicKey.PubKeyExponent.ToDec;
  Result := Result + SCRLF + 'Signature: ' + PrintHex(FSignValue, FSignLength);
end;

end.
