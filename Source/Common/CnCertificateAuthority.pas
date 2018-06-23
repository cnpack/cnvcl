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
* 备    注：生成客户端 CSR 文件做证书签名请求，
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
  SysUtils, Classes, Windows, CnRSA, CnBerUtils, CnMD5, CnSHA1, CnSHA2;

type
  TCnCASignType = (ctMd5RSA, ctSha1RSA, ctSha256RSA);
  {* 证书签名使用的散列签名算法，ctSha1RSA 表示先 Sha1 再 RSA}

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
    FCASignType: TCnCASignType;
    FSignValue: Pointer;
    FSignLength: Integer;
    FDigestLength: Integer;
    FDigestValue: Pointer;
    FDigestType: TCnRSASignDigestType;
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
    property CASignType: TCnCASignType read FCASignType write FCASignType;
    {* 客户端使用的散列与签名算法}
    property SignValue: Pointer read FSignValue write FSignValue;
    {* 散列后签名的结果}
    property SignLength: Integer read FSignLength write FSignLength;
    {* 散列后签名的结果长度}
    property DigestType: TCnRSASignDigestType read FDigestType write FDigestType;
    {* 客户端散列使用的散列算法，应与 CASignType 意义相等}
    property DigestValue: Pointer read FDigestValue write FDigestValue;
    {* 散列值}
    property DigestLength: Integer read FDigestLength write FDigestLength;
    {* 散列值的长度}
  end;

function CnCANewCertificateSignRequest(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCSRFile: string; const CountryName: string; const
  StateOrProvinceName: string; const LocalityName: string; const OrganizationName:
  string; const OrganizationalUnitName: string; const CommonName: string; const
  EmailAddress: string; CASignType: TCnCASignType = ctSha1RSA): Boolean;
{* 根据公私钥与一些 DN 信息以及指定散列算法生成 CSR 格式的证书请求文件}

function CnCALoadCertificateSignRequestFromFile(const FileName: string;
  CertificateRequest: TCnRSACertificateRequest): Boolean;
{* 解析 PEM 格式的 CSR 文件并将内容放入 TCnRSACertificateRequest 对象中}

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

function AddCASignTypeOIDNodeToWriter(AWriter: TCnBerWriter; CASignType: TCnCASignType;
  AParent: TCnBerWriteNode): TCnBerWriteNode;
begin
  Result := nil;
  case CASignType of
    ctSha1RSA:
      Result := AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SHA1_RSAENCRYPTION[0],
        SizeOf(OID_SHA1_RSAENCRYPTION), AParent);
    // TODO: 其它算法类型支持
  end;
end;

// 根据指定数字摘要算法计算数据的二进制散列值并写入 Stream
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

function CnCANewCertificateSignRequest(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCSRFile: string; const CountryName: string; const
  StateOrProvinceName: string; const LocalityName: string; const OrganizationName:
  string; const OrganizationalUnitName: string; const CommonName: string; const
  EmailAddress: string; CASignType: TCnCASignType): Boolean;
var
  B: Byte;
  OutLen: Integer;
  OutBuf: array of Byte;
  Writer, HashWriter: TCnBerWriter;
  Stream, DigestStream, ValueStream: TMemoryStream;
  Root, DNRoot, InfoRoot, PubNode, HashNode, Node, HashRoot: TCnBerWriteNode;

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
    Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, PubNode);
    Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_RSAENCRYPTION_PKCS1[0],
      SizeOf(OID_RSAENCRYPTION_PKCS1), Node);
    Writer.AddNullNode(Node);
    Node := Writer.AddContainerNode(CN_BER_TAG_BIT_STRING, PubNode);
    Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Node);
    AddBigNumberToWriter(Writer, PublicKey.PubKeyProduct, Node);
    AddBigNumberToWriter(Writer, PublicKey.PubKeyExponent, Node);

    // 拿出 InfoRoot 的数据
    ValueStream := TMemoryStream.Create;
    InfoRoot.SaveValueToStream(ValueStream);

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

    // RSA 私钥加密此 BER 块得到签名值
    SetLength(OutBuf, PrivateKey.BitsCount div 8);
    if not CnRSAEncryptRawData(DigestStream.Memory, DigestStream.Size,
      @OutBuf[0], OutLen, PrivateKey) then
      Exit;

    // 增加 Hash 算法说明
    HashNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);
    AddCASignTypeOIDNodeToWriter(Writer, CASignType, HashNode);
    Writer.AddNullNode(HashNode);

    // 写入最终签名值
    Writer.AddBasicNode(CN_BER_TAG_BIT_STRING, @OutBuf[0], OutLen, Root);

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
function CnCALoadCertificateSignRequestFromFile(const FileName: string;
  CertificateRequest: TCnRSACertificateRequest): Boolean;
var
  I: Integer;
  P: Pointer;
  IsRSA, HasPub: Boolean;
  Reader: TCnBerReader;
  MemStream: TMemoryStream;
  DNRoot, PubNode, HashNode, SignNode, Node, StrNode: TCnBerReadNode;
  OutBuf: array of Byte;
  OutLen: Integer;
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
            CertificateRequest.CASignType := ctSha1RSA;
          // TODO: 支持更多算法
        end;

        // 复制签名内容，跳过 BIT String 的前导对齐 0
        FreeMemory(CertificateRequest.SignValue);
        CertificateRequest.SignLength := SignNode.BerDataLength - 1;
        CertificateRequest.SignValue := GetMemory(CertificateRequest.SignLength);
        P := Pointer(Integer(SignNode.BerDataAddress) + 1);
        CopyMemory(CertificateRequest.SignValue, P, CertificateRequest.SignLength);

        // 解开 RSA 签名的内容得到 DER 编码的 Hash 值与算法
        SetLength(OutBuf, CertificateRequest.PublicKey.BitsCount div 8);
        if CnRSADecryptRawData(CertificateRequest.SignValue, CertificateRequest.SignLength,
          @OutBuf[0], OutLen, CertificateRequest.PublicKey) then
        begin
          FreeAndNil(Reader);
          Reader := TCnBerReader.Create(@OutBuf[0], OutLen);
          Reader.ParseToTree;

          if Reader.TotalCount < 5 then
            Exit;
          
          Node := Reader.Items[2];
          CertificateRequest.DigestType := GetDigestSignTypeFromBerOID(Node.BerDataAddress,
            Node.BerDataLength);
          if CertificateRequest.DigestType = sdtNone then
            Exit;

          // 获取 Ber 解出的散列值
          Node := Reader.Items[4];
          FreeMemory(CertificateRequest.DigestValue);
          CertificateRequest.DigestLength := Node.BerDataLength;
          CertificateRequest.DigestValue := GetMemory(CertificateRequest.DigestLength);
          CopyMemory(CertificateRequest.DigestValue, Node.BerDataAddress,
            CertificateRequest.DigestLength);
        end;
        Result := True;
      end;
    finally
      Reader.Free;
      MemStream.Free;
      SetLength(OutBuf, 0);
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
  FreeMemory(FDigestValue);
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
end.
