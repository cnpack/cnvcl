unit UnitCertificateAuthority;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, CnECC, CnRSA, CnCertificateAuthority;

type
  TFormCA = class(TForm)
    pgc1: TPageControl;
    tsRequest: TTabSheet;
    grpGenRequest: TGroupBox;
    lblKey: TLabel;
    edtRSAECCKey: TEdit;
    btnBrowseKey: TButton;
    lblContryName: TLabel;
    edtContryName: TEdit;
    lblStateOrProvinceName: TLabel;
    edtStateOrProvinceName: TEdit;
    lblLocalityName: TLabel;
    edtLocalityName: TEdit;
    lblOrgName: TLabel;
    edtOrgName: TEdit;
    lblOrgUnitName: TLabel;
    edtOrgUnitName: TEdit;
    lblCommonName: TLabel;
    edtCommonName: TEdit;
    edtEmail: TEdit;
    lblEmail: TLabel;
    lblHash: TLabel;
    cbbHash: TComboBox;
    btnGenerateCSR: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    grpParse: TGroupBox;
    lblCSR: TLabel;
    edtCSR: TEdit;
    btnBrowseCSR: TButton;
    mmoCSRParse: TMemo;
    tsSign: TTabSheet;
    grpSign: TGroupBox;
    lblSignCSR: TLabel;
    edtSignCSR: TEdit;
    btnSignCSRBrowse: TButton;
    lblRoot: TLabel;
    edtSignKey: TEdit;
    btnSignKeyBrowse: TButton;
    btnSign: TButton;
    grpParseCER: TGroupBox;
    lblCRT: TLabel;
    edtCRT: TEdit;
    btnBrowseCRT: TButton;
    mmoCRT: TMemo;
    lblRootCrt: TLabel;
    edtRootCRT: TEdit;
    btnRootCRTBrowse: TButton;
    btnSelfSign: TButton;
    btnParseCSR: TButton;
    btnParseCRT: TButton;
    btnVerifyCSR: TButton;
    btnVerifySelfSignedCRT: TButton;
    btnVerifyCRT: TButton;
    tsMisc: TTabSheet;
    grpMisc: TGroupBox;
    btnGetWinRoot: TButton;
    lstCerts: TListBox;
    mmoCertInfo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseCSRClick(Sender: TObject);
    procedure btnBrowseKeyClick(Sender: TObject);
    procedure btnParseCSRClick(Sender: TObject);
    procedure btnGenerateCSRClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBrowseCRTClick(Sender: TObject);
    procedure btnParseCRTClick(Sender: TObject);
    procedure btnVerifyCSRClick(Sender: TObject);
    procedure btnSelfSignClick(Sender: TObject);
    procedure btnVerifySelfSignedCRTClick(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure btnSignCSRBrowseClick(Sender: TObject);
    procedure btnSignKeyBrowseClick(Sender: TObject);
    procedure btnRootCRTBrowseClick(Sender: TObject);
    procedure btnVerifyCRTClick(Sender: TObject);
    procedure btnGetWinRootClick(Sender: TObject);
    procedure lstCertsClick(Sender: TObject);
  private
    FClientRsaPriv: TCnRSAPrivateKey;
    FClientRsaPub: TCnRSAPublicKey;
    FServerRsaPriv: TCnRSAPrivateKey;
    FServerRsaPub: TCnRSAPublicKey;
    FClientEccPriv: TCnEccPrivateKey;
    FClientEccPub: TCnEccPublicKey;
    FClientCurveType: TCnEccCurveType;
    FServerEccPriv: TCnEccPrivateKey;
    FServerEccPub: TCnEccPublicKey;
    FServerCurveType: TCnEccCurveType;
  public
    { Public declarations }
  end;

var
  FormCA: TFormCA;

implementation

{$R *.DFM}

uses
  CnNative;

const
  crypt32 = 'crypt32.dll';

//+-------------------------------------------------------------------------
//  Certificate versions
//--------------------------------------------------------------------------

const
  CERT_V1 = 0;
  {$EXTERNALSYM CERT_V1}
  CERT_V2 = 1;
  {$EXTERNALSYM CERT_V2}
  CERT_V3 = 2;
  {$EXTERNALSYM CERT_V3}

//+-------------------------------------------------------------------------
//  Certificate Information Flags
//--------------------------------------------------------------------------

  CERT_INFO_VERSION_FLAG                 = 1;
  {$EXTERNALSYM CERT_INFO_VERSION_FLAG}
  CERT_INFO_SERIAL_NUMBER_FLAG           = 2;
  {$EXTERNALSYM CERT_INFO_SERIAL_NUMBER_FLAG}
  CERT_INFO_SIGNATURE_ALGORITHM_FLAG     = 3;
  {$EXTERNALSYM CERT_INFO_SIGNATURE_ALGORITHM_FLAG}
  CERT_INFO_ISSUER_FLAG                  = 4;
  {$EXTERNALSYM CERT_INFO_ISSUER_FLAG}
  CERT_INFO_NOT_BEFORE_FLAG              = 5;
  {$EXTERNALSYM CERT_INFO_NOT_BEFORE_FLAG}
  CERT_INFO_NOT_AFTER_FLAG               = 6;
  {$EXTERNALSYM CERT_INFO_NOT_AFTER_FLAG}
  CERT_INFO_SUBJECT_FLAG                 = 7;
  {$EXTERNALSYM CERT_INFO_SUBJECT_FLAG}
  CERT_INFO_SUBJECT_PUBLIC_KEY_INFO_FLAG = 8;
  {$EXTERNALSYM CERT_INFO_SUBJECT_PUBLIC_KEY_INFO_FLAG}
  CERT_INFO_ISSUER_UNIQUE_ID_FLAG        = 9;
  {$EXTERNALSYM CERT_INFO_ISSUER_UNIQUE_ID_FLAG}
  CERT_INFO_SUBJECT_UNIQUE_ID_FLAG       = 10;
  {$EXTERNALSYM CERT_INFO_SUBJECT_UNIQUE_ID_FLAG}
  CERT_INFO_EXTENSION_FLAG               = 11;
  {$EXTERNALSYM CERT_INFO_EXTENSION_FLAG}


const
  CRYPT_ASN_ENCODING  = $00000001;
  {$EXTERNALSYM CRYPT_ASN_ENCODING}
  CRYPT_NDR_ENCODING  = $00000002;
  {$EXTERNALSYM CRYPT_NDR_ENCODING}
  X509_ASN_ENCODING   = $00000001;
  {$EXTERNALSYM X509_ASN_ENCODING}
  X509_NDR_ENCODING   = $00000002;
  {$EXTERNALSYM X509_NDR_ENCODING}
  PKCS_7_ASN_ENCODING = $00010000;
  {$EXTERNALSYM PKCS_7_ASN_ENCODING}
  PKCS_7_NDR_ENCODING = $00020000;
  {$EXTERNALSYM PKCS_7_NDR_ENCODING}

type
  HCRYPTPROV = TCnNativeUInt;
  {$EXTERNALSYM HCRYPTPROV}

  _CRYPTOAPI_BLOB = record
    cbData: DWORD;
    pbData: PByte;
  end;
  {$EXTERNALSYM _CRYPTOAPI_BLOB}
  CRYPT_INTEGER_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM CRYPT_INTEGER_BLOB}
  PCRYPT_INTEGER_BLOB = ^_CRYPTOAPI_BLOB;
  {$EXTERNALSYM PCRYPT_INTEGER_BLOB}
  CRYPT_OBJID_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM CRYPT_OBJID_BLOB}
  CERT_NAME_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM CERT_NAME_BLOB}

  PCERT_NAME_BLOB = ^_CRYPTOAPI_BLOB;
  {$EXTERNALSYM PCERT_NAME_BLOB}

  PCRYPT_BIT_BLOB = ^CRYPT_BIT_BLOB;
  {$EXTERNALSYM PCRYPT_BIT_BLOB}
  _CRYPT_BIT_BLOB = record
    cbData: DWORD;
    pbData: PByte;
    cUnusedBits: DWORD;
  end;
  {$EXTERNALSYM _CRYPT_BIT_BLOB}
  CRYPT_BIT_BLOB = _CRYPT_BIT_BLOB;
  {$EXTERNALSYM CRYPT_BIT_BLOB}


  PCRYPT_ALGORITHM_IDENTIFIER = ^CRYPT_ALGORITHM_IDENTIFIER;
  {$EXTERNALSYM PCRYPT_ALGORITHM_IDENTIFIER}
  _CRYPT_ALGORITHM_IDENTIFIER = record
    pszObjId: LPSTR;
    Parameters: CRYPT_OBJID_BLOB;
  end;
  {$EXTERNALSYM _CRYPT_ALGORITHM_IDENTIFIER}
  CRYPT_ALGORITHM_IDENTIFIER = _CRYPT_ALGORITHM_IDENTIFIER;
  {$EXTERNALSYM CRYPT_ALGORITHM_IDENTIFIER}


  PCERT_EXTENSION = ^CERT_EXTENSION;
  {$EXTERNALSYM PCERT_EXTENSION}
  _CERT_EXTENSION = record
    pszObjId: LPSTR;
    fCritical: BOOL;
    Value: CRYPT_OBJID_BLOB;
  end;
  {$EXTERNALSYM _CERT_EXTENSION}
  CERT_EXTENSION = _CERT_EXTENSION;
  {$EXTERNALSYM CERT_EXTENSION}


  PCERT_PUBLIC_KEY_INFO = ^CERT_PUBLIC_KEY_INFO;
  {$EXTERNALSYM PCERT_PUBLIC_KEY_INFO}
  _CERT_PUBLIC_KEY_INFO = record
    Algorithm: CRYPT_ALGORITHM_IDENTIFIER;
    PublicKey: CRYPT_BIT_BLOB;
  end;
  {$EXTERNALSYM _CERT_PUBLIC_KEY_INFO}
  CERT_PUBLIC_KEY_INFO = _CERT_PUBLIC_KEY_INFO;
  {$EXTERNALSYM CERT_PUBLIC_KEY_INFO}



  PCERT_INFO = ^CERT_INFO;
  {$EXTERNALSYM PCERT_INFO}
  _CERT_INFO = record
    dwVersion: DWORD;
    SerialNumber: CRYPT_INTEGER_BLOB;
    SignatureAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    Issuer: CERT_NAME_BLOB;
    NotBefore: FILETIME;
    NotAfter: FILETIME;
    Subject: CERT_NAME_BLOB;
    SubjectPublicKeyInfo: CERT_PUBLIC_KEY_INFO;
    IssuerUniqueId: CRYPT_BIT_BLOB;
    SubjectUniqueId: CRYPT_BIT_BLOB;
    cExtension: DWORD;
    rgExtension: PCERT_EXTENSION;
  end;
  {$EXTERNALSYM _CERT_INFO}
  CERT_INFO = _CERT_INFO;
  {$EXTERNALSYM CERT_INFO}

  HCERTSTORE = Pointer;
  {$EXTERNALSYM HCERTSTORE}

  PCERT_CONTEXT = ^CERT_CONTEXT;
  {$EXTERNALSYM PCERT_CONTEXT}
  _CERT_CONTEXT = record
    dwCertEncodingType: DWORD;
    pbCertEncoded: PByte;
    cbCertEncoded: DWORD;
    pCertInfo: PCERT_INFO;
    hCertStore: HCERTSTORE;
  end;
  {$EXTERNALSYM _CERT_CONTEXT}
  CERT_CONTEXT = _CERT_CONTEXT;
  {$EXTERNALSYM CERT_CONTEXT}
  PCCERT_CONTEXT = PCERT_CONTEXT;
  {$EXTERNALSYM PCCERT_CONTEXT}

function CertOpenStore(lpszStoreProvider: LPCSTR; dwEncodingType: DWORD;
  hCryptProv: HCRYPTPROV; dwFlags: DWORD; pvPara: Pointer): HCERTSTORE; stdcall;
  external crypt32 name 'CertOpenStore' {$IFDEF SUPPORT_EXTERNAL_DELAYED} delayed {$ENDIF};
{$EXTERNALSYM CertOpenStore}

const
  CERT_CLOSE_STORE_FORCE_FLAG = $00000001;
  {$EXTERNALSYM CERT_CLOSE_STORE_FORCE_FLAG}
  CERT_CLOSE_STORE_CHECK_FLAG = $00000002;
  {$EXTERNALSYM CERT_CLOSE_STORE_CHECK_FLAG}


function CertCloseStore(hCertStore: HCERTSTORE; dwFlags: DWORD): BOOL; stdcall;
  external crypt32 name 'CertCloseStore' {$IFDEF SUPPORT_EXTERNAL_DELAYED} delayed {$ENDIF};
{$EXTERNALSYM CertCloseStore}

function CertEnumCertificatesInStore(hCertStore: HCERTSTORE;
  pPrevCertContext: PCCERT_CONTEXT): PCCERT_CONTEXT; stdcall;
  external crypt32 name 'CertEnumCertificatesInStore' {$IFDEF SUPPORT_EXTERNAL_DELAYED} delayed {$ENDIF};
{$EXTERNALSYM CertEnumCertificatesInStore}

function CertFindCertificateInStore(hCertStore: HCERTSTORE;
  dwCertEncodingType, dwFindFlags, dwFindType: DWORD; pvFindPara: Pointer;
  pPrevCertContext: PCCERT_CONTEXT): PCCERT_CONTEXT; stdcall;
  external crypt32 name 'CertFindCertificateInStore' {$IFDEF SUPPORT_EXTERNAL_DELAYED} delayed {$ENDIF};
{$EXTERNALSYM CertFindCertificateInStore}

//+-------------------------------------------------------------------------
// Certificate comparison functions
//--------------------------------------------------------------------------

const
  CERT_COMPARE_MASK           = $FFFF;
  {$EXTERNALSYM CERT_COMPARE_MASK}
  CERT_COMPARE_SHIFT          = 16;
  {$EXTERNALSYM CERT_COMPARE_SHIFT}
  CERT_COMPARE_ANY            = 0;
  {$EXTERNALSYM CERT_COMPARE_ANY}
  CERT_COMPARE_SHA1_HASH      = 1;
  {$EXTERNALSYM CERT_COMPARE_SHA1_HASH}
  CERT_COMPARE_NAME           = 2;
  {$EXTERNALSYM CERT_COMPARE_NAME}
  CERT_COMPARE_ATTR           = 3;
  {$EXTERNALSYM CERT_COMPARE_ATTR}
  CERT_COMPARE_MD5_HASH       = 4;
  {$EXTERNALSYM CERT_COMPARE_MD5_HASH}
  CERT_COMPARE_PROPERTY       = 5;
  {$EXTERNALSYM CERT_COMPARE_PROPERTY}
  CERT_COMPARE_PUBLIC_KEY     = 6;
  {$EXTERNALSYM CERT_COMPARE_PUBLIC_KEY}
  CERT_COMPARE_HASH           = CERT_COMPARE_SHA1_HASH;
  {$EXTERNALSYM CERT_COMPARE_HASH}
  CERT_COMPARE_NAME_STR_A     = 7;
  {$EXTERNALSYM CERT_COMPARE_NAME_STR_A}
  CERT_COMPARE_NAME_STR_W     = 8;
  {$EXTERNALSYM CERT_COMPARE_NAME_STR_W}
  CERT_COMPARE_KEY_SPEC       = 9;
  {$EXTERNALSYM CERT_COMPARE_KEY_SPEC}
  CERT_COMPARE_ENHKEY_USAGE   = 10;
  {$EXTERNALSYM CERT_COMPARE_ENHKEY_USAGE}
  CERT_COMPARE_CTL_USAGE      = CERT_COMPARE_ENHKEY_USAGE;
  {$EXTERNALSYM CERT_COMPARE_CTL_USAGE}
  CERT_COMPARE_SUBJECT_CERT   = 11;
  {$EXTERNALSYM CERT_COMPARE_SUBJECT_CERT}
  CERT_COMPARE_ISSUER_OF      = 12;
  {$EXTERNALSYM CERT_COMPARE_ISSUER_OF}
  CERT_COMPARE_EXISTING       = 13;
  {$EXTERNALSYM CERT_COMPARE_EXISTING}
  CERT_COMPARE_SIGNATURE_HASH = 14;
  {$EXTERNALSYM CERT_COMPARE_SIGNATURE_HASH}
  CERT_COMPARE_KEY_IDENTIFIER = 15;
  {$EXTERNALSYM CERT_COMPARE_KEY_IDENTIFIER}
  CERT_COMPARE_CERT_ID        = 16;
  {$EXTERNALSYM CERT_COMPARE_CERT_ID}

//+-------------------------------------------------------------------------
//  dwFindType
//
//  The dwFindType definition consists of two components:
//   - comparison function
//   - certificate information flag
//--------------------------------------------------------------------------

  CERT_FIND_ANY            = CERT_COMPARE_ANY shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_ANY}
  CERT_FIND_SHA1_HASH      = CERT_COMPARE_SHA1_HASH shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_SHA1_HASH}
  CERT_FIND_MD5_HASH       = CERT_COMPARE_MD5_HASH shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_MD5_HASH}
  CERT_FIND_SIGNATURE_HASH = CERT_COMPARE_SIGNATURE_HASH shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_SIGNATURE_HASH}
  CERT_FIND_KEY_IDENTIFIER = CERT_COMPARE_KEY_IDENTIFIER shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_KEY_IDENTIFIER}
  CERT_FIND_HASH           = CERT_FIND_SHA1_HASH;
  {$EXTERNALSYM CERT_FIND_HASH}
  CERT_FIND_PROPERTY       = CERT_COMPARE_PROPERTY shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_PROPERTY}
  CERT_FIND_PUBLIC_KEY     = CERT_COMPARE_PUBLIC_KEY shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_PUBLIC_KEY}
  CERT_FIND_SUBJECT_NAME   = CERT_COMPARE_NAME shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG;
  {$EXTERNALSYM CERT_FIND_SUBJECT_NAME}
  CERT_FIND_SUBJECT_ATTR   = CERT_COMPARE_ATTR shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG;
  {$EXTERNALSYM CERT_FIND_SUBJECT_ATTR}
  CERT_FIND_ISSUER_NAME    = CERT_COMPARE_NAME shl CERT_COMPARE_SHIFT or CERT_INFO_ISSUER_FLAG;
  {$EXTERNALSYM CERT_FIND_ISSUER_NAME}
  CERT_FIND_ISSUER_ATTR    = CERT_COMPARE_ATTR shl CERT_COMPARE_SHIFT or CERT_INFO_ISSUER_FLAG;
  {$EXTERNALSYM CERT_FIND_ISSUER_ATTR}
  CERT_FIND_SUBJECT_STR_A  = CERT_COMPARE_NAME_STR_A shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG;
  {$EXTERNALSYM CERT_FIND_SUBJECT_STR_A}
  CERT_FIND_SUBJECT_STR_W  = CERT_COMPARE_NAME_STR_W shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG;
  {$EXTERNALSYM CERT_FIND_SUBJECT_STR_W}
  CERT_FIND_SUBJECT_STR    = CERT_FIND_SUBJECT_STR_W;
  {$EXTERNALSYM CERT_FIND_SUBJECT_STR}
  CERT_FIND_ISSUER_STR_A   = CERT_COMPARE_NAME_STR_A shl CERT_COMPARE_SHIFT or CERT_INFO_ISSUER_FLAG;
  {$EXTERNALSYM CERT_FIND_ISSUER_STR_A}
  CERT_FIND_ISSUER_STR_W   = CERT_COMPARE_NAME_STR_W shl CERT_COMPARE_SHIFT or CERT_INFO_ISSUER_FLAG;
  {$EXTERNALSYM CERT_FIND_ISSUER_STR_W}
  CERT_FIND_ISSUER_STR     = CERT_FIND_ISSUER_STR_W;
  {$EXTERNALSYM CERT_FIND_ISSUER_STR}
  CERT_FIND_KEY_SPEC       = CERT_COMPARE_KEY_SPEC shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_KEY_SPEC}
  CERT_FIND_ENHKEY_USAGE   = CERT_COMPARE_ENHKEY_USAGE shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_ENHKEY_USAGE}
  CERT_FIND_CTL_USAGE      = CERT_FIND_ENHKEY_USAGE;
  {$EXTERNALSYM CERT_FIND_CTL_USAGE}

  CERT_FIND_SUBJECT_CERT = CERT_COMPARE_SUBJECT_CERT shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_SUBJECT_CERT}
  CERT_FIND_ISSUER_OF    = CERT_COMPARE_ISSUER_OF shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_ISSUER_OF}
  CERT_FIND_EXISTING     = CERT_COMPARE_EXISTING shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_EXISTING}
  CERT_FIND_CERT_ID      = CERT_COMPARE_CERT_ID shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_CERT_ID}

//+-------------------------------------------------------------------------
//  CERT_FIND_ANY
//
//  Find any certificate.
//
//  pvFindPara isn't used.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_HASH
//
//  Find a certificate with the specified hash.
//
//  pvFindPara points to a CRYPT_HASH_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_KEY_IDENTIFIER
//
//  Find a certificate with the specified KeyIdentifier. Gets the
//  CERT_KEY_IDENTIFIER_PROP_ID property and compares with the input
//  CRYPT_HASH_BLOB.
//
//  pvFindPara points to a CRYPT_HASH_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_PROPERTY
//
//  Find a certificate having the specified property.
//
//  pvFindPara points to a DWORD containing the PROP_ID
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_PUBLIC_KEY
//
//  Find a certificate matching the specified public key.
//
//  pvFindPara points to a CERT_PUBLIC_KEY_INFO containing the public key
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_SUBJECT_NAME
//  CERT_FIND_ISSUER_NAME
//
//  Find a certificate with the specified subject/issuer name. Does an exact
//  match of the entire name.
//
//  Restricts search to certificates matching the dwCertEncodingType.
//
//  pvFindPara points to a CERT_NAME_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_SUBJECT_ATTR
//  CERT_FIND_ISSUER_ATTR
//
//  Find a certificate with the specified subject/issuer attributes.
//
//  Compares the attributes in the subject/issuer name with the
//  Relative Distinguished Name's (CERT_RDN) array of attributes specified in
//  pvFindPara. The comparison iterates through the CERT_RDN attributes and looks
//  for an attribute match in any of the subject/issuer's RDNs.
//
//  The CERT_RDN_ATTR fields can have the following special values:
//    pszObjId == NULL              - ignore the attribute object identifier
//    dwValueType == RDN_ANY_TYPE   - ignore the value type
//    Value.pbData == NULL          - match any value
//
//  CERT_CASE_INSENSITIVE_IS_RDN_ATTRS_FLAG should be set in dwFindFlags to do
//  a case insensitive match. Otherwise, defaults to an exact, case sensitive
//  match.
//
//  CERT_UNICODE_IS_RDN_ATTRS_FLAG should be set in dwFindFlags if the RDN was
//  initialized with unicode strings as for
//  CryptEncodeObject(X509_UNICODE_NAME).
//
//  Restricts search to certificates matching the dwCertEncodingType.
//
//  pvFindPara points to a CERT_RDN (defined in wincert.h).
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_SUBJECT_STR_A
//  CERT_FIND_SUBJECT_STR_W | CERT_FIND_SUBJECT_STR
//  CERT_FIND_ISSUER_STR_A
//  CERT_FIND_ISSUER_STR_W  | CERT_FIND_ISSUER_STR
//
//  Find a certificate containing the specified subject/issuer name string.
//
//  First, the certificate's subject/issuer is converted to a name string
//  via CertNameToStrA/CertNameToStrW(CERT_SIMPLE_NAME_STR). Then, a
//  case insensitive substring within string match is performed.
//
//  Restricts search to certificates matching the dwCertEncodingType.
//
//  For *_STR_A, pvFindPara points to a null terminated character string.
//  For *_STR_W, pvFindPara points to a null terminated wide character string.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_KEY_SPEC
//
//  Find a certificate having a CERT_KEY_SPEC_PROP_ID property matching
//  the specified KeySpec.
//
//  pvFindPara points to a DWORD containing the KeySpec.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_ENHKEY_USAGE
//
//  Find a certificate having the szOID_ENHANCED_KEY_USAGE extension or
//  the CERT_ENHKEY_USAGE_PROP_ID and matching the specified pszUsageIdentifers.
//
//  pvFindPara points to a CERT_ENHKEY_USAGE data structure. If pvFindPara
//  is NULL or CERT_ENHKEY_USAGE's cUsageIdentifier is 0, then, matches any
//  certificate having enhanced key usage.
//
//  If the CERT_FIND_VALID_ENHKEY_USAGE_FLAG is set, then, only does a match
//  for certificates that are valid for the specified usages. By default,
//  the ceriticate must be valid for all usages. CERT_FIND_OR_ENHKEY_USAGE_FLAG
//  can be set, if the certificate only needs to be valid for one of the
//  specified usages. Note, CertGetValidUsages() is called to get the
//  certificate's list of valid usages. Only the CERT_FIND_OR_ENHKEY_USAGE_FLAG
//  is applicable when this flag is set.
//
//  The CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG can be set in dwFindFlags to
//  also match a certificate without either the extension or property.
//
//  If CERT_FIND_NO_ENHKEY_USAGE_FLAG is set in dwFindFlags, finds
//  certificates without the key usage extension or property. Setting this
//  flag takes precedence over pvFindPara being NULL.
//
//  If the CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG is set, then, only does a match
//  using the extension. If pvFindPara is NULL or cUsageIdentifier is set to
//  0, finds certificates having the extension. If
//  CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG is set, also matches a certificate
//  without the extension. If CERT_FIND_NO_ENHKEY_USAGE_FLAG is set, finds
//  certificates without the extension.
//
//  If the CERT_FIND_PROP_ONLY_ENHKEY_USAGE_FLAG is set, then, only does a match
//  using the property. If pvFindPara is NULL or cUsageIdentifier is set to
//  0, finds certificates having the property. If
//  CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG is set, also matches a certificate
//  without the property. If CERT_FIND_NO_ENHKEY_USAGE_FLAG is set, finds
//  certificates without the property.
//
//  If CERT_FIND_OR_ENHKEY_USAGE_FLAG is set, does an "OR" match of any of
//  the specified pszUsageIdentifiers. If not set, then, does an "AND" match
//  of all of the specified pszUsageIdentifiers.
//--------------------------------------------------------------------------

const
  CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG  = $1;
  {$EXTERNALSYM CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG}
  CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG  = $2;
  {$EXTERNALSYM CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG}
  CERT_FIND_PROP_ONLY_ENHKEY_USAGE_FLAG = $4;
  {$EXTERNALSYM CERT_FIND_PROP_ONLY_ENHKEY_USAGE_FLAG}
  CERT_FIND_NO_ENHKEY_USAGE_FLAG        = $8;
  {$EXTERNALSYM CERT_FIND_NO_ENHKEY_USAGE_FLAG}
  CERT_FIND_OR_ENHKEY_USAGE_FLAG        = $10;
  {$EXTERNALSYM CERT_FIND_OR_ENHKEY_USAGE_FLAG}
  CERT_FIND_VALID_ENHKEY_USAGE_FLAG     = $20;
  {$EXTERNALSYM CERT_FIND_VALID_ENHKEY_USAGE_FLAG}

  CERT_FIND_OPTIONAL_CTL_USAGE_FLAG = CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG;
  {$EXTERNALSYM CERT_FIND_OPTIONAL_CTL_USAGE_FLAG}

  CERT_FIND_EXT_ONLY_CTL_USAGE_FLAG = CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG;
  {$EXTERNALSYM CERT_FIND_EXT_ONLY_CTL_USAGE_FLAG}

  CERT_FIND_PROP_ONLY_CTL_USAGE_FLAG = CERT_FIND_PROP_ONLY_ENHKEY_USAGE_FLAG;
  {$EXTERNALSYM CERT_FIND_PROP_ONLY_CTL_USAGE_FLAG}

  CERT_FIND_NO_CTL_USAGE_FLAG    = CERT_FIND_NO_ENHKEY_USAGE_FLAG;
  {$EXTERNALSYM CERT_FIND_NO_CTL_USAGE_FLAG}
  CERT_FIND_OR_CTL_USAGE_FLAG    = CERT_FIND_OR_ENHKEY_USAGE_FLAG;
  {$EXTERNALSYM CERT_FIND_OR_CTL_USAGE_FLAG}
  CERT_FIND_VALID_CTL_USAGE_FLAG = CERT_FIND_VALID_ENHKEY_USAGE_FLAG;
  {$EXTERNALSYM CERT_FIND_VALID_CTL_USAGE_FLAG}

//+-------------------------------------------------------------------------
//  CERT_FIND_CERT_ID
//
//  Find a certificate with the specified CERT_ID.
//
//  pvFindPara points to a CERT_ID.
//--------------------------------------------------------------------------


function CertFreeCertificateContext(pCertContext: PCCERT_CONTEXT): BOOL; stdcall;
  external crypt32 name 'CertFreeCertificateContext' {$IFDEF SUPPORT_EXTERNAL_DELAYED} delayed {$ENDIF};
{$EXTERNALSYM CertFreeCertificateContext}


function CertNameToStr(dwCertEncodingType: DWORD; pName: PCERT_NAME_BLOB;
  dwStrType: DWORD; psz: LPTSTR; csz: DWORD): DWORD; stdcall;
  external crypt32 name 'CertNameToStrW' {$IFDEF SUPPORT_EXTERNAL_DELAYED} delayed {$ENDIF};
{$EXTERNALSYM CertNameToStr}

//+-------------------------------------------------------------------------
//  Certificate name string types
//--------------------------------------------------------------------------

const
  CERT_SIMPLE_NAME_STR = 1;
  {$EXTERNALSYM CERT_SIMPLE_NAME_STR}
  CERT_OID_NAME_STR    = 2;
  {$EXTERNALSYM CERT_OID_NAME_STR}
  CERT_X500_NAME_STR   = 3;
  {$EXTERNALSYM CERT_X500_NAME_STR}


function CertGetNameString(pCertContext: PCCERT_CONTEXT; dwType, dwFlags: DWORD;
  pvTypePara: Pointer; pszNameString: LPWSTR; cchNameString: DWORD): DWORD; stdcall;
  external crypt32 name 'CertGetNameStringW' {$IFDEF SUPPORT_EXTERNAL_DELAYED} delayed {$ENDIF};
{$EXTERNALSYM CertGetNameString}

//+-------------------------------------------------------------------------
//  Certificate name types
//--------------------------------------------------------------------------

const
  CERT_NAME_EMAIL_TYPE            = 1;
  {$EXTERNALSYM CERT_NAME_EMAIL_TYPE}
  CERT_NAME_RDN_TYPE              = 2;
  {$EXTERNALSYM CERT_NAME_RDN_TYPE}
  CERT_NAME_ATTR_TYPE             = 3;
  {$EXTERNALSYM CERT_NAME_ATTR_TYPE}
  CERT_NAME_SIMPLE_DISPLAY_TYPE   = 4;
  {$EXTERNALSYM CERT_NAME_SIMPLE_DISPLAY_TYPE}
  CERT_NAME_FRIENDLY_DISPLAY_TYPE = 5;
  {$EXTERNALSYM CERT_NAME_FRIENDLY_DISPLAY_TYPE}

//+-------------------------------------------------------------------------
//  Certificate name flags
//--------------------------------------------------------------------------

  CERT_NAME_ISSUER_FLAG           = $1;
  {$EXTERNALSYM CERT_NAME_ISSUER_FLAG}
  CERT_NAME_DISABLE_IE4_UTF8_FLAG = $00010000;
  {$EXTERNALSYM CERT_NAME_DISABLE_IE4_UTF8_FLAG}


function CertOpenSystemStore(hProv: HCRYPTPROV; szSubsystemProtocol: LPCTSTR): HCERTSTORE; stdcall;
  external crypt32 name 'CertOpenSystemStoreW' {$IFDEF SUPPORT_EXTERNAL_DELAYED} delayed {$ENDIF};
{$EXTERNALSYM CertOpenSystemStore}

function FindCertWithSerialNumber(AStore: HCERTSTORE;
                                  ASerialNumber: string): PCERT_CONTEXT;
var
  PrevContext, CurContext: PCERT_CONTEXT;
  CertInfo: string;
begin
  Result := nil;
  if AStore <> nil then
  begin
    PrevContext := nil;
    CurContext := CertEnumCertificatesInStore(AStore, PrevContext);
    while CurContext <> nil do
    begin
      CertInfo := DataToHex(CurContext^.pCertInfo^.SerialNumber.pbData, CurContext^.pCertInfo^.SerialNumber.cbData);
      if SameText(CertInfo, ASerialNumber) then
      begin
        Result := CurContext;
        Exit;
      end;
      PrevContext := CurContext;
      CurContext := CertEnumCertificatesInStore(AStore, PrevContext);
    end;
  end;
end;

function GetCertInfo(Context: PCERT_CONTEXT; InfoFlag: Integer = 0;
  InfoType: Integer = CERT_NAME_SIMPLE_DISPLAY_TYPE): WideString;
var
  cbSize: DWORD;
begin
  Result := '';
  cbSize := CertGetNameString(Context, InfoType, InfoFlag, nil, nil, 0);
  if cbSize > 0 then
  begin
    SetLength(Result, cbSize - 1);
    CertGetNameString(Context, InfoType, InfoFlag, nil, PWideChar(Result), cbSize);
  end;
end;

procedure TFormCA.FormCreate(Sender: TObject);
begin
  cbbHash.ItemIndex := 1;
  FClientRsaPriv := TCnRSAPrivateKey.Create;
  FClientRsaPub := TCnRSAPublicKey.Create;
  FServerRsaPriv := TCnRSAPrivateKey.Create;
  FServerRsaPub := TCnRSAPublicKey.Create;
  FClientEccPriv := TCnEccPrivateKey.Create;
  FClientEccPub := TCnEccPublicKey.Create;
  FServerEccPriv := TCnEccPrivateKey.Create;
  FServerEccPub := TCnEccPublicKey.Create;
end;

procedure TFormCA.btnBrowseCSRClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtCSR.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnBrowseKeyClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtRSAECCKey.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnParseCSRClick(Sender: TObject);
var
  CSR: TCnCertificateRequest;
  OutBuf: TBytes;
  OutLen: Integer;
begin
  CSR := TCnCertificateRequest.Create;
  if CnCALoadCertificateSignRequestFromFile(edtCSR.Text, CSR) then
  begin
    mmoCSRParse.Clear;
    mmoCSRParse.Lines.Add(CSR.ToString);

    if CSR.IsRSA and (CSR.SignValue <> nil) and (CSR.SignLength > 0) and (CSR.RSAPublicKey.BitsCount > 128) then
    begin
      SetLength(OutBuf, CSR.RSAPublicKey.BitsCount div 8);
      if CnRSADecryptRawData(CSR.SignValue, CSR.SignLength, @OutBuf[0], OutLen, CSR.RSAPublicKey) then
      begin
        mmoCSRParse.Lines.Add('');
        mmoCSRParse.Lines.Add('--------');
        mmoCSRParse.Lines.Add('Digest after RSA Decryption:');
        mmoCSRParse.Lines.Add(DataToHex(@OutBuf[0], OutLen));
      end;
    end;
  end
  else
    ShowMessage('Parse CSR Failed.');
  CSR.Free;
end;

function ConvertItemIndexToCASignType(ItemIndex: Integer; IsRSA: Boolean): TCnCASignType;
begin
  if IsRSA then
    Result := TCnCASignType(ItemIndex)
  else
    Result := TCnCASignType(ItemIndex + 3);
end;

procedure TFormCA.btnGenerateCSRClick(Sender: TObject);
begin
  if FileExists(edtRSAECCKey.Text) then
  begin
    if CnRSALoadKeysFromPem(edtRSAECCKey.Text, FClientRsaPriv, FClientRsaPub) then
    begin
      if dlgSave.Execute then
      begin
        if CnCANewCertificateSignRequest(FClientRsaPriv, FClientRsaPub, dlgSave.FileName, edtContryName.Text,
          edtStateOrProvinceName.Text, edtLocalityName.Text, edtOrgName.Text, edtOrgUnitName.Text,
          edtCommonName.Text, edtEmail.Text, ConvertItemIndexToCASignType(cbbHash.ItemIndex, True)) then
          ShowMessage('Generate RSA CSR File Success.')
        else
          ShowMessage('Generate RSA CSR File Fail.');
      end;
    end
    else if CnEccLoadKeysFromPem(edtRSAECCKey.Text, FClientEccPriv, FClientEccPub, FClientCurveType) then
    begin
      if dlgSave.Execute then
      begin
        if CnCANewCertificateSignRequest(FClientEccPriv, FClientEccPub, FClientCurveType,
          dlgSave.FileName, edtContryName.Text, edtStateOrProvinceName.Text,
          edtLocalityName.Text, edtOrgName.Text, edtOrgUnitName.Text, edtCommonName.Text,
          edtEmail.Text, ConvertItemIndexToCASignType(cbbHash.ItemIndex, False)) then
          ShowMessage('Generate ECC CSR File Success.')
        else
          ShowMessage('Generate ECC CSR File Fail.');
      end;
    end;
  end
  else
    ShowMessage('Invalid RSA or ECC Keys');
end;

procedure TFormCA.FormDestroy(Sender: TObject);
begin
  FClientEccPriv.Free;
  FClientEccPub.Free;
  FServerEccPriv.Free;
  FServerEccPub.Free;

  FServerRsaPub.Free;
  FServerRsaPriv.Free;
  FClientRsaPub.Free;
  FClientRsaPriv.Free;
end;

procedure TFormCA.btnBrowseCRTClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtCRT.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnParseCRTClick(Sender: TObject);
var
  CRT: TCnCertificate;
begin
  CRT := TCnCertificate.Create;
  if not CnCALoadCertificateFromFile(edtCRT.Text, CRT) then
    ShowMessage('Parse CRT File Failed.')
  else
  begin
    mmoCRT.Clear;
    mmoCRT.Lines.Add(CRT.ToString);
  end;
  CRT.Free;
end;

procedure TFormCA.btnVerifyCSRClick(Sender: TObject);
begin
  if CnCAVerifyCertificateSignRequestFile(edtCSR.Text) then
    ShowMessage('CSR Verify OK.')
  else
    ShowMessage('CSR Verify Fail.');
end;

procedure TFormCA.btnSelfSignClick(Sender: TObject);
begin
  if FileExists(edtRSAECCKey.Text) then
  begin
    if CnRSALoadKeysFromPem(edtRSAECCKey.Text, FClientRsaPriv, FClientRsaPub) then
    begin
      if dlgSave.Execute then
      begin
        if CnCANewSelfSignedCertificate(FClientRsaPriv, FClientRsaPub, dlgSave.FileName, edtContryName.Text,
          edtStateOrProvinceName.Text, edtLocalityName.Text, edtOrgName.Text,
          edtOrgUnitName.Text, edtCommonName.Text, edtEmail.Text, '1234567890987654321',
          Now - 1, Now + 365, ConvertItemIndexToCASignType(cbbHash.ItemIndex, True)) then
          ShowMessage('Self-Signed RSA CRT File OK.')
        else
          ShowMessage('Self-Signed RSA CRT File Fail.');
      end;
    end
    else
    begin
      if CnEccLoadKeysFromPem(edtRSAECCKey.Text, FClientEccPriv, FClientEccPub, FClientCurveType) then
      begin
        if dlgSave.Execute then
        begin
          if CnCANewSelfSignedCertificate(FClientEccPriv, FClientEccPub, FClientCurveType, dlgSave.FileName,
            edtContryName.Text, edtStateOrProvinceName.Text, edtLocalityName.Text, edtOrgName.Text,
            edtOrgUnitName.Text, edtCommonName.Text, edtEmail.Text, '1234567890987654321',
            Now - 1, Now + 365, ConvertItemIndexToCASignType(cbbHash.ItemIndex, False)) then
            ShowMessage('Self-Signed ECC CRT File OK.')
          else
            ShowMessage('Self-Signed ECC CRT File Fail.');
        end;
      end;
    end;
  end;
end;

procedure TFormCA.btnVerifySelfSignedCRTClick(Sender: TObject);
begin
  if CnCAVerifySelfSignedCertificateFile(edtCRT.Text) then
    ShowMessage('Self-Signed CRT Verify OK.')
  else
    ShowMessage('Self-Signed CRT Verify Fail.');
end;

procedure TFormCA.btnSignClick(Sender: TObject);
begin
  if FileExists(edtSignCSR.Text) and FileExists(edtRootCRT.Text) and FileExists(edtSignKey.Text) then
  begin
    if CnRSALoadKeysFromPem(edtSignKey.Text, FServerRsaPriv, FServerRsaPub) then
    begin
      if dlgSave.Execute then
      begin
        if CnCASignCertificate(FServerRsaPriv, edtRootCRT.Text, edtSignCSR.Text, dlgSave.FileName,
          '1234567890987654321', Now - 1, Now + 365, TCnCASignType(cbbHash.ItemIndex)) then
          ShowMessage('Sign CRT File OK.')
        else
          ShowMessage('Sign CRT File Fail.');
      end;
    end
    else if CnEccLoadKeysFromPem(edtSignKey.Text, FServerEccPriv, FServerEccPub, FServerCurveType) then
    begin
      if dlgSave.Execute then
      begin
        if CnCASignCertificate(FServerEccPriv, FServerCurveType, edtRootCRT.Text, edtSignCSR.Text, dlgSave.FileName,
          '1234567890987654321', Now - 1, Now + 365, TCnCASignType(cbbHash.ItemIndex + 3)) then
          ShowMessage('Sign CRT File OK.')
        else
          ShowMessage('Sign CRT File Fail.');
      end;
    end;
  end;
end;

procedure TFormCA.btnSignCSRBrowseClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtSignCSR.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnSignKeyBrowseClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtSignKey.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnRootCRTBrowseClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtRootCRT.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnVerifyCRTClick(Sender: TObject);
var
  ParentCRT: TCnCertificate;
  RSAPub: TCnRSAPublicKey;
  EccPub: TCnEccPublicKey;
  CurveType: TCnEccCurveType;
begin
  if FileExists(edtCRT.Text) and dlgOpen.Execute then
  begin
    ParentCRT := nil;
    RSAPub := nil;
    EccPub := nil;

    try
      // 读 Parent CRT 的被签发者信息、或 ECC/RSA 的父公钥
      ParentCRT := TCnCertificate.Create;
      RSAPub := TCnRSAPublicKey.Create;
      EccPub := TCnEccPublicKey.Create;

      if CnCALoadCertificateFromFile(dlgOpen.FileName, ParentCRT) then  // 是父证书
      begin
        if ParentCRT.BasicCertificate.SubjectIsRSA then
        begin
          if CnCAVerifyCertificateFile(edtCRT.Text, ParentCRT.BasicCertificate.SubjectRSAPublicKey) then
            ShowMessage('Verify CRT using RSA Parent CRT OK.')
          else
            ShowMessage('Verify CRT using RSA Parent CRT Fail.')
        end
        else
        begin
          if CnCAVerifyCertificateFile(edtCRT.Text, ParentCRT.BasicCertificate.SubjectEccPublicKey,
            ParentCRT.BasicCertificate.SubjectEccCurveType) then
            ShowMessage('Verify CRT using Ecc Parent CRT OK.')
          else
            ShowMessage('Verify CRT using Ecc Parent CRT Fail.');
        end;
      end
      else if CnRSALoadPublicKeyFromPem(dlgOpen.FileName, RSAPub) or  
        CnRSALoadKeysFromPem(dlgOpen.FileName, nil ,RSAPub) then        // 是父 RSA 公钥
      begin
        if CnCAVerifyCertificateFile(edtCRT.Text, RSAPub) then
            ShowMessage('Verify CRT using RSA Parent Public Key OK.')
          else
            ShowMessage('Verify CRT using RSA Parent Public Key Fail.');
      end
      else if CnEccLoadKeysFromPem(dlgOpen.FileName, nil, EccPub, CurveType) or
        CnEccLoadPublicKeyFromPem(dlgOpen.FileName, EccPub, CurveType) then  // 是父 ECC 公钥
      begin
        if CnCAVerifyCertificateFile(edtCRT.Text, EccPub, CurveType) then
            ShowMessage('Verify CRT using ECC Parent Public Key OK.')
          else
            ShowMessage('Verify CRT using ECC Parent Public Key Fail.');
      end;
    finally
      EccPub.Free;
      RSAPub.Free;
      ParentCRT.Free;
    end;
  end;
end;

procedure TFormCA.btnGetWinRootClick(Sender: TObject);
var
  SL: TStringList;
  hStore: HCERTSTORE;
  pCertContext: PCERT_CONTEXT;
  dwSize: DWORD;
  pName: PWideChar;
begin
  lstCerts.Items.Clear;

  SL := TStringList.Create;
  try
    // 打开受信任的根证书颁发机构存储
    hStore := CertOpenSystemStore(0, 'ROOT');
    if hStore = nil then
    begin
      raise Exception.Create('无法打开证书存储');
      Exit;
    end;

    try
      // 枚举存储中的所有证书
      pCertContext := nil;
      while True do
      begin
        pCertContext := CertEnumCertificatesInStore(hStore, pCertContext);
        if pCertContext = nil then
        begin
          // ShowMessage(IntToStr(GetLastError));
          Break;
        end;

        // 获取证书的发行者
        dwSize := CertGetNameString(pCertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE, CERT_NAME_ISSUER_FLAG, nil, nil, 0);
        if dwSize > 0 then
        begin
          GetMem(pName, dwSize * SizeOf(WideChar));
          try
            CertGetNameString(pCertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE, CERT_NAME_ISSUER_FLAG, nil, pName, dwSize);
            SL.Add(pName);
          finally
            FreeMem(pName);
          end;
        end;
      end;
    finally
      CertCloseStore(hStore, 0);
    end;

    if SL.Count > 0 then
    begin
      lstCerts.Items.Assign(SL);
      ShowMessage('发现证书数 ' + IntToStr(SL.Count));
    end
    else
      ShowMessage('未发现根证书');
  finally
    SL.Free;
  end;
end;

procedure TFormCA.lstCertsClick(Sender: TObject);
var
  SelIssuer, Issuer: WideString;
  hStore: HCERTSTORE;
  pCertContext: PCERT_CONTEXT;
  dwSize: DWORD;
  pName: PWideChar;
begin
  mmoCertInfo.Lines.Clear;
  if (lstCerts.Items.Count <= 0) or (lstCerts.ItemIndex <= 0) then
    Exit;

  SelIssuer := lstCerts.Items[lstCerts.ItemIndex];
  if SelIssuer = '' then
    Exit;

  // 打开受信任的根证书颁发机构存储
  hStore := CertOpenSystemStore(0, 'ROOT');
  if hStore = nil then
  begin
    raise Exception.Create('无法打开证书存储');
    Exit;
  end;

  try
    // 枚举存储中的所有证书
    pCertContext := nil;
    while True do
    begin
      pCertContext := CertEnumCertificatesInStore(hStore, pCertContext);
      if pCertContext = nil then
        Break;

      // 获取证书的发行者
      dwSize := CertGetNameString(pCertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE, CERT_NAME_ISSUER_FLAG, nil, nil, 0);
      if dwSize > 0 then
      begin
        GetMem(pName, dwSize * SizeOf(WideChar));
        try
          CertGetNameString(pCertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE, CERT_NAME_ISSUER_FLAG, nil, pName, dwSize);

          if SelIssuer = pName then
          begin
            // TODO: 获取该 pCertContext 的信息并输出到 mmoCertInfo 中

            Break;
          end;
        finally
          FreeMem(pName);
        end;
      end;
    end;
  finally
    CertCloseStore(hStore, 0);
  end;
end;

end.
