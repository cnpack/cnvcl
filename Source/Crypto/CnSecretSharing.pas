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

unit CnSecretSharing;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ����ܹ����㷨ʵ�ֵ�Ԫ��
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע������Ԫʵ�������빲������㷨��Ŀǰ����Ŀǰ���� Shamir ���޷����� Feldman VSS ������
*
*           Shamir ���޷��������ù������ʽ���ɶ�������겢���ò�ֵ��ԭ������ܹ�������
*           �������ǲ�ֺ������û����֤�Ƿ���ȷ�Ļ��ƣ�������˸Ľ���� Feldman VSS ������
*           ������Ҫ�õ����� P ���� P = 2*Q + 1 �� Q Ҳ����������Ƕ�� Shamir ģ Q��
*           ������֤�ļ�����ģ P
*
*           Shamir ���޷������� Split �� Reconstruct �������ϡ�
*
*           Feldman VSS ������֤������ Split��Verify �� Reconstruct �������ϡ�
* ����ƽ̨��Win7 + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2022.05.28 V1.1
*               �� Shamir �Ļ�����ʵ�� Feldman's VSS ����
*           2022.05.24 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnConsts, CnNative, CnPrime, CnContainers,
  CnPolynomial, CnRandom, CnBigNumber;

const
  CN_SHAMIR_DEFAULT_PRIME_BITS         = 1024;
  {* Shamir �㷨������Ĭ��λ��}

  // ������
  ECN_SECRET_OK                        = ECN_OK;
  {* ���ܹ���ϵ�д����룺�޴���ֵΪ 0}

  ECN_SECRET_ERROR_BASE                = ECN_CUSTOM_ERROR_BASE + $400;
  {* ���ܹ���ϵ�д�����Ļ�׼��ʼֵ��Ϊ ECN_CUSTOM_ERROR_BASE ���� $400}

  ECN_SECRET_INVALID_INPUT             = ECN_SECRET_ERROR_BASE + 1;
  {* ���ܹ��������֮����Ϊ�ջ򳤶ȴ���}
  ECN_SECRET_RANDOM_ERROR              = ECN_SECRET_ERROR_BASE + 2;
  {* ���ܹ��������֮�������ش���}
  ECN_SECRET_FELDMAN_CHECKERROR        = ECN_SECRET_ERROR_BASE + 3;
  {* ���ܹ��������֮ Feldman VSS ������}
  ECN_SECRET_PRIME_ERROR               = ECN_SECRET_ERROR_BASE + 4;
  {* ���ܹ��������֮������ش���}

//==============================================================================
//
//  RFC 7919 ���Ƽ��� Diffie-Hellman ʹ�õ���������һ�� 2 Ҳ������������Ԫ��Ϊ 2
//  ע��˴�������Ԫ 2 ��������ԭ���� G^q mod p <> 1 ��Ҫ��
//  �෴����Ȼ���� 1������ Feldman VSS ��Ҫ��
//
//==============================================================================

  CN_PRIME_FFDHE_2048 =
    'FFFFFFFFFFFFFFFFADF85458A2BB4A9AAFDC5620273D3CF1' +
    'D8B9C583CE2D3695A9E13641146433FBCC939DCE249B3EF9' +
    '7D2FE363630C75D8F681B202AEC4617AD3DF1ED5D5FD6561' +
    '2433F51F5F066ED0856365553DED1AF3B557135E7F57C935' +
    '984F0C70E0E68B77E2A689DAF3EFE8721DF158A136ADE735' +
    '30ACCA4F483A797ABC0AB182B324FB61D108A94BB2C8E3FB' +
    'B96ADAB760D7F4681D4F42A3DE394DF4AE56EDE76372BB19' +
    '0B07A7C8EE0A6D709E02FCE1CDF7E2ECC03404CD28342F61' +
    '9172FE9CE98583FF8E4F1232EEF28183C3FE3B1B4C6FAD73' +
    '3BB5FCBC2EC22005C58EF1837D1683B2C6F34A26C1B2EFFA' +
    '886B423861285C97FFFFFFFFFFFFFFFF';
  {* RFC 7919 ���Ƽ��� Diffie-Hellman ʹ�õ� 2048 λ����}

  CN_PRIME_FFDHE_3072 =
    'FFFFFFFFFFFFFFFFADF85458A2BB4A9AAFDC5620273D3CF1' +
    'D8B9C583CE2D3695A9E13641146433FBCC939DCE249B3EF9' +
    '7D2FE363630C75D8F681B202AEC4617AD3DF1ED5D5FD6561' +
    '2433F51F5F066ED0856365553DED1AF3B557135E7F57C935' +
    '984F0C70E0E68B77E2A689DAF3EFE8721DF158A136ADE735' +
    '30ACCA4F483A797ABC0AB182B324FB61D108A94BB2C8E3FB' +
    'B96ADAB760D7F4681D4F42A3DE394DF4AE56EDE76372BB19' +
    '0B07A7C8EE0A6D709E02FCE1CDF7E2ECC03404CD28342F61' +
    '9172FE9CE98583FF8E4F1232EEF28183C3FE3B1B4C6FAD73' +
    '3BB5FCBC2EC22005C58EF1837D1683B2C6F34A26C1B2EFFA' +
    '886B4238611FCFDCDE355B3B6519035BBC34F4DEF99C0238' +
    '61B46FC9D6E6C9077AD91D2691F7F7EE598CB0FAC186D91C' +
    'AEFE130985139270B4130C93BC437944F4FD4452E2D74DD3' +
    '64F2E21E71F54BFF5CAE82AB9C9DF69EE86D2BC522363A0D' +
    'ABC521979B0DEADA1DBF9A42D5C4484E0ABCD06BFA53DDEF' +
    '3C1B20EE3FD59D7C25E41D2B66C62E37FFFFFFFFFFFFFFFF';
  {* RFC 7919 ���Ƽ��� Diffie-Hellman ʹ�õ� 3072 λ����}

  CN_PRIME_FFDHE_4096 =
    'FFFFFFFFFFFFFFFFADF85458A2BB4A9AAFDC5620273D3CF1' +
    'D8B9C583CE2D3695A9E13641146433FBCC939DCE249B3EF9' +
    '7D2FE363630C75D8F681B202AEC4617AD3DF1ED5D5FD6561' +
    '2433F51F5F066ED0856365553DED1AF3B557135E7F57C935' +
    '984F0C70E0E68B77E2A689DAF3EFE8721DF158A136ADE735' +
    '30ACCA4F483A797ABC0AB182B324FB61D108A94BB2C8E3FB' +
    'B96ADAB760D7F4681D4F42A3DE394DF4AE56EDE76372BB19' +
    '0B07A7C8EE0A6D709E02FCE1CDF7E2ECC03404CD28342F61' +
    '9172FE9CE98583FF8E4F1232EEF28183C3FE3B1B4C6FAD73' +
    '3BB5FCBC2EC22005C58EF1837D1683B2C6F34A26C1B2EFFA' +
    '886B4238611FCFDCDE355B3B6519035BBC34F4DEF99C0238' +
    '61B46FC9D6E6C9077AD91D2691F7F7EE598CB0FAC186D91C' +
    'AEFE130985139270B4130C93BC437944F4FD4452E2D74DD3' +
    '64F2E21E71F54BFF5CAE82AB9C9DF69EE86D2BC522363A0D' +
    'ABC521979B0DEADA1DBF9A42D5C4484E0ABCD06BFA53DDEF' +
    '3C1B20EE3FD59D7C25E41D2B669E1EF16E6F52C3164DF4FB' +
    '7930E9E4E58857B6AC7D5F42D69F6D187763CF1D55034004' +
    '87F55BA57E31CC7A7135C886EFB4318AED6A1E012D9E6832' +
    'A907600A918130C46DC778F971AD0038092999A333CB8B7A' +
    '1A1DB93D7140003C2A4ECEA9F98D0ACC0A8291CDCEC97DCF' +
    '8EC9B55A7F88A46B4DB5A851F44182E1C68A007E5E655F6A' +
    'FFFFFFFFFFFFFFFF';
  {* RFC 7919 ���Ƽ��� Diffie-Hellman ʹ�õ� 4096 λ����}

  CN_PRIME_FFDHE_6144 =
    'FFFFFFFFFFFFFFFFADF85458A2BB4A9AAFDC5620273D3CF1' +
    'D8B9C583CE2D3695A9E13641146433FBCC939DCE249B3EF9' +
    '7D2FE363630C75D8F681B202AEC4617AD3DF1ED5D5FD6561' +
    '2433F51F5F066ED0856365553DED1AF3B557135E7F57C935' +
    '984F0C70E0E68B77E2A689DAF3EFE8721DF158A136ADE735' +
    '30ACCA4F483A797ABC0AB182B324FB61D108A94BB2C8E3FB' +
    'B96ADAB760D7F4681D4F42A3DE394DF4AE56EDE76372BB19' +
    '0B07A7C8EE0A6D709E02FCE1CDF7E2ECC03404CD28342F61' +
    '9172FE9CE98583FF8E4F1232EEF28183C3FE3B1B4C6FAD73' +
    '3BB5FCBC2EC22005C58EF1837D1683B2C6F34A26C1B2EFFA' +
    '886B4238611FCFDCDE355B3B6519035BBC34F4DEF99C0238' +
    '61B46FC9D6E6C9077AD91D2691F7F7EE598CB0FAC186D91C' +
    'AEFE130985139270B4130C93BC437944F4FD4452E2D74DD3' +
    '64F2E21E71F54BFF5CAE82AB9C9DF69EE86D2BC522363A0D' +
    'ABC521979B0DEADA1DBF9A42D5C4484E0ABCD06BFA53DDEF' +
    '3C1B20EE3FD59D7C25E41D2B669E1EF16E6F52C3164DF4FB' +
    '7930E9E4E58857B6AC7D5F42D69F6D187763CF1D55034004' +
    '87F55BA57E31CC7A7135C886EFB4318AED6A1E012D9E6832' +
    'A907600A918130C46DC778F971AD0038092999A333CB8B7A' +
    '1A1DB93D7140003C2A4ECEA9F98D0ACC0A8291CDCEC97DCF' +
    '8EC9B55A7F88A46B4DB5A851F44182E1C68A007E5E0DD902' +
    '0BFD64B645036C7A4E677D2C38532A3A23BA4442CAF53EA6' +
    '3BB454329B7624C8917BDD64B1C0FD4CB38E8C334C701C3A' +
    'CDAD0657FCCFEC719B1F5C3E4E46041F388147FB4CFDB477' +
    'A52471F7A9A96910B855322EDB6340D8A00EF092350511E3' +
    '0ABEC1FFF9E3A26E7FB29F8C183023C3587E38DA0077D9B4' +
    '763E4E4B94B2BBC194C6651E77CAF992EEAAC0232A281BF6' +
    'B3A739C1226116820AE8DB5847A67CBEF9C9091B462D538C' +
    'D72B03746AE77F5E62292C311562A846505DC82DB854338A' +
    'E49F5235C95B91178CCF2DD5CACEF403EC9D1810C6272B04' +
    '5B3B71F9DC6B80D63FDD4A8E9ADB1E6962A69526D43161C1' +
    'A41D570D7938DAD4A40E329CD0E40E65FFFFFFFFFFFFFFFF';
  {* RFC 7919 ���Ƽ��� Diffie-Hellman ʹ�õ� 6144 λ����}

  CN_PRIME_FFDHE_8192 =
    'FFFFFFFFFFFFFFFFADF85458A2BB4A9AAFDC5620273D3CF1' +
    'D8B9C583CE2D3695A9E13641146433FBCC939DCE249B3EF9' +
    '7D2FE363630C75D8F681B202AEC4617AD3DF1ED5D5FD6561' +
    '2433F51F5F066ED0856365553DED1AF3B557135E7F57C935' +
    '984F0C70E0E68B77E2A689DAF3EFE8721DF158A136ADE735' +
    '30ACCA4F483A797ABC0AB182B324FB61D108A94BB2C8E3FB' +
    'B96ADAB760D7F4681D4F42A3DE394DF4AE56EDE76372BB19' +
    '0B07A7C8EE0A6D709E02FCE1CDF7E2ECC03404CD28342F61' +
    '9172FE9CE98583FF8E4F1232EEF28183C3FE3B1B4C6FAD73' +
    '3BB5FCBC2EC22005C58EF1837D1683B2C6F34A26C1B2EFFA' +
    '886B4238611FCFDCDE355B3B6519035BBC34F4DEF99C0238' +
    '61B46FC9D6E6C9077AD91D2691F7F7EE598CB0FAC186D91C' +
    'AEFE130985139270B4130C93BC437944F4FD4452E2D74DD3' +
    '64F2E21E71F54BFF5CAE82AB9C9DF69EE86D2BC522363A0D' +
    'ABC521979B0DEADA1DBF9A42D5C4484E0ABCD06BFA53DDEF' +
    '3C1B20EE3FD59D7C25E41D2B669E1EF16E6F52C3164DF4FB' +
    '7930E9E4E58857B6AC7D5F42D69F6D187763CF1D55034004' +
    '87F55BA57E31CC7A7135C886EFB4318AED6A1E012D9E6832' +
    'A907600A918130C46DC778F971AD0038092999A333CB8B7A' +
    '1A1DB93D7140003C2A4ECEA9F98D0ACC0A8291CDCEC97DCF' +
    '8EC9B55A7F88A46B4DB5A851F44182E1C68A007E5E0DD902' +
    '0BFD64B645036C7A4E677D2C38532A3A23BA4442CAF53EA6' +
    '3BB454329B7624C8917BDD64B1C0FD4CB38E8C334C701C3A' +
    'CDAD0657FCCFEC719B1F5C3E4E46041F388147FB4CFDB477' +
    'A52471F7A9A96910B855322EDB6340D8A00EF092350511E3' +
    '0ABEC1FFF9E3A26E7FB29F8C183023C3587E38DA0077D9B4' +
    '763E4E4B94B2BBC194C6651E77CAF992EEAAC0232A281BF6' +
    'B3A739C1226116820AE8DB5847A67CBEF9C9091B462D538C' +
    'D72B03746AE77F5E62292C311562A846505DC82DB854338A' +
    'E49F5235C95B91178CCF2DD5CACEF403EC9D1810C6272B04' +
    '5B3B71F9DC6B80D63FDD4A8E9ADB1E6962A69526D43161C1' +
    'A41D570D7938DAD4A40E329CCFF46AAA36AD004CF600C838' +
    '1E425A31D951AE64FDB23FCEC9509D43687FEB69EDD1CC5E' +
    '0B8CC3BDF64B10EF86B63142A3AB8829555B2F747C932665' +
    'CB2C0F1CC01BD70229388839D2AF05E454504AC78B758282' +
    '2846C0BA35C35F5C59160CC046FD8251541FC68C9C86B022' +
    'BB7099876A460E7451A8A93109703FEE1C217E6C3826E52C' +
    '51AA691E0E423CFC99E9E31650C1217B624816CDAD9A95F9' +
    'D5B8019488D9C0A0A1FE3075A577E23183F81D4A3F2FA457' +
    '1EFC8CE0BA8A4FE8B6855DFE72B0A66EDED2FBABFBE58A30' +
    'FAFABE1C5D71A87E2F741EF8C1FE86FEA6BBFDE530677F0D' +
    '97D11D49F7A8443D0822E506A9F4614E011E2A94838FF88C' +
    'D68C8BB7C5C6424CFFFFFFFFFFFFFFFF';
  {* RFC 7919 ���Ƽ��� Diffie-Hellman ʹ�õ� 8192 λ����}

type
  TCnFeldmanVssPiece = class
  {* Feldman VSS ��Ƭʵ����}
  private
    FOrder: TCnBigNumber;
    FShare: TCnBigNumber;
    FCommitments: TCnBigNumberList;
    function GetCommitmenet(Index: Integer): TCnBigNumber;
    function GetCommitmentCount: Integer;
  public
    constructor Create; virtual;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}

    property Order: TCnBigNumber read FOrder;
    {* �Ա��� X}
    property Share: TCnBigNumber read FShare;
    {* ��Ӧ�����ܷ�Ƭ}
    property CommitmentCount: Integer read GetCommitmentCount;
    {* ��������ֵ֤�б�����}
    property Commitments[Index: Integer]: TCnBigNumber read GetCommitmenet;
    {* ��������ֵ֤�б�}
  end;

// ====================== Shamir ���޷���ʵ�����ܹ��� ==========================

function CnInt64ShamirSplit(Secret: Int64; ShareCount: Integer; Threshold: Integer;
  OutShares: TCnInt64List; var Prime: Int64): Boolean;
{* �� Shamir ���޷���ʵ�� Int64 �����ܹ�����һ Int64 ֵ���Ϊ ShareCount �� Int64 ֵ
   ֻ��Ҫ���� Threshold ��ֵ����˳����ܻ�ԭ Secret�������Ƿ��ֳɹ���
   ���ֵ�� OutShares �У���Ӧ˳��ֵΪ���±� + 1����� 0 ���Ӧ 1����
   ������������� Prime ��ָ������Ϊ 0�������ɷ���Ҫ�������ֵ���ء�

   ������
     Secret: Int64                        - ����ֵ���������
     ShareCount: Integer                  - ��ֵķ�Ƭ����
     Threshold: Integer                   - ��ԭ�ķ�Ƭ����
     OutShares: TCnInt64List              - ����Ĳ��ֵ�б�
     var Prime: Int64                     - ��ָ��������������������С���ڲ�����������

   ����ֵ��Boolean                        - ���ز���Ƿ�ɹ�
}

function CnInt64ShamirReconstruct(Prime: Int64; InOrders: TCnInt64List;
  InShares: TCnInt64List; out Secret: Int64): Boolean;
{* �� Shamir ���޷���ʵ�� Int64 �����ܹ����� Threshold ����ֺ�� Int64 ֵ�����Ӧ��Ų����
   ����������� Secret�������Ƿ�����ɹ����ɹ�������ֵ�� Secret �з��ء�


   ������
     Prime: Int64                         - ���ʱʹ�õ�����
     InOrders: TCnInt64List               - ��Ƭ����б�
     InShares: TCnInt64List               - ��Ƭ��Ӧ�Ĳ��ֵ�б�
     out Secret: Int64                    - ����������������

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function CnShamirSplit(Secret: TCnBigNumber; ShareCount: Integer; Threshold: Integer;
  OutOrders: TCnBigNumberList; OutShares: TCnBigNumberList; Prime: TCnBigNumber): Boolean;
{* �� Shamir ���޷���ʵ�ִ�����Χ�ڵ����ܹ�����һ���� Secret ���Ϊ ShareCount ������ֵ
   ֻ��Ҫ���� Threshold ��ֵ����˳����ܻ�ԭ Secret�������Ƿ��ֳɹ���
   ���˳��� InOrders �У��������� 1 2 3 4 �����������ֵ�� OutShares ��
   �� Prime ֵ��Ϊ 0 �Ҵ��� Secret �Ļ��������������б�֤ Prime Ϊ������

   ������
     Secret: TCnBigNumber                 - ����ֵ���������
     ShareCount: Integer                  - ��ֵķ�Ƭ����
     Threshold: Integer                   - ��ԭ�ķ�Ƭ����
     OutOrders: TCnBigNumberList          - ����ķ�Ƭ����б�
     OutShares: TCnBigNumberList          - ����Ĳ��ֵ�б�
     Prime: TCnBigNumber                  - ��ָ��������0 ���ڲ�����

   ����ֵ��Boolean                        - ���ز���Ƿ�ɹ�
}

function CnShamirReconstruct(Prime: TCnBigNumber; InOrders: TCnBigNumberList;
  InShares: TCnBigNumberList; OutSecret: TCnBigNumber): Boolean;
{* �� Shamir ���޷���ʵ�ִ�����Χ�ڵ����ܹ����� Threshold ����ֺ�Ĵ���ֵ�����Ӧ��Ų����
   ����������� Secret�������Ƿ�����ɹ����ɹ�������ֵ�� Secret �з��ء�

   ������
     Prime: TCnBigNumber                  - ���ʱʹ�õ�����
     InOrders: TCnBigNumberList           - ��Ƭ����б�
     InShares: TCnBigNumberList           - ��Ƭ��Ŷ�Ӧ�Ĳ��ֵ�б�
     OutSecret: TCnBigNumber              - ����������������

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

// =============== Feldman's VSS ��չ Shamir ���޷���ʵ�����ܹ��� ==============

function CnInt64FeldmanVssGeneratePrime(out Prime: Int64; out Generator: Int64): Boolean;
{* ���� Feldman VSS ���������������Ԫ�������Ƿ����ɳɹ�����ע�ⲻͬ�� DH ��Ҫ��
   �����������󣩼�һ��һ��Ҳ��������С��������Ԫ��С������ģ������ֵΪ 1��

   ������
     out Prime: Int64                     - ���ɵ�����
     out Generator: Int64                 - ���ɵ�����Ԫ

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function CnInt64FeldmanVssSplit(Secret: Int64; ShareCount: Integer; Threshold: Integer;
  OutShares: TCnInt64List; OutCommitments: TCnInt64List; var Prime: Int64; var Generator: Int64): Boolean;
{* Feldman ��չ�� Shamir ���޷���ʵ�� Int64 �����ܹ���
   ��һ Int64 ֵ���Ϊ ShareCount �� Int64 ֵ����������ֵ֤��Commitment����
   ֻ��Ҫ���� Threshold ��ֵ����˳����ܻ�ԭ Secret�������Ƿ��ֳɹ���
   ���ֵ�� OutShares �У���Ӧ˳��ֵΪ���±� + 1����� 0 ���Ӧ 1����
   ��ֵ� Share ֵ��ͨ�� CnInt64FeldmanVssVerify �ù�������ֵ֤����֤���Ƿ�Ϸ���
   ������������� Prime ��ָ������Ϊ 0�������ɷ���Ҫ�������ֵ���ء�
   ע���ڲ���װ�� Shamir ʹ�õ������� (Prime - 1) / 2������ Prime��

   ������
     Secret: Int64                        - ����ֵ���������
     ShareCount: Integer                  - ��ֵķ�Ƭ����
     Threshold: Integer                   - ��ԭ�ķ�Ƭ����
     OutShares: TCnInt64List              - ����Ĳ��ֵ�б�
     OutCommitments: TCnInt64List         - �������ֵ֤�б�
     var Prime: Int64                     - ��ָ��������0 ���ڲ�����
     var Generator: Int64                 - ��ָ������Ԫ��0 ���ڲ�����

   ����ֵ��Boolean                        - ���ز���Ƿ�ɹ�
}

function CnInt64FeldmanVssVerify(Prime: Int64; Generator: Int64; InOrder: Int64;
  InShare: Int64; Commitments: TCnInt64List): Boolean;
{* Feldman ��չ�� Shamir ���޷���ʵ�� Int64 �����ܹ���������֤�Ƿ�ͨ����
   �ù�������ֵ֤����֤ĳ��ŵ����ܷ�Ƭ�Ƿ�Ϸ���

   ������
     Prime: Int64                         - ���ʱʹ�õ�����
     Generator: Int64                     - ���ʱʹ�õ�����Ԫ
     InOrder: Int64                       - ����֤�ķ�Ƭ���
     InShare: Int64                       - ����֤�ķ�Ƭ��Ŷ�Ӧ�Ĳ��ֵ
     Commitments: TCnInt64List            - ����֤����ֵ֤�б�

   ����ֵ��Boolean                        - ������֤�Ƿ�ɹ�
}

function CnInt64FeldmanVssReconstruct(Prime: Int64; Generator: Int64; InOrders: TCnInt64List;
  InShares: TCnInt64List; Commitments: TCnInt64List; out Secret: Int64; Verify: Boolean = True): Boolean;
{* Feldman ��չ�� Shamir ���޷���ʵ�� Int64 �����ܹ���Verify ��ʾ����ǰ�Ƿ������֤��
   �� Threshold ����ֺ�� Int64 ֵ�����Ӧ��Ų���ϴ���������� Secret��
   �����Ƿ�����ɹ����ɹ�������ֵ�� Secret �з��ء�

   ������
     Prime: Int64                         - ���ʱʹ�õ�����
     Generator: Int64                     - ���ʱʹ�õ�����Ԫ
     InOrders: TCnInt64List               - ������ķ�Ƭ����б�
     InShares: TCnInt64List               - ������ķ�Ƭ��Ŷ�Ӧ�Ĳ��ֵ�б�
     Commitments: TCnInt64List            - ���������ֵ֤�б�
     out Secret: Int64                    - ����������������
     Verify: Boolean                      - ����ǰ�Ƿ������֤

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function CnFeldmanVssSplit(Secret: TCnBigNumber; ShareCount: Integer; Threshold: Integer;
  OutOrders: TCnBigNumberList; OutShares: TCnBigNumberList; OutCommitments: TCnBigNumberList;
  Prime: TCnBigNumber; Generator: TCnBigNumber): Boolean;
{* Feldman ��չ�� Shamir ���޷���ʵ�ִ�����Χ�ڵ����ܹ���
   �� Threshold ����ֺ�Ĵ���ֵ�����Ӧ��Ų���ϣ���������ֵ֤��Commitment�������ز���Ƿ�ɹ���
   ��ֳ���ֵ��ͨ�� CnFeldmanVssVerify �ù�������ֵ֤����֤���Ƿ�Ϸ���
   ����������������ѣ�������紫�룬��ʹ�� CN_PRIME_FFDHE_* ϵ�з��� 2q + 1 ��������
   ����Ԫ��Ϊ 2��ע��������� Secret �����ڲ���װ�� Shamir ʹ�õ������� (Prime - 1) / 2������ Prime
   ��ֺ�Orders �� Shares һһ��Ӧ���Է�������ܳ��з���Commitments �� Prime��Generator ��ȫ��������

   ������
     Secret: TCnBigNumber                 - ����ֵ���������
     ShareCount: Integer                  - ��ֵķ�Ƭ����
     Threshold: Integer                   - ��ԭ�ķ�Ƭ����
     OutOrders: TCnBigNumberList          - ����ķ�Ƭ����б�
     OutShares: TCnBigNumberList          - ����Ĳ��ֵ�б�
     OutCommitments: TCnBigNumberList     - �������ֵ֤
     Prime: TCnBigNumber                  - ��ָ��������0 ���ڲ�����
     Generator: TCnBigNumber              - ��ָ������Ԫ��0 ���ڲ�����

   ����ֵ��Boolean                        - ���ز���Ƿ�ɹ�
}

function CnFeldmanVssVerify(Prime: TCnBigNumber; Generator: TCnBigNumber; InOrder: TCnBigNumber;
  InShare: TCnBigNumber; Commitments: TCnBigNumberList): Boolean;
{* Feldman ��չ�� Shamir ���޷���ʵ�ִ�����Χ�ڵ����ܹ����Ƭ������֤��������֤�Ƿ�ͨ����
   �ù�������ֵ֤�Լ� Prime��Generator ����֤ĳ��ŵ����ܷ�Ƭ�Ƿ�Ϸ���

   ������
     Prime: TCnBigNumber                  - ���ʱʹ�õ�����
     Generator: TCnBigNumber              - ���ʱʹ�õ�����Ԫ
     InOrder: TCnBigNumber                - ����֤�ķ�Ƭ���
     InShare: TCnBigNumber                - ����֤�ķ�Ƭ��Ŷ�Ӧ�Ĳ��ֵ
     Commitments: TCnBigNumberList        - ����֤����ֵ֤�б�

   ����ֵ��Boolean                        - ������֤�Ƿ�ɹ�
}

function CnFeldmanVssReconstruct(Prime: TCnBigNumber; Generator: TCnBigNumber;
  InOrders: TCnBigNumberList; InShares: TCnBigNumberList; Commitments: TCnBigNumberList;
  OutSecret: TCnBigNumber; Verify: Boolean = True): Boolean;
{* Feldman ��չ�� Shamir ���޷���ʵ�ִ�����Χ�ڵ����ܹ���Verify ��ʾ����ǰ�Ƿ���֤��
   �� Threshold ����ֺ�Ĵ���ֵ�����Ӧ��Ų���ϴ���������� Secret�������Ƿ�����ɹ���
   �ɹ�������ֵ�� Secret �з��ء�

   ������
     Prime: TCnBigNumber                  - ���ʱʹ�õ�����
     Generator: TCnBigNumber              - ���ʱʹ�õ�����Ԫ
     InOrders: TCnBigNumberList           - ������ķ�Ƭ����б�
     InShares: TCnBigNumberList           - ������ķ�Ƭ��Ŷ�Ӧ�Ĳ��ֵ�б�
     Commitments: TCnBigNumberList        - ���������ֵ֤�б�
     OutSecret: TCnBigNumber              - ����������������
     Verify: Boolean                      - ����ǰ�Ƿ������֤

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

implementation

function CnInt64ShamirSplit(Secret: Int64; ShareCount, Threshold: Integer;
  OutShares: TCnInt64List; var Prime: Int64): Boolean;
var
  Poly: TCnInt64Polynomial;
  I: Integer;
begin
  Result := False;

  if (Secret < 0) or (ShareCount < 3) or (Threshold < 2) or (ShareCount < Threshold) then
  begin
    _CnSetLastError(ECN_SECRET_INVALID_INPUT);
    Exit;
  end;

  if (Prime <= 0) or (Prime <= Secret) or not CnInt64IsPrime(Prime) then // ������������С������������
  begin
    if Secret < CN_PRIME_NUMBERS_SQRT_UINT32[High(CN_PRIME_NUMBERS_SQRT_UINT32)] then
      Prime := CN_PRIME_NUMBERS_SQRT_UINT32[High(CN_PRIME_NUMBERS_SQRT_UINT32)]
    else
    begin
      // TODO: Ѱ��һ���� Secret �������
    end;
  end;

  Poly := nil;
  try
    Poly := TCnInt64Polynomial.Create;

    Poly.MaxDegree := Threshold - 1;
    Poly[0] := Secret;
    for I := 1 to Poly.MaxDegree do
    begin
      Poly[I] := RandomInt64LessThan(Prime);
      if Poly[I] = 0 then  // ������� 0 ϵ����ɸ���Ӱ��
        Poly[I] := 1;
    end;

    // �������������ʽ
    OutShares.Clear;
    for I := 1 to ShareCount do
      OutShares.Add(Int64PolynomialGaloisGetValue(Poly, I, Prime));

    Result := True;
    _CnSetLastError(ECN_SECRET_OK);
  finally
    Poly.Free;
  end;
end;

function CnInt64ShamirReconstruct(Prime: Int64; InOrders, InShares: TCnInt64List;
  out Secret: Int64): Boolean;
var
  I, J: Integer;
  X, Y, N, D: Int64;
begin
  Result := False;
  if (Prime < 2) or (InOrders.Count < 2) or (InOrders.Count <> InShares.Count) then
  begin
    _CnSetLastError(ECN_SECRET_INVALID_INPUT);
    Exit;
  end;

  // �������ղ�ֵ��ʽ��InOrder ��һ�� X ���꣬InShares ��һ�� Y ����
  // �� T �� Shares����Ҫ�ۼ� T �ÿ�����һ�� X Y ���꣬�����¼��㷽����
  // ÿ�� = ��Y * (-�������� X �Ļ�) / (��X - ��������X) �Ļ�)

  Secret := 0;
  for I := 0 to InOrders.Count - 1 do
  begin
    X := InOrders[I];
    Y := InShares[I];

    //  ���˵ķŵ����� N �У������ķŵ���ĸ D ������ģ��Ԫ
    N := Y;
    D := 1;
    for J := 0 to InOrders.Count - 1 do
    begin
      if J <> I then
      begin
        N := Int64NonNegativeMulMod(N, InOrders[J], Prime);
        D := Int64NonNegativeMulMod(D, Int64AddMod(X, -InOrders[J], Prime), Prime);
      end;
    end;
    D := CnInt64ModularInverse2(D, Prime);

    N := Int64NonNegativeMulMod(N, D, Prime);
    Secret := Int64AddMod(Secret, N, Prime);
  end;

  Result := True;
  _CnSetLastError(ECN_SECRET_OK);
end;

function CnShamirSplit(Secret: TCnBigNumber; ShareCount, Threshold: Integer;
  OutOrders, OutShares: TCnBigNumberList; Prime: TCnBigNumber): Boolean;
var
  Poly: TCnBigNumberPolynomial;
  T: TCnBigNumber;
  I, Bits: Integer;
begin
  Result := False;

  if (Secret.IsNegative) or (ShareCount < 3) or (Threshold < 2) or (ShareCount < Threshold) then
  begin
    _CnSetLastError(ECN_SECRET_INVALID_INPUT);
    Exit;
  end;

  if Prime.IsZero or Prime.IsNegative or (BigNumberCompare(Prime, Secret) <= 0) then // ���������С������������
  begin
    // Ѱ��һ���� Secret ����������� Bits Ϊý��
    Bits := Secret.GetBitsCount + 1;
    if Bits < CN_SHAMIR_DEFAULT_PRIME_BITS then
      Bits := CN_SHAMIR_DEFAULT_PRIME_BITS;

    if not BigNumberGeneratePrimeByBitsCount(Prime, Bits) then
    begin
      _CnSetLastError(ECN_SECRET_PRIME_ERROR);
      Exit;
    end;
  end;

  Poly := nil;
  T := nil;

  try
    Poly := TCnBigNumberPolynomial.Create;

    Poly.MaxDegree := Threshold - 1;
    if BigNumberCopy(Poly[0], Secret) = nil then
      Exit;

    for I := 1 to Poly.MaxDegree do
    begin
      if not BigNumberRandRange(Poly[I], Prime) then
      begin
        _CnSetLastError(ECN_SECRET_RANDOM_ERROR);
        Exit;
      end;

      if Poly[I].IsZero then // ����ϵ������ 0
        Poly[I].SetOne;
    end;

    // �������������ʽ���� 1 �� ShareCount ����ֱ���ֵ
    OutOrders.Clear;
    OutShares.Clear;

    T := TCnBigNumber.Create;
    for I := 1 to ShareCount do
    begin
      OutOrders.Add.SetWord(I);
      T.SetWord(I);
      if not BigNumberPolynomialGaloisGetValue(OutShares.Add, Poly, T, Prime) then
        Exit;
    end;

    Result := True;
    _CnSetLastError(ECN_SECRET_OK);
  finally
    T.Free;
    Poly.Free;
  end;
end;

function CnShamirReconstruct(Prime: TCnBigNumber; InOrders, InShares: TCnBigNumberList;
  OutSecret: TCnBigNumber): Boolean;
var
  I, J: Integer;
  X, Y, T, N, D: TCnBigNumber;
begin
  Result := False;
  if Prime.IsNegative or Prime.IsZero or (InOrders.Count < 2) or (InOrders.Count <> InShares.Count) then
  begin
    _CnSetLastError(ECN_SECRET_INVALID_INPUT);
    Exit;
  end;

  // �������ղ�ֵ��ʽ��InOrder ��һ�� X ���꣬InShares ��һ�� Y ����
  // �� T �� Shares����Ҫ�ۼ� T �ÿ�����һ�� X Y ���꣬�����¼��㷽����
  // ÿ�� = ��Y * (-�������� X �Ļ�) / (��X - ��������X) �Ļ�)

  N := nil;
  D := nil;
  T := nil;

  try
    OutSecret.SetZero;

    T := TCnBigNumber.Create;
    N := TCnBigNumber.Create;
    D := TCnBigNumber.Create;

    for I := 0 to InOrders.Count - 1 do
    begin
      X := InOrders[I];
      Y := InShares[I];

      //  ���˵ķŵ����� N �У������ķŵ���ĸ D ������ģ��Ԫ
      if BigNumberCopy(N, Y) = nil then
        Exit;
      D.SetOne;

      for J := 0 to InOrders.Count - 1 do
      begin
        if J <> I then
        begin
          if not BigNumberDirectMulMod(N, N, InOrders[J], Prime) then
            Exit;

          if not BigNumberSubMod(T, X, InOrders[J], Prime) then
            Exit;

          if not BigNumberDirectMulMod(D, D, T, Prime) then
            Exit;
        end;
      end;

      if not BigNumberModularInverse(T, D, Prime) then
        Exit;

      if not BigNumberDirectMulMod(N, T, N, Prime) then
        Exit;

      if not BigNumberAddMod(OutSecret, OutSecret, N, Prime) then
        Exit;
    end;

    Result := True;
    _CnSetLastError(ECN_SECRET_OK);
  finally
    T.Free;
    D.Free;
    N.Free;
  end;
end;

function CnInt64FeldmanVssGeneratePrime(out Prime, Generator: Int64): Boolean;
var
  I: Integer;
  Q: TUInt64;
begin
  repeat
    Prime := CnGenerateInt64Prime2;
    Q := (Prime - 1) shr 1;

    if not CnInt64IsPrime(Q) then
      Continue;

    for I := 2 to MaxInt do
    begin
      if MontgomeryPowerMod(I, Q, Prime) = 1 then
      begin
        Generator := I;
        Result := True;
        Exit;
      end;
    end;
  until False;
end;

function CnInt64FeldmanVssSplit(Secret: Int64; ShareCount, Threshold: Integer;
  OutShares, OutCommitments: TCnInt64List; var Prime, Generator: Int64): Boolean;
var
  Poly: TCnInt64Polynomial;
  Q: Int64;
  I: Integer;
begin
  Result := False;
  if (Secret < 0) or (ShareCount < 3) or (Threshold < 2) or (ShareCount < Threshold) then
  begin
    _CnSetLastError(ECN_SECRET_INVALID_INPUT);
    Exit;
  end;

  if (Prime = 0) or (Generator = 0) then // 0 ��ʾҪ��������
  begin
    if not CnInt64FeldmanVssGeneratePrime(Prime, Generator) then
      Exit;

    if Secret > Prime then
    begin
      _CnSetLastError(ECN_SECRET_INVALID_INPUT);
      Exit;
    end;
  end;
  Q := (Prime - 1) shr 1;

  // ��ʼ�հ� Shamir ���޷�����֣�������� Q���� Commits �� Prime
  Poly := nil;
  try
    Poly := TCnInt64Polynomial.Create;

    Poly.MaxDegree := Threshold - 1;
    Poly[0] := Secret;
    for I := 1 to Poly.MaxDegree do
    begin
      Poly[I] := RandomInt64LessThan(Q);
      if Poly[I] = 0 then  // ������� 0 ϵ����ɸ���Ӱ��
        Poly[I] := 1;
    end;

    // �������������ʽ
    OutShares.Clear;
    for I := 1 to ShareCount do
      OutShares.Add(Int64PolynomialGaloisGetValue(Poly, I, Q));

    // ����ϵ������ֵ֤
    OutCommitments.Clear;
    for I := 1 to Threshold do
      OutCommitments.Add(MontgomeryPowerMod(Generator, Poly[I - 1], Prime));

    Result := True;
    _CnSetLastError(ECN_SECRET_OK);
  finally
    Poly.Free;
  end;
end;

function CnInt64FeldmanVssVerify(Prime, Generator: Int64; InOrder, InShare: Int64;
  Commitments: TCnInt64List): Boolean;
var
  I: Integer;
  L, R, T: Int64;
begin
  // ��֤ĳ��Ƭ�Ƿ���ȷ���ȼ��� Generator^InShare mod Prime
  L := MontgomeryPowerMod(Generator, InShare, Prime);

  // �ټ������Ļ���ÿ������ I���� Commitments[I]^(InOrder^I) mod Prime
  R := 1;
  for I := 0 to Commitments.Count - 1 do
  begin
    T := PowerPowerMod(Commitments[I], InOrder, I, Prime);
    R := MultipleMod(R, T, Prime);
  end;

  Result := L = R;
  _CnSetLastError(ECN_SECRET_OK);
end;

function CnInt64FeldmanVssReconstruct(Prime, Generator: Int64; InOrders, InShares,
  Commitments: TCnInt64List; out Secret: Int64; Verify: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Prime < 2) or (Generator < 2) or (InOrders.Count <> InShares.Count)
    or (InOrders.Count < 2) or (Commitments.Count <= 1) then
  begin
    _CnSetLastError(ECN_SECRET_INVALID_INPUT);
    Exit;
  end;

  if Verify then
  begin
    for I := 0 to InOrders.Count - 1 do
    begin
      if not CnInt64FeldmanVssVerify(Prime, Generator, InOrders[I],
        InShares[I], Commitments) then
      begin
        _CnSetLastError(ECN_SECRET_FELDMAN_CHECKERROR);
        Exit;
      end;
    end;
  end;

  Result := CnInt64ShamirReconstruct((Prime - 1) shr 1, InOrders, InShares, Secret);
end;

function CnFeldmanVssSplit(Secret: TCnBigNumber; ShareCount, Threshold: Integer;
  OutOrders, OutShares, OutCommitments: TCnBigNumberList; Prime, Generator: TCnBigNumber): Boolean;
var
  Poly: TCnBigNumberPolynomial;
  T, Q: TCnBigNumber;
  I: Integer;
begin
  Result := False;

  if (Secret.IsNegative) or (ShareCount < 3) or (Threshold < 2) or (ShareCount < Threshold) then
  begin
    _CnSetLastError(ECN_SECRET_INVALID_INPUT);
    Exit;
  end;

  if Prime.IsZero or Prime.IsNegative or Generator.IsZero or Generator.IsNegative or
    (BigNumberCompare(Prime, Secret) <= 0) then // �������������Ԫ���ԣ�������˳�
  begin
    _CnSetLastError(ECN_SECRET_PRIME_ERROR);
    Exit;
  end;

  Q := nil;
  Poly := nil;
  T := nil;

  try
    Q := TCnBigNumber.Create;
    if BigNumberCopy(Q, Prime) = nil then
      Exit;
    Q.SubWord(1);
    Q.ShiftRightOne;

    Poly := TCnBigNumberPolynomial.Create;
    Poly.MaxDegree := Threshold - 1;
    if BigNumberCopy(Poly[0], Secret) = nil then
      Exit;

    for I := 1 to Poly.MaxDegree do
    begin
      if not BigNumberRandRange(Poly[I], Q) then
      begin
        _CnSetLastError(ECN_SECRET_RANDOM_ERROR);
        Exit;
      end;

      if Poly[I].IsZero then // ����ϵ������ 0
        Poly[I].SetOne;
    end;

    // �������������ʽ���� 1 �� ShareCount ����ֱ���ֵ
    OutOrders.Clear;
    OutShares.Clear;

    T := TCnBigNumber.Create;
    for I := 1 to ShareCount do
    begin
      OutOrders.Add.SetWord(I);
      T.SetWord(I);
      if not BigNumberPolynomialGaloisGetValue(OutShares.Add, Poly, T, Q) then
        Exit;
    end;

    // ����ϵ������ֵ֤
    OutCommitments.Clear;
    for I := 1 to Threshold do
    begin
      if not BigNumberPowerMod(OutCommitments.Add, Generator, Poly[I - 1], Prime) then
        Exit;
    end;

    Result := True;
    _CnSetLastError(ECN_SECRET_OK);
  finally
    T.Free;
    Poly.Free;
    Q.Free;
  end;
end;

function CnFeldmanVssVerify(Prime, Generator: TCnBigNumber; InOrder, InShare: TCnBigNumber;
  Commitments: TCnBigNumberList): Boolean;
var
  I: Integer;
  L, R, T, D: TCnBigNumber;
begin
  Result := False;

  // ��֤ĳ��Ƭ�Ƿ���ȷ���ȼ��� Generator^InShare mod Prime
  L := nil;
  R := nil;
  T := nil;
  D := nil;

  try
    L := TCnBigNumber.Create;
    if not BigNumberPowerMod(L, Generator, InShare, Prime) then
      Exit;

    // �ټ������Ļ���ÿ������ I���� Commitments[I]^(InOrder^I) mod Prime
    R := TCnBigNumber.Create;
    R.SetOne;

    T := TCnBigNumber.Create;
    D := TCnBigNumber.Create;

    for I := 0 to Commitments.Count - 1 do
    begin
      D.SetWord(I);
      if not BigNumberPowerPowerMod(T, Commitments[I], InOrder, D, Prime) then
        Exit;

      if not BigNumberDirectMulMod(R, R, T, Prime) then
        Exit;
    end;

    Result := BigNumberEqual(L, R);
    _CnSetLastError(ECN_SECRET_OK);
  finally
    D.Free;
    T.Free;
    R.Free;
    L.Free;
  end;
end;

function CnFeldmanVssReconstruct(Prime, Generator: TCnBigNumber; InOrders, InShares,
  Commitments: TCnBigNumberList; OutSecret: TCnBigNumber; Verify: Boolean = True): Boolean;
var
  I: Integer;
  Q: TCnBigNumber;
begin
  Result := False;
  if Prime.IsZero or Prime.IsNegative or Generator.IsZero or Generator.IsNegative
    or (InOrders.Count <> InShares.Count) or (InOrders.Count < 2) or (Commitments.Count <= 1) then
  begin
    _CnSetLastError(ECN_SECRET_INVALID_INPUT);
    Exit;
  end;

  if Verify then
  begin
    for I := 0 to InOrders.Count - 1 do
    begin
      if not CnFeldmanVssVerify(Prime, Generator, InOrders[I],
        InShares[I], Commitments) then
      begin
        _CnSetLastError(ECN_SECRET_FELDMAN_CHECKERROR);
        Exit;
      end;
    end;
  end;

  Q := TCnBigNumber.Create;
  try
    if BigNumberCopy(Q, Prime) = nil then
      Exit;
    Q.SubWord(1);
    Q.ShiftRightOne;

    Result := CnShamirReconstruct(Q, InOrders, InShares, OutSecret);
  finally
    Q.Free;
  end;
end;

{ TCnFeldmanVssPiece }

constructor TCnFeldmanVssPiece.Create;
begin
  inherited;
  FOrder := TCnBigNumber.Create;
  FShare := TCnBigNumber.Create;
  FCommitments := TCnBigNumberList.Create;
end;

destructor TCnFeldmanVssPiece.Destroy;
begin
  FCommitments.Free;
  FShare.Free;
  FOrder.Free;
  inherited;
end;

function TCnFeldmanVssPiece.GetCommitmenet(Index: Integer): TCnBigNumber;
begin
  Result := FCommitments[Index];
end;

function TCnFeldmanVssPiece.GetCommitmentCount: Integer;
begin
  Result := FCommitments.Count;
end;

end.
