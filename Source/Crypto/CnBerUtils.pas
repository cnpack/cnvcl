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

unit CnBerUtils;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�ASN.1 ��ʽ�� BER/DER �����ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע������Ԫʵ���� ASN.1 ��ʽ�� BER/DER ����룬�ṩ�� TCnBerReader ��ɽ�
*           ���ݽ���Ϊ��״���ݽṹ���ṩ�� TCnBerWriter ��ʵ��������װ�������
* ����ƽ̨��WinXP + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2023.12.13 V1.6
*               ���Ӷ� 00 00 ���̶����Ƚڵ�Ľ���֧�֣�����һ������
*           2022.04.26 V1.5
*               �޸� LongWord �� Integer ��ַת����֧�� MacOS64
*           2022.04.15 V1.4
*               ����һ AsCommonInteger ���������Զ��������ݳ��� 1��2��4 ��ȡ����ֵ��
*           2020.03.28 V1.3
*               �����ⲿ���ڵ����� TypeMask ��Ӧ�� ECC ��˽Կ���ڵ�������
*           2019.04.19 V1.2
*               ֧�� Win32/Win64/MacOS��֧�� VCL �� FMX �µ� TreeView ������
*           2018.05.27 V1.1
*               �� Parser ��Ϊ Reader ��ʵ�� Writer
*           2018.05.24 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

// ����Ҫ���ú� TreeView �����Ĺ��ܣ����ڹ���ѡ���ж��� ENABLE_UIINTERACT

// �� ENABLE_FMX ������ FMX �������Ƿ�֧�� FMX��Ĭ�ϲ�֧�֣��Ա����������Ķ������̫��
{$IFNDEF ENABLE_FMX}
  {$UNDEF SUPPORT_FMX}
{$ENDIF}

uses
  SysUtils, Classes, TypInfo, CnNative, CnBigNumber, CnTree
  {$IFDEF DEBUG} {$IFDEF ENABLE_UIINTERACT}
    {$IFDEF MSWINDOWS}, ComCtrls  {$ENDIF} // ��� Windows �±�������Ҳ����õ�Ԫ�����ڱ���ѡ����� Vcl ǰ׺
    {$IFDEF SUPPORT_FMX}, FMX.TreeView {$ENDIF}
  {$ENDIF} {$ENDIF};
  // If ComCtrls not found, please add 'Vcl' to 'Unit Scope Names' in Project Options.

const
  CN_BER_TAG_TYPE_MASK                      = $C0;
  {* Ber Tag ���������룬����һ�ֽ��ڵ������λ��00 Ϊ Universal��01 Ϊ Application��10 Ϊ Context-Specific��11 Ϊ Private}

  CN_BER_TAG_STRUCT_MASK                    = $20;
  {* Ber Tag �Ľṹ����}

  CN_BER_TAG_VALUE_MASK                     = $1F;
  {* Ber Tag ��ֵ����}

  CN_BER_LENLEN_MASK                        = $80;
  {* Ber Tag �ĳ��ȵĳ�������}

  CN_BER_LENGTH_MASK                        = $7F;
  {* Ber Tag �ĳ�������}

  CN_BER_TAG_RESERVED                       = $00;
  {* Ber Tag ���� 0������}

  CN_BER_TAG_BOOLEAN                        = $01;
  {* �������͵� Ber Tag}

  CN_BER_TAG_INTEGER                        = $02;
  {* �������͵� Ber Tag}

  CN_BER_TAG_BIT_STRING                     = $03;
  {* λ�����͵� Ber Tag}

  CN_BER_TAG_OCTET_STRING                   = $04;
  {* �ַ������͵� Ber Tag}

  CN_BER_TAG_NULL                           = $05;
  {* Null ���͵� Ber Tag}

  CN_BER_TAG_OBJECT_IDENTIFIER              = $06;
  {* �����ʶ�����͵� Ber Tag}

  CN_BER_TAG_OBJECT_DESCRIPION              = $07;
  {* �����������͵� Ber Tag}

  CN_BER_TAG_EXTERNAL                       = $08;
  {* �ⲿ���͵� Ber Tag}

  CN_BER_TAG_REAL                           = $09;
  {* ���������͵� Ber Tag}

  CN_BER_TAG_ENUMERATED                     = $0A;
  {* ö�����͵� Ber Tag}

  CN_BER_TAG_EMBEDDED_PDV                   = $0B;
  {* Ƕ�����͵� Ber Tag}

  CN_BER_TAG_UFT8STRING                     = $0C;
  {* UTF-8 �ַ������͵� Ber Tag}

  CN_BER_TAG_RELATIVE_OID                   = $0D;
  {* ���������������͵� Ber Tag}

  CN_BER_TAG_SEQUENCE                       = $10;
  {* �������͵� Ber Tag}

  CN_BER_TAG_SET                            = $11;
  {* �������͵� Ber Tag}

  CN_BER_TAG_NUMERICSTRING                  = $12;
  {* �����ַ������͵� Ber Tag}

  CN_BER_TAG_PRINTABLESTRING                = $13;
  {* �ɴ�ӡ�ַ������͵� Ber Tag}

  CN_BER_TAG_TELETEXSTRING                  = $14;
  {* T61String �ַ������͵� Ber Tag}

  CN_BER_TAG_VIDEOTEXSTRING                 = $15;
  {* ��Ƶ�ַ������͵� Ber Tag}

  CN_BER_TAG_IA5STRING                      = $16;
  {* IA5 �ַ������͵� Ber Tag}

  CN_BER_TAG_UTCTIME                        = $17;
  {* Э������ʱ������ʱ�����͵� Ber Tag}

  CN_BER_TAG_GENERALIZEDTIME                = $18;
  {* ͨ������ʱ�����͵� Ber Tag}

  CN_BER_TAG_GRAPHICSTRING                  = $19;
  {* ͼ���ַ������͵� Ber Tag}

  CN_BER_TAG_VISIBLESTRING                  = $1A;
  {* �ɼ��ַ������͵� Ber Tag}

  CN_BER_TAG_GENERALSTRING                  = $1B;
  {* ��ͨ�ַ������͵� Ber Tag}

  CN_BER_TAG_UNIVERSALSTRING                = $1C;
  {* ͨ���ַ������͵� Ber Tag}

  CN_BER_TAG_CHARACTER_STRING               = $1D;
  {* �ַ����ַ������͵� Ber Tag}

  CN_BER_TAG_BMPSTRING                      = $1E;
  {* λͼ�ַ������͵� Ber Tag}

type
  ECnBerException = class(Exception);
  {* BER ����쳣}

  TCnBerTagRange = CN_BER_TAG_BOOLEAN..CN_BER_TAG_BMPSTRING;
  {* BER �� Tag ��Χ}

  TCnBerTagSet = set of TCnBerTagRange;
  {* BER �� Tag ����}

  TCnBerTag = (cbtReserved_0, cbtBoolean, cbtInteger, cbtBit_String,
    cbtOctet_String, cbtNull, cbtObject_Identifier, cbtObject_Descripion,
    cbtExternal, cbtReal, cbtEnumerated, cbtEmbedded_Pdv, cbtUft8String,
    cbtRelative_Oid, cbtReserved_0E, cbtReserved_0F, cbtSequence, cbtSet,
    cbtNumericString, cbtPrintableString, cbtTeletexString, cbtVideotexString,
    cbtIa5String, cbtUtcTime, cbtGeneralizedTime, cbtGraphicString,
    cbtVisibleString, cbtGeneralString, cbtUniversalString, cbtCharacter_String,
    cbtBmpstring);
  {* BER �� Tag ö�ٶ���}

  TCnBerTags = set of TCnBerTag;
  {* BER �� Tag ö�ټ���}

  TCnBerReadNode = class(TCnLeaf)
  {* ����һ���������� ASN.1 �ڵ�}
  private
    FOriginData: PByte;
    FBerLength: Integer;
    FBerOffset: Integer;
    FBerTag: Integer;
    FBerDataLength: Integer;
    FBerDataOffset: Integer;
    function GetItems(AIndex: Integer): TCnBerReadNode;
    procedure SetItems(AIndex: Integer; const Value: TCnBerReadNode);

    function InternalAsInteger(ByteSize: Integer): Integer;
    function InternalAsString(TagSet: TCnBerTagSet): AnsiString;
    function GetBerDataAddress: Pointer;
    function GetBerAddress: Pointer;
  public
    procedure CopyDataTo(DestBuf: Pointer);
    {* �����ݸ��������������������ߴ�������Ҫ BerDataLength �ֽڴ�С��

       ������
         DestBuf: Pointer                 - ���ɴ�������ݵ����ݿ��ַ

       ����ֵ�����ޣ�
    }

    procedure CopyHeadTo(DestBuf: Pointer);
    {* ���ڵ�ͷ��Ҳ���� TL ���ݸ��������������������ߴ�������Ҫ BerLength - BerDataLength �ֽڴ�С��

       ������
         DestBuf: Pointer                 - ���ɴ�������ݵ����ݿ��ַ

       ����ֵ�����ޣ�
    }

    procedure CopyTLVTo(DestBuf: Pointer);
    {* ���ڵ�ȫ�����ݸ��������������������ߴ�������Ҫ BerLength �ֽڴ�С��

       ������
         DestBuf: Pointer                 - ���ɴ�������ݵ����ݿ��ַ

       ����ֵ�����ޣ�
    }

    function AsBoolean: Boolean;
    {* ���ز���ֵ��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - ���ز���ֵ
    }
    function AsShortInt: ShortInt;
    {* ���� 8 λ�з���������

       ������
         ���ޣ�

       ����ֵ��ShortInt                   - ���� 8 λ�з�������
    }

    function AsByte: Byte;
    {* ���� 8 λ�޷���������

       ������
         ���ޣ�

       ����ֵ��Byte                       - ���� 8 λ�޷�������
    }

    function AsSmallInt: SmallInt;
    {* ���� 8 λ�з���������

       ������
         ���ޣ�

       ����ֵ��SmallInt                   - ���� 16 λ�з�������
    }

    function AsWord: Word;
    {* ���� 16 λ�޷���������

       ������
         ���ޣ�

       ����ֵ��Word                       - ���� 16 λ�޷�������
    }

    function AsInteger: Integer;
    {* ���� 32 λ�з���������

       ������
         ���ޣ�

       ����ֵ��Integer                    - ���� 32 λ�з�������
    }

    function AsCardinal: Cardinal;
    {* ���� 32 λ�޷���������

       ������
         ���ޣ�

       ����ֵ��Cardinal                   - ���� 32 λ�޷�������
    }

    function AsInt64: Int64;
    {* ���� 64 λ�з���������

       ������
         ���ޣ�

       ����ֵ��Int64                      - ���� 64 λ�з�������
    }

    // ע���������ͷ��صķ���������ʱӦ���� BerDataLength ��Ӧ��������ʱ�����

    function AsCommonInteger: Integer;
    {* �÷����� BerDataLength ��ʵ��ֵ�������Ͳ���ʵ�ʵ���BerDataLength ���� Integer ʱ����

       ������
         ���ޣ�

       ����ֵ��Integer                    - ��������ֵ
    }

    procedure AsBigNumber(OutNum: TCnBigNumber);
    {* ���ߴ緵�����ʹ�����

       ������
         OutNum: TCnBigNumber             - ���ɷ��������Ĵ���

       ����ֵ�����ޣ�
    }

    function AsRawString: string;
    {* ֱ�ӷ����ַ��������������͡�

       ������
         ���ޣ�

       ����ֵ��string                     - �����ַ���
    }

    function AsAnsiString: AnsiString;
    {* ���ص��ֽ��ַ�����

       ������
         ���ޣ�

       ����ֵ��AnsiString                 - ���ص��ֽ��ַ���
    }

    function AsString: string;
    {* �����ַ���������Ϊ�����ַ������͡�

       ������
         ���ޣ�

       ����ֵ��string                     - �����ַ���
    }

    function AsPrintableString: string;
    {* ���ؿɴ�ӡ�ַ�����

       ������
         ���ޣ�

       ����ֵ��string                     - ���ؿɴ�ӡ�ַ���
    }

    function AsIA5String: string;
    {* ���ش� ASCII ����ַ�����

       ������
         ���ޣ�

       ����ֵ��string                     - ���ش� ASCII ���ַ���
    }

    function AsDateTime: TDateTime;
    {* ���� UTCTime �� GeneralizedTime��

       ������
         ���ޣ�

       ����ֵ��TDateTime                  - ��������ʱ��
    }

    function IsNull: Boolean;
    {* �����Ƿ�Ϊ Null��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�Ϊ Null
    }

    function IsString: Boolean;
    {* �����Ƿ�Ϊ�ַ�����

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�Ϊ�ַ���
    }
    function IsInteger: Boolean;
    {* �����Ƿ�Ϊ������

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�Ϊ����
    }

    function IsDateTime: Boolean;
    {* �����Ƿ�Ϊ����ʱ�䡣

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�Ϊ����ʱ��
    }

    function GetNextSibling: TCnBerReadNode;
    {* ������һ��ͬ���ڵ㡣

       ������
         ���ޣ�

       ����ֵ��TCnBerReadNode             - ������һ��ͬ���ڵ�
    }

    function GetPrevSibling: TCnBerReadNode;
    {* ������һ��ͬ���ڵ㡣

       ������
         ���ޣ�

       ����ֵ��TCnBerReadNode             - ������һ��ͬ���ڵ�
    }

    property Items[AIndex: Integer]: TCnBerReadNode read GetItems write SetItems; default;
    {* ��Ŀ����}

    property BerOffset: Integer read FBerOffset write FBerOffset;
    {* �ýڵ��Ӧ�� ASN.1 ���ݱ����������е�ƫ��}
    property BerAddress: Pointer read GetBerAddress;
    {* �����ڵ��������ʼ��ַ��Ҳ���� FOriginData + FBerOffset}
    property BerLength: Integer read FBerLength write FBerLength;
    {* �����ڵ�����ݳ���}

    property BerTag: Integer read FBerTag write FBerTag;
    {* �ڵ����ͣ�Ҳ���� Tag}
    property BerDataLength: Integer read FBerDataLength write FBerDataLength;
    {* �ڵ����ݳ���}
    property BerDataOffset: Integer read FBerDataOffset write FBerDataOffset;
    {* �ýڵ��Ӧ�����������������е�ƫ��}
    property BerDataAddress: Pointer read GetBerDataAddress;
    {* �ýڵ��Ӧ�����ݵ���ʼ��ַ������ FOriginData + FBerDataOffset}
  end;

  TCnBerReader = class(TObject)
  {* ��ȡ������ BER �������ݿ�Ľ�������}
  private
    FBerTree: TCnTree;
    FData: PByte;
    FDataByteLen: Cardinal;
    FParseInnerString: Boolean;
    FCurrentIsBitString: Boolean;
{$IFDEF DEBUG}
{$IFDEF ENABLE_UIINTERACT}
  {$IFDEF MSWINDOWS}
    function GetOnSaveNode: TCnTreeNodeEvent;
    procedure SetOnSaveNode(const Value: TCnTreeNodeEvent);
  {$ENDIF}
  {$IFDEF SUPPORT_FMX}
    function GetOnSaveItem: TCnTreeViewItemEvent;
    procedure SetOnSaveItem(const Value: TCnTreeViewItemEvent);
  {$ENDIF}
{$ENDIF}
{$ENDIF}
    function GetTotalCount: Integer;
    function GetItems(Index: Integer): TCnBerReadNode;
    function ParseArea(Parent: TCnLeaf; AData: PByteArray; ADataByteLen: Cardinal;
      AStartOffset: Cardinal; var IsEnd: Boolean; IsTop: Boolean = True): Cardinal;
    {* ����һ������Ϊһ�������ڵ㣬������������� ASN.1 �ڵ����ι��� Parent �ڵ��£�
      ������� Area ���ܳ��ȡ�IsTop ��ʾ�� Parent �� Root �����ڵ㣬�Դ��������⡣
      ADataLen ����� 0����ʾ�ǲ������ڵ㣬ParseArea ��ʱ��Ҫ�ж� 00 00 �Ը�֪��һ����β��
      ��ͨ�� IsEnd �������ظ��ߵ�����}
  protected

  public
    constructor Create(Data: PByte; DataByteLen: Cardinal; AParseInnerString: Boolean = False);
    {* ���캯����

       ������
         Data: PByte                      - ����������������ַ
         DataByteLen: Cardinal            - ���������������ֽڳ���
         AParseInnerString: Boolean       - �Ƿ������Ƕ�ַ���

       ����ֵ��TCnBerReader               - ���ش����Ķ���ʵ��
    }

    destructor Destroy; override;
    {* ��������}

    procedure ParseToTree;
    {* ��������Ҫ���ô˷���ʵʩ����}
    procedure ManualParseNodeData(RootNode: TCnBerReadNode);
    {* ĳЩ�ڵ�� Tag ���� SEQUENCE/SET �ȵ�����ȴ�������ݣ���Ҫ�ⲿ�ֹ����ô˷�����ʵʩ���ν�����

       ������
         RootNode: TCnBerReadNode         - ���ֹ������Ľڵ�

       ����ֵ�����ޣ�
    }

{$IFDEF DEBUG}
{$IFDEF ENABLE_UIINTERACT}
  {$IFDEF MSWINDOWS}
    procedure DumpToTreeView(ATreeView: ComCtrls.TTreeView); {$IFDEF SUPPORT_FMX} overload; {$ENDIF}
    {* ����������������ϵ� VCL ���ؼ��С�

       ������
         ATreeView: ComCtrls.TTreeView    - ��������ݵ� VCL ���ؼ�ʵ��

       ����ֵ�����ޣ�
    }

    property OnSaveNode: TCnTreeNodeEvent read GetOnSaveNode write SetOnSaveNode;
  {$ENDIF}
  {$IFDEF SUPPORT_FMX}
    procedure DumpToTreeView(ATreeView: FMX.TreeView.TTreeView); {$IFDEF MSWINDOWS} overload; {$ENDIF}
    {* ����������������ϵ� FMX ���ؼ��С�

       ������
         ATreeView: FMX.TreeView.TTreeView                - ��������ݵ� FMX ���ؼ�ʵ��

       ����ֵ�����ޣ�
    }

    property OnSaveItem: TCnTreeViewItemEvent read GetOnSaveItem write SetOnSaveItem;
  {$ENDIF}
{$ENDIF}
{$ENDIF}

    property ParseInnerString: Boolean read FParseInnerString;
    {* �Ƿ� BitString/OctetString ����Ҳ��������������������PKCS#8 �� Pem �ļ��г���}
    property TotalCount: Integer read GetTotalCount;
    {* ���������� ASN.1 �ڵ�����}
    property Items[Index: Integer]: TCnBerReadNode read GetItems;
    {* ˳��������н��������� ASN.1 �ڵ㣬�±�� 0 ��ʼ�������� Tree ����� Root}
  end;

  TCnBerWriteNode = class(TCnLeaf)
  {* ����һ���ڱ��벢д��� ASN.1 �ڵ�}
  private
    FMem: TMemoryStream;        // ���ɻ������ͽڵ���������ݣ���ֻ��������
    FHead: array[0..5] of Byte; // ����������֮ǰ��ͷ���ݣ����� Tag��Len ��
    FHeadLen: Integer;
    FIsContainer: Boolean;
    FDataLength: Integer;
    FData: Pointer;
    FBerTag: Integer;
    FBerTypeMask: Byte;
    function GetIsContainer: Boolean;
    procedure SetIsContainer(const Value: Boolean);
    function GetItems(AIndex: Integer): TCnBerWriteNode;
    procedure SetItems(AIndex: Integer; const Value: TCnBerWriteNode);

    procedure FillHeadCalcLen(ATag: Integer; ADataLen: Integer);
    // ���㲢��� FHead �� FHeadLen
  public
    constructor Create(ATree: TCnTree); override;
    {* ���캯����

       ������
         ATree: TCnTree                   - ָ��������

       ����ֵ��TCnBerWriteNode            - ���ش����Ķ���ʵ��
    }

    destructor Destroy; override;
    {* ��������}

    function SaveToStream(Stream: TStream): Integer;
    {* ����ǻ������;ͽ��Լ�д����������д���ֽڳ��ȣ�
       ����������򰤸����ӽڵ�д������Ȼ����Լ�ͷ��ƴ������ƴ���ӽڵ�ķ����ֽڳ��ȡ�
       ����ֵΪ���ڵ�����ӽڵ���������ݵ�д���ֽ��ֽڳ��ȡ�

       ������
         Stream: TStream                  - д�����

       ����ֵ��Integer                    - ���ڵ�����ӽڵ���������ݵ�д���ֽڳ���
    }

    function SaveValueToStream(Stream: TStream): Integer;
    {* ����ǻ������;ͽ��Լ����� Tag �볤��֮��������������д����������д���ֽڳ��ȡ�
       ����������򰤸����ӽڵ�д�����󷵻ء�
       ����ֵΪ�ӽڵ��������ݳ��ȵ��������Լ��� Tag �볤�ȡ�

       ������
         Stream: TStream                  - д�����

       ����ֵ��Integer                    - �����ӽڵ��������ݵ�д���ֽڳ���
    }

    function GetNodeLength: Integer;
    {* ����ǻ������;ͷ��������ֽڳ��ȣ�������������Լ�ͷ�Ӹ��ӽڵ���ֽڳ��ȡ�

       ������
         ���ޣ�

       ����ֵ��Integer                    - �����ֽڳ���
    }

    procedure FillBasicNode(ATag: Integer; AData: PByte; ADataByteLen: Integer);
    {* ��紴���˻����ڵ���ô˷������������ݣ�Container �ڵ㲻�ã�
       ע��ԭʼ BitString ��֧��ͷ�ֽڣ��ݲ���Ҫ�Լ���䡣

       ������
         ATag: Integer                    - �������� Tag
         AData: PByte                     - �����������ݿ��ַ
         ADataByteLen: Integer            - �����������ݿ��ֽڳ���

       ����ֵ�����ޣ�
    }

    property Items[AIndex: Integer]: TCnBerWriteNode read GetItems write SetItems;
    {* �����ӽڵ�}
    property Data: Pointer read FData write FData;
    {* ���ݿ��ַ}
    property DataLength: Integer read FDataLength write FDataLength;
    {* ���ݿ��ֽڳ���}
    property IsContainer: Boolean read GetIsContainer write SetIsContainer;
    {* �Ƿ�������}
    property BerTag: Integer read FBerTag write FBerTag;
    {* �ڵ����ͣ�Ҳ���� Tag}
    property BerTypeMask: Byte read FBerTypeMask write FBerTypeMask;
    {* �ڵ� Mask��ֻ�������λ��Ч��д�� BerTag ʱ�����ֵ��}
  end;

  TCnBerWriter = class(TObject)
  {* д BER ��������ݵĹ�����}
  private
    FBerTree: TCnTree;
    function GetTotalSize: Integer;
{$IFDEF DEBUG}
{$IFDEF ENABLE_UIINTERACT}
  {$IFDEF MSWINDOWS}
    function GetOnSaveNode: TCnTreeNodeEvent;
    procedure SetOnSaveNode(const Value: TCnTreeNodeEvent);
  {$ENDIF}
  {$IFDEF SUPPORT_FMX}
    function GetOnSaveItem: TCnTreeViewItemEvent;
    procedure SetOnSaveItem(const Value: TCnTreeViewItemEvent);
  {$ENDIF}
{$ENDIF}
{$ENDIF}
  public
    constructor Create;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}

    procedure SaveTo(DestBuf: Pointer);
    {* �� BER ���ݱ����� DestBuf ��ָ���ڴ������ڴ�����С������Ҫ GetTotalSize��

       ������
         DestBuf: Pointer                 - ���ɴ�������ݵ����ݿ��ַ

       ����ֵ�����ޣ�
    }

    procedure SaveToFile(const FileName: string);
    {* �� BER ���ݱ�����ָ���ļ���

       ������
         const FileName: string           - ������ļ���

       ����ֵ�����ޣ�
    }

    procedure SaveToStream(Stream: TStream);
    {* �� BER ���ݱ���������

       ������
         Stream: TStream                  - �������

       ����ֵ�����ޣ�
    }

{$IFDEF DEBUG}
{$IFDEF ENABLE_UIINTERACT}
  {$IFDEF MSWINDOWS}
    procedure DumpToTreeView(ATreeView: ComCtrls.TTreeView); {$IFDEF SUPPORT_FMX} overload; {$ENDIF}
    {* ����������������ϵ� VCL ���ؼ��С�

       ������
         ATreeView: ComCtrls.TTreeView    - ��������ݵ� VCL ���ؼ�ʵ��

       ����ֵ�����ޣ�
    }

    property OnSaveNode: TCnTreeNodeEvent read GetOnSaveNode write SetOnSaveNode;
  {$ENDIF}
  {$IFDEF SUPPORT_FMX}
    procedure DumpToTreeView(ATreeView: FMX.TreeView.TTreeView); {$IFDEF MSWINDOWS} overload; {$ENDIF}
    {* ����������������ϵ� FMX ���ؼ��С�

       ������
         ATreeView: FMX.TreeView.TTreeView                - ��������ݵ� FMX ���ؼ�ʵ��

       ����ֵ�����ޣ�
    }

    property OnSaveItem: TCnTreeViewItemEvent read GetOnSaveItem write SetOnSaveItem;
  {$ENDIF}
{$ENDIF}
{$ENDIF}

    function AddNullNode(Parent: TCnBerWriteNode = nil): TCnBerWriteNode;
    {* ����һ�� Null �ڵ㡣

       ������
         Parent: TCnBerWriteNode          - �������ڵ�ĸ��ڵ㣬��Ϊ nil ��ʾ���������ڵ���

       ����ֵ��TCnBerWriteNode            - ���������Ľڵ�
    }

    function AddBasicNode(ATag: Integer; AData: PByte; ADataByteLen: Integer;
      Parent: TCnBerWriteNode = nil): TCnBerWriteNode; overload;
    {* ����һ���������͵Ľڵ㣬���ݴ� AData ���Ƴ���Ϊ DataLen �Ķ���

       ������
         ATag: Integer                    - �������ڵ�� Tag
         AData: PByte                     - �����������ݿ��ַ
         ADataByteLen: Integer            - �����������ݿ��ֽڳ���
         Parent: TCnBerWriteNode          - �������ڵ�ĸ��ڵ㣬��Ϊ nil ��ʾ���������ڵ���

       ����ֵ��TCnBerWriteNode            - ���������Ľڵ�
    }

    function AddBasicNode(ATag: Integer; AStream: TStream;
      Parent: TCnBerWriteNode = nil): TCnBerWriteNode; overload;
    {* ����һ���������͵Ľڵ㣬���ݴ�ָ�������ƶ�����

       ������
         ATag: Integer                    - �������ڵ�� Tag
         AStream: TStream                 - ��������������
         Parent: TCnBerWriteNode          - �������ڵ�ĸ��ڵ㣬��Ϊ nil ��ʾ���������ڵ���

       ����ֵ��TCnBerWriteNode            - ���������Ľڵ�
    }

    function AddAnsiStringNode(ATag: Integer; const AStr: AnsiString;
      Parent: TCnBerWriteNode = nil): TCnBerWriteNode;
    {* ����һ���ַ����͵� Node�����ݴ�ָ�� AnsiString ���ƶ�����

       ������
         ATag: Integer                    - �������ڵ�� Tag
         const AStr: AnsiString           - ���������ַ���
         Parent: TCnBerWriteNode          - �������ڵ�ĸ��ڵ㣬��Ϊ nil ��ʾ���������ڵ���

       ����ֵ��TCnBerWriteNode            - ���������Ľڵ�
    }

    function AddContainerNode(ATag: Integer; Parent: TCnBerWriteNode = nil): TCnBerWriteNode;
    {* ����һ���������͵Ľڵ㣬�˽ڵ������Ϊ���� BasicNode �� Parent��

       ������
         ATag: Integer                    - �������ڵ�� Tag
         Parent: TCnBerWriteNode          - �������ڵ�ĸ��ڵ㣬��Ϊ nil ��ʾ���������ڵ���

       ����ֵ��TCnBerWriteNode            - ���������Ľڵ�
    }

    function AddRawNode(RawTag: Integer; RawLV: PByte; LVLen: Integer;
      Parent: TCnBerWriteNode = nil): TCnBerWriteNode;
    {* ����һ��ԭʼ�ڵ㣬�˽ڵ�� Tag ֱֵ���� RawTag ָ����
       ����ĳ������ݵȲ������ˣ�ֱ���� RawLV �� LVLen ������ָ����

       ������
         RawTag: Integer                  - ָ����ԭʼ Tag
         RawLV: PByte                     - ָ����ԭʼ����
         LVLen: Integer                   - ָ����ԭʼ�����ֶ�������ĳ���
         Parent: TCnBerWriteNode          - �������ڵ�ĸ��ڵ㣬��Ϊ nil ��ʾ���������ڵ���

       ����ֵ��TCnBerWriteNode            - ���������Ľڵ�
    }

    property TotalSize: Integer read GetTotalSize;
    {* ���������ܳߴ磬�ֽ�Ϊ��λ}
  end;

function CompareObjectIdentifier(Node: TCnBerReadNode; OIDAddr: Pointer;
  OIDSize: Integer): Boolean;
{* �Ƚ�һ���ڵ��е������Ƿ����һ��ָ���� OID��

   ������
     Node: TCnBerReadNode                 - ���ȽϵĽڵ�
     OIDAddr: Pointer                     - OID ���ݿ��ַ
     OIDSize: Integer                     - OID ���ݿ��ֽڳ���

   ����ֵ��Boolean                        - �����Ƿ����
}

function AddBigNumberToWriter(Writer: TCnBerWriter; Num: TCnBigNumber;
  Parent: TCnBerWriteNode; Tag: Integer = CN_BER_TAG_INTEGER; FixedLen: Integer = 0): TCnBerWriteNode;
{* ��һ������������д��һ�������� Ber ���͸�ʽ�Ľڵ㣬FixedLen Ϊ 0 ʱ�޹̶����ȣ�
   FixedLen ָ������ʵ�ʳ��Ȳ���ʱʹ�ù̶����ȣ�Ϊ 0 ʱ��ʹ�ô���ʵ�ʳ��ȡ�
   �� FixedLen Ϊ 0���ڵ��������λ��ʵ����������Ƿ��һ���ֽ� 0��
   �� FixedLen ��Ϊ 0���ڵ���� FixedLen �����ʵ�ʳ��ȵĻ�����ǿ�м�һ���ֽ� 0��

   ������
     Writer: TCnBerWriter                 - BER д��Ĺ�����ʵ��
     Num: TCnBigNumber                    - ��д��Ĵ���
     Parent: TCnBerWriteNode              - �������Ľڵ�ĸ��ڵ�
     Tag: Integer                         - ָ��д��� Tag
     FixedLen: Integer                    - д��̶��ֽڳ��ȣ�0 ��ʾʹ�ô���ʵ�ʳ���

   ����ֵ��TCnBerWriteNode                - ���������Ľڵ�
}

procedure PutIndexedBigIntegerToBigNumber(Node: TCnBerReadNode; BigNumber: TCnBigNumber);
{* ��һ�� Ber ���͸�ʽ�Ľڵ�����д��һ�������С�

   ������
     Node: TCnBerReadNode                 - ��д��Ľڵ�
     BigNumber: TCnBigNumber              - ��д��Ĵ���

   ����ֵ�����ޣ�
}

implementation

resourcestring
  SCnErrorDataCorruptionTagBase = 'Data Corruption when Processing Tag (Base %d), %d > %d.';
  SCnErrorDataCorruptionTagBaseLen = 'Data Corruption when Processing Tag (Base %d) at %d Got Len %d.';
  SCnErrorLengthTooLongOrIncorrect = 'Length Too Long or Incorrect (Base %d) %d.';
  SCnErrorOffsetLenTag = 'Offset %d. Len %d. Tag %d (%s). DataLen %d';
  SCnErrorBerTagTypeMismatchForBytesize = 'Ber Tag Type Mismatch for ByteSize: ';
  SCnErrorInvalidBytesize = 'Invalid ByteSize: ';
  SCnErrorDataLengthOverflow = 'Data Length %d Overflow for Required %d.';
  SCnErrorBerTagTypeMismatch = 'Ber Tag Type Mismatch for Int64: ';
  SCnErrorBerTagTypeMismatchForString = 'Ber Tag Type Mismatch for String: ';
  SCnErrorBerTagTypeMismatchForBignumber = 'Ber Tag Type Mismatch for BigNumber.';
  SCnErrorBerTagTypeMismatchForBoolean = 'Ber Tag Type Mismatch for Boolean: ';
  SCnErrorBerTagTypeMismatchForCommonInteger = 'Ber Tag Type Mismatch for Common Integer.';
  SCnErrorDataLengthOverflowForCommonInteger = 'Data Length %d Overflow for Common Integer.';

const
  CN_TAG_SET_STRING: TCnBerTagSet = [CN_BER_TAG_UFT8STRING, CN_BER_TAG_NUMERICSTRING,
    CN_BER_TAG_PRINTABLESTRING, CN_BER_TAG_IA5STRING, CN_BER_TAG_TELETEXSTRING];

  CN_TAG_SET_TIME: TCnBerTagSet = [CN_BER_TAG_UTCTIME, CN_BER_TAG_GENERALIZEDTIME];

{$IFDEF DEBUG}

function GetTagName(Tag: Integer): string;
begin
  Result := 'Invalid';
  if Tag in [Ord(Low(TCnBerTag))..Ord(High(TCnBerTag))] then
  begin
    Result := GetEnumName(TypeInfo(TCnBerTag), Tag);
    if (Length(Result) > 3) and (Copy(Result, 1, 3) = 'cbt') then
      Delete(Result, 1, 3);
  end;
end;

{$ENDIF}

function CompareObjectIdentifier(Node: TCnBerReadNode; OIDAddr: Pointer;
  OIDSize: Integer): Boolean;
var
  P: Pointer;
begin
  Result := False;
  if (Node <> nil) then
  begin
    P := Node.BerDataAddress;
    if (P <> nil) and (OIDAddr <> nil) and (OIDSize > 0) then
    begin
      if OIDSize = Node.BerDataLength then
        Result := CompareMem(OIDAddr, P, OIDSize);
    end;
  end;
end;

// FixedLen С�ڻ���� 0 ʱ������������λ�� 1������Ҫǰ�油 0 �����븺���ı�������
// FixedLen ���� 0 ʱ���繻������̶��� 0�����򰴴���ʵ������� 0
function CalcIntegerTLV(BigNumber: TCnBigNumber; FixedLen: Integer = 0): Cardinal;
begin
  Result := BigNumber.GetBytesCount;
  if FixedLen <= 0 then
  begin
    if BigNumber.IsBitSet((Result * 8) - 1) then // �������λ�Ƿ��� 1 �����Ƿ� 0
      Inc(Result);
  end
  else
  begin
    if Cardinal(FixedLen) >= Result then // �̶�λ��������ǰ�油 0
      Result := FixedLen + 1
    else if BigNumber.IsBitSet((Result * 8) - 1) then // �̶�λ������ʵ�ʳ���ǰ�水�貹 0
      Inc(Result);
  end;
end;

function AddBigNumberToWriter(Writer: TCnBerWriter; Num: TCnBigNumber;
  Parent: TCnBerWriteNode; Tag: Integer; FixedLen: Integer): TCnBerWriteNode;
var
  P: Pointer;
  C, D: Integer;
begin
  Result := nil;
  if (Writer = nil) or (Num = nil) then
    Exit;

  // Integer ������Ҫ�������λ�Ծ����Ƿ�һ�� 0
  C := CalcIntegerTLV(Num, FixedLen);
  if C <= 0 then
    Exit;

  P := GetMemory(C);
  D := C - Num.GetBytesCount;

  FillChar(P^, D, 0);
  Num.ToBinary(PAnsiChar(TCnIntAddress(P) + D));

  Result := Writer.AddBasicNode(Tag, P, C, Parent);
  FreeMemory(P);
end;

procedure PutIndexedBigIntegerToBigNumber(Node: TCnBerReadNode; BigNumber: TCnBigNumber);
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

{ TCnBerReader }

constructor TCnBerReader.Create(Data: PByte; DataByteLen: Cardinal;
  AParseInnerString: Boolean);
begin
  FData := Data;
  FDataByteLen := DataByteLen;
  FParseInnerString := AParseInnerString;
  FBerTree := TCnTree.Create(TCnBerReadNode);
end;

destructor TCnBerReader.Destroy;
begin
  FBerTree.Free;
  inherited;
end;

{$IFDEF DEBUG}
{$IFDEF ENABLE_UIINTERACT}
{$IFDEF MSWINDOWS}

procedure TCnBerReader.DumpToTreeView(ATreeView: ComCtrls.TTreeView);
begin
  FBerTree.SaveToTreeView(ATreeView);
end;

function TCnBerReader.GetOnSaveNode: TCnTreeNodeEvent;
begin
  Result := FBerTree.OnSaveANode;
end;

procedure TCnBerReader.SetOnSaveNode(const Value: TCnTreeNodeEvent);
begin
  FBerTree.OnSaveANode := Value;
end;

{$ENDIF}

{$IFDEF SUPPORT_FMX}

procedure TCnBerReader.DumpToTreeView(ATreeView: FMX.TreeView.TTreeView);
begin
  FBerTree.SaveToTreeView(ATreeView);
end;

function TCnBerReader.GetOnSaveItem: TCnTreeViewItemEvent;
begin
  Result := FBerTree.OnSaveAItem;
end;

procedure TCnBerReader.SetOnSaveItem(const Value: TCnTreeViewItemEvent);
begin
  FBerTree.OnSaveAItem := Value;
end;

{$ENDIF}
{$ENDIF}
{$ENDIF}

function TCnBerReader.GetItems(Index: Integer): TCnBerReadNode;
begin
  Result := TCnBerReadNode(FBerTree.Items[Index + 1]);
end;

function TCnBerReader.GetTotalCount: Integer;
begin
  Result := FBerTree.Root.AllCount;
end;

function TCnBerReader.ParseArea(Parent: TCnLeaf; AData: PByteArray;
  ADataByteLen: Cardinal; AStartOffset: Cardinal; var IsEnd: Boolean; IsTop: Boolean): Cardinal;
var
  Run, Start: Cardinal;
  Tag, DataLen, DataOffset, LenLen, Delta, SubLen: Integer;
  B: Byte;
  IsStruct, OutLenIsZero, MyEnd, LenSingle: Boolean;
  ALeaf: TCnBerReadNode;
begin
  Run := 0;  // Run �ǻ��� AData ��ʼ����ƫ����
  Result := ADataByteLen;
  OutLenIsZero := ADataByteLen = 0;
  MyEnd := False;

  while (ADataByteLen = 0) or (Run < ADataByteLen) do // ADataLen ������� 0 ��ʾ�ǲ������ڵ�
  begin
    B := AData^[Run];

    if B = $FF then
      Exit;

    Start := Run;

    // ���� Tag ����
    IsStruct := (B and CN_BER_TAG_STRUCT_MASK) <> 0;
    Tag := B and CN_BER_TAG_VALUE_MASK;

    Inc(Run);
    if (Run >= ADataByteLen) and (ADataByteLen > 0) then
      raise ECnBerException.CreateFmt(SCnErrorDataCorruptionTagBase,
        [AStartOffset, Run, ADataByteLen]);

    // Run ָ�򳤶ȣ���� Tag �ͳ��ȶ��� 0����ʾ���������ݵ��սᣬ���½��ڵ�
    // ע�� ADataLen ������Ϊ�Ƕ���ڵ㣬�������ⲿ���룬
    if (IsTop or (ADataByteLen = 0)) and (B = 0) and (AData^[Run] = 0) then
    begin
      if OutLenIsZero then // �� Tag �� 0 �������ֽ�
        Inc(Result, 2);
      IsEnd := True;
      Exit;
    end;

    // ������
    Delta := 1;  // 1 ��ʾ Tag ��ռ�ֽ�
    B := AData^[Run];
    if (B and CN_BER_LENLEN_MASK) = 0 then
    begin
      // ���ֽھ��ǳ���
      DataLen := B;
      DataOffset := AStartOffset + Run + 1;
      Inc(Delta); // ���ϳ��ȵ���һ�ֽ�
      Inc(Run);   // Run ָ������
      LenSingle := True;
    end
    else
    begin
      // ���ֽڸ�λΪ 1����ʾ���ȵĳ���
      LenSingle := False;
      LenLen := B and CN_BER_LENGTH_MASK;
      Inc(Delta); // ���ϳ��ȵĳ�����һ�ֽ�
      Inc(Run);   // Run ָ����峤�ȣ���� LenLen Ϊ 0���� Run ָ����һ�� Area ��ͷ

      // AData[Run] �� AData[Run + LenLen - 1] �ǳ���
      if (ADataByteLen > 0) and (Run + Cardinal(LenLen) - 1 >= ADataByteLen) then
        raise ECnBerException.CreateFmt(SCnErrorDataCorruptionTagBaseLen,
          [AStartOffset, Run, LenLen]);

      DataLen := 0;
      if LenLen = SizeOf(Byte) then
        DataLen := AData^[Run]
      else if LenLen = SizeOf(Word) then
        DataLen := (Cardinal(AData^[Run]) shl 8) or Cardinal(AData^[Run + 1])
      else if LenLen > SizeOf(Word) then  // TODO: LenLen = 0 ʱ�ǲ��������룬BER ��֧�֣��� 00 00 ��β
        raise ECnBerException.CreateFmt(SCnErrorLengthTooLongOrIncorrect, [AStartOffset, LenLen]);

      DataOffset := AStartOffset + Run + Cardinal(LenLen);
      if LenLen > 0 then
      begin
        Inc(Delta, LenLen);
        Inc(Run, LenLen);   // Run ָ������
      end;
    end;

    // Tag, Len, DataOffset ����ȫ�ˣ�Delta ��������ʼ���뵱ǰ�ڵ���ʼ����ƫ��
    if Parent = nil then
      Parent := FBerTree.Root;

    ALeaf := FBerTree.AddChild(Parent) as TCnBerReadNode;
    ALeaf.FOriginData := FData;

    ALeaf.BerOffset := AStartOffset + Start;
    ALeaf.BerLength := DataLen + Delta;
    ALeaf.BerTag := Tag;
    ALeaf.BerDataLength := DataLen;  // ע�� DataLen Ϊ 0 ʱ���� LenSingle �� True����ʾû������Ϊ False �ű�ʾδ������
    ALeaf.BerDataOffset := DataOffset;

    if OutLenIsZero then
      Inc(Result, ALeaf.BerLength);

{$IFDEF DEBUG}
    ALeaf.Text := Format(SCnErrorOffsetLenTag, [ALeaf.BerOffset,
      ALeaf.BerLength, ALeaf.BerTag, GetTagName(ALeaf.BerTag), ALeaf.BerDataLength]);
{$ENDIF}

    SubLen := 0;
    // ���ӽڵ�ʱ�Գ��ȵ�Ҫ��(DataLen > 0) �� (DataLen = 0 �� LenSingle Ϊ False)
    // Ҳ����˵������һ���ֽڱ�ʾ DataLen �� 0��ȷʵ�ͱ�ʾû����
    // ��ϱ�ʾ 0����˵�����޹̶��������ݣ����ӽڵ������һ���ֽڵ��� 00 00 ��β
    if (IsStruct or (FParseInnerString and (ALeaf.BerTag in [CN_BER_TAG_BIT_STRING,
      CN_BER_TAG_OCTET_STRING])))
      and ((DataLen > 0) or (DataLen = 0) and not LenSingle) then
    begin
      // ˵�� BerDataOffset �� BerDataLength �ڿ������ӽڵ�
      try
        if ALeaf.BerTag = CN_BER_TAG_BIT_STRING then // �ճ� 8 �ı�����ȱ�ٵ� Bit ������ӦС�� 8�������ܼ������������ and (AData^[Run + 1] < 8)
        begin
          FCurrentIsBitString := True;
          try
            try
              // BIT_STRING ��������һ�������ֽ��Ǹ� BIT_STRING �ճ� 8 �ı�����ȱ�ٵ� Bit ��������Ҫ����
              SubLen := ParseArea(ALeaf, PByteArray(TCnNativeUInt(AData) + Run + 1),
                ALeaf.BerDataLength - 1, ALeaf.BerDataOffset + 1, MyEnd, False);
            except
              // ����Щ����û����ֽڡ������������ʱ������������ֽڣ����½���
              SubLen := ParseArea(ALeaf, PByteArray(TCnNativeUInt(AData) + Run),
                ALeaf.BerDataLength, ALeaf.BerDataOffset, MyEnd, False);
            end;
          finally
            FCurrentIsBitString := False;
          end;
        end
        else
        begin
          SubLen := ParseArea(ALeaf, PByteArray(TCnNativeUInt(AData) + Run),
            ALeaf.BerDataLength, ALeaf.BerDataOffset, MyEnd, False);
        end;
      except
        ; // �����Ƕ����ʧ�ܣ�����ֹ��������ͨ�ڵ�
      end;
    end;

    if DataLen = 0 then // ���ֵı��飨������ͷ�ܿ飩���Ȳ�ȷ��ʱ������ʱҪ���ӽ������صĳ���
      Inc(Run, SubLen)
    else
      Inc(Run, DataLen);

    Inc(Result, SubLen);  // �ӿ����۽�������䳤�ȶ�Ҫ���ӵ��������Ϸ���
  end;
end;

procedure TCnBerReader.ParseToTree;
var
  MyEnd: Boolean;
begin
  ParseArea(FBerTree.Root, PByteArray(FData), FDataByteLen, 0, MyEnd);
end;

procedure TCnBerReader.ManualParseNodeData(RootNode: TCnBerReadNode);
var
  MyEnd: Boolean;
begin
  RootNode.Clear;
  ParseArea(RootNode, PByteArray(RootNode.BerDataAddress), RootNode.BerDataLength,
    RootNode.BerDataOffset, MyEnd, False);
  // ע�� RootNode һ�㲻���� Tree �� Root����� IsTop Ҫ�� False
end;

{ TCnBerReadNode }

function TCnBerReadNode.AsPrintableString: string;
begin
  Result := string(InternalAsString([CN_BER_TAG_PRINTABLESTRING]));
end;

function TCnBerReadNode.InternalAsInteger(ByteSize: Integer): Integer;
var
  IntValue: Integer;
begin
  if FBerTag <> CN_BER_TAG_INTEGER then
    raise ECnBerException.Create(SCnErrorBerTagTypeMismatchForBytesize + IntToStr(ByteSize));

  if not (ByteSize in [SizeOf(Byte)..SizeOf(Cardinal)]) then
    raise ECnBerException.Create(SCnErrorInvalidBytesize + IntToStr(ByteSize));

  if FBerDataLength > ByteSize then
    raise ECnBerException.CreateFmt(SCnErrorDataLengthOverflow,
      [FBerDataLength, ByteSize]);

  IntValue := 0;
  CopyDataTo(@IntValue);

  // Byte ���轻����SmallInt ������λ��Integer ������λ
  if ByteSize = SizeOf(Word) then
    IntValue := Integer(UInt16NetworkToHost(Word(IntValue)))
  else if ByteSize = SizeOf(Cardinal) then
    IntValue := UInt32NetworkToHost(IntValue);
  Result := IntValue;
end;

function TCnBerReadNode.AsInt64: Int64;
begin
  if FBerTag <> CN_BER_TAG_INTEGER then
    raise ECnBerException.Create(SCnErrorBerTagTypeMismatch + IntToStr(FBerTag));

  if FBerDataLength > SizeOf(Int64) then
    raise ECnBerException.CreateFmt(SCnErrorDataLengthOverflow,
      [FBerDataLength, SizeOf(Int64)]);

  Result := 0;
  CopyDataTo(@Result);
  Result := Int64NetworkToHost(Result);
end;

function TCnBerReadNode.AsByte: Byte;
begin
  Result := Byte(InternalAsInteger(SizeOf(Byte)));
end;

function TCnBerReadNode.AsCardinal: Cardinal;
begin
  Result := Cardinal(InternalAsInteger(SizeOf(Cardinal)));
end;

function TCnBerReadNode.AsInteger: Integer;
begin
  Result := Integer(InternalAsInteger(SizeOf(Integer)));
end;

function TCnBerReadNode.AsShortInt: ShortInt;
begin
  Result := ShortInt(InternalAsInteger(SizeOf(ShortInt)));
end;

function TCnBerReadNode.AsSmallInt: SmallInt;
begin
  Result := SmallInt(InternalAsInteger(SizeOf(SmallInt)));
end;

function TCnBerReadNode.AsWord: Word;
begin
  Result := Word(InternalAsInteger(SizeOf(Word)));
end;

procedure TCnBerReadNode.CopyDataTo(DestBuf: Pointer);
begin
  if (FOriginData <> nil) and (FBerDataLength > 0) then
    Move(Pointer(TCnIntAddress(FOriginData) + FBerDataOffset)^, DestBuf^, FBerDataLength);
end;

function TCnBerReadNode.GetItems(AIndex: Integer): TCnBerReadNode;
begin
  Result := inherited GetItems(AIndex) as TCnBerReadNode;
end;

procedure TCnBerReadNode.SetItems(AIndex: Integer; const Value: TCnBerReadNode);
begin
  inherited SetItems(AIndex, Value);
end;

function TCnBerReadNode.GetBerDataAddress: Pointer;
begin
  if FOriginData = nil then
    Result := nil
  else
    Result := Pointer(TCnIntAddress(FOriginData) + FBerDataOffset);
end;

function TCnBerReadNode.GetNextSibling: TCnBerReadNode;
begin
  Result := TCnBerReadNode(inherited GetNextSibling);
end;

function TCnBerReadNode.GetPrevSibling: TCnBerReadNode;
begin
  Result := TCnBerReadNode(inherited GetPrevSibling);
end;

procedure TCnBerReadNode.CopyHeadTo(DestBuf: Pointer);
begin
  if FOriginData <> nil then
    Move(Pointer(TCnIntAddress(FOriginData) + FBerOffset)^, DestBuf^, FBerLength - FBerDataLength);
end;

procedure TCnBerReadNode.CopyTLVTo(DestBuf: Pointer);
begin
  if (FOriginData <> nil) and (FBerLength > 0) then
    Move(Pointer(TCnIntAddress(FOriginData) + FBerOffset)^, DestBuf^, FBerLength);
end;

function TCnBerReadNode.AsIA5String: string;
begin
  Result := string(InternalAsString([CN_BER_TAG_IA5STRING]));
end;

function TCnBerReadNode.AsString: string;
begin
  Result := string(InternalAsString(CN_TAG_SET_STRING + CN_TAG_SET_TIME));
end;

function TCnBerReadNode.AsDateTime: TDateTime;
var
  S: string;
  Y, M, D, H, Mi, Se: Word;
begin
  S := string(InternalAsString(CN_TAG_SET_TIME));
  // TODO: YYMMDDhhmm ����� Z �� ss �� +- ʱ��
  if (Length(S) in [11, 13]) and (S[Length(S)] = 'Z') then
  begin
    Y := StrToInt(Copy(S, 1, 2)) + 2000;
    M := StrToInt(Copy(S, 3, 2));
    D := StrToInt(Copy(S, 5, 2));
    H := StrToInt(Copy(S, 7, 2));
    Mi := StrToInt(Copy(S, 9, 2));
    if Length(S) = 13 then
      Se := StrToInt(Copy(S, 11, 2))
    else
      Se := 0;

    Result := EncodeDate(Y, M, D) + EncodeTime(H, Mi, Se, 0);
  end
  else
    Result := StrToDateTime(S);

  // TODO: Ҳ������ Integer �� Binary Time ��ʽ��
  // 1970 �� 1 �� 1 ����ʱ����������ο� rfc4049
end;

function TCnBerReadNode.InternalAsString(TagSet: TCnBerTagSet): AnsiString;
var
  P: Pointer;
begin
  if (TagSet <> []) and not (FBerTag in TagSet) then
    raise ECnBerException.Create(SCnErrorBerTagTypeMismatchForString + IntToStr(FBerTag));

  Result := '';
  P := GetBerDataAddress;
  if (P <> nil) and (BerDataLength > 0) then
  begin
    SetLength(Result, BerDataLength);
    Move(P^, Result[1], BerDataLength);
  end;
end;

function TCnBerReadNode.IsNull: Boolean;
begin
  Result := FBerTag = CN_BER_TAG_NULL;
end;

function TCnBerReadNode.IsDateTime: Boolean;
begin
  Result := FBerTag in CN_TAG_SET_TIME;
end;

function TCnBerReadNode.IsString: Boolean;
begin
  Result := FBerTag in (CN_TAG_SET_STRING + CN_TAG_SET_TIME);
end;

function TCnBerReadNode.IsInteger: Boolean;
begin
  Result := FBerTag = CN_BER_TAG_INTEGER;
end;

procedure TCnBerReadNode.AsBigNumber(OutNum: TCnBigNumber);
begin
  if FBerTag <> CN_BER_TAG_INTEGER then
    raise ECnBerException.Create(SCnErrorBerTagTypeMismatchForBignumber);

  OutNum.SetBinary(GetBerDataAddress, FBerDataLength);
end;

function TCnBerReadNode.GetBerAddress: Pointer;
begin
  if FOriginData = nil then
    Result := nil
  else
    Result := Pointer(TCnIntAddress(FOriginData) + FBerOffset);
end;

function TCnBerReadNode.AsBoolean: Boolean;
var
  B: Byte;
begin
  if (FBerTag <> CN_BER_TAG_BOOLEAN) and (FBerDataLength <> 1) then
    raise ECnBerException.Create(SCnErrorBerTagTypeMismatchForBoolean + IntToStr(FBerTag));

  CopyDataTo(@B);
  Result := B <> 0;
end;

function TCnBerReadNode.AsRawString: string;
begin
  Result := string(InternalAsString([]));
end;

function TCnBerReadNode.AsAnsiString: AnsiString;
begin
  Result := InternalAsString([]);
end;

function TCnBerReadNode.AsCommonInteger: Integer;
var
  IntValue: Integer;
begin
  if FBerTag <> CN_BER_TAG_INTEGER then
    raise ECnBerException.Create(SCnErrorBerTagTypeMismatchForCommonInteger);

  if FBerDataLength > SizeOf(Cardinal) then
    raise ECnBerException.CreateFmt(SCnErrorDataLengthOverflowForCommonInteger,
      [FBerDataLength]);

  IntValue := 0;
  CopyDataTo(@IntValue);

  // Byte ���轻����SmallInt ������λ��Integer ������λ
  if FBerDataLength = SizeOf(Word) then
    IntValue := Integer(UInt16NetworkToHost(Word(IntValue)))
  else if FBerDataLength = SizeOf(Cardinal) then
    IntValue := UInt32NetworkToHost(IntValue);
  Result := IntValue;
end;

{ TCnBerWriter }

function TCnBerWriter.AddBasicNode(ATag: Integer; AData: PByte;
  ADataByteLen: Integer; Parent: TCnBerWriteNode): TCnBerWriteNode;
begin
  if Parent = nil then
    Parent := FBerTree.Root as TCnBerWriteNode;

  Result := FBerTree.AddChild(Parent) as TCnBerWriteNode;
  Result.FIsContainer := False;
  Result.FillBasicNode(ATag, AData, ADataByteLen);
end;

function TCnBerWriter.AddContainerNode(ATag: Integer;
  Parent: TCnBerWriteNode): TCnBerWriteNode;
begin
  if Parent = nil then
    Parent := FBerTree.Root as TCnBerWriteNode;

  Result := FBerTree.AddChild(Parent) as TCnBerWriteNode;
  Result.BerTag := ATag;
  Result.IsContainer := True;
end;

function TCnBerWriter.AddNullNode(Parent: TCnBerWriteNode): TCnBerWriteNode;
begin
  if Parent = nil then
    Parent := FBerTree.Root as TCnBerWriteNode;

  Result := FBerTree.AddChild(Parent) as TCnBerWriteNode;
  Result.IsContainer := False;
  Result.FillBasicNode(CN_BER_TAG_NULL, nil, 0); // TODO: Null ������
end;

constructor TCnBerWriter.Create;
begin
  inherited;
  FBerTree := TCnTree.Create(TCnBerWriteNode);
end;

destructor TCnBerWriter.Destroy;
begin
  FBerTree.Free;
  inherited;
end;

{$IFDEF DEBUG}
{$IFDEF ENABLE_UIINTERACT}
{$IFDEF MSWINDOWS}

procedure TCnBerWriter.DumpToTreeView(ATreeView: ComCtrls.TTreeView);
begin
  FBerTree.SaveToTreeView(ATreeView);
end;

function TCnBerWriter.GetOnSaveNode: TCnTreeNodeEvent;
begin
  Result := FBerTree.OnSaveANode;
end;

procedure TCnBerWriter.SetOnSaveNode(const Value: TCnTreeNodeEvent);
begin
  FBerTree.OnSaveANode := Value;
end;

{$ENDIF}

{$IFDEF SUPPORT_FMX}

procedure TCnBerWriter.DumpToTreeView(ATreeView: FMX.TreeView.TTreeView);
begin
  FBerTree.SaveToTreeView(ATreeView);
end;

function TCnBerWriter.GetOnSaveItem: TCnTreeViewItemEvent;
begin
  Result := FBerTree.OnSaveAItem;
end;

procedure TCnBerWriter.SetOnSaveItem(const Value: TCnTreeViewItemEvent);
begin
  FBerTree.OnSaveAItem := Value;
end;

{$ENDIF}
{$ENDIF}
{$ENDIF}

function TCnBerWriter.GetTotalSize: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FBerTree.Root.Count - 1 do
    Result := Result + TCnBerWriteNode(FBerTree.Root).Items[I].GetNodeLength;
end;

procedure TCnBerWriter.SaveTo(DestBuf: Pointer);
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    SaveToStream(Mem);
    Mem.Position := 0;
    Mem.Read(DestBuf^, Mem.Size);
  finally
    Mem.Free;
  end;
end;

procedure TCnBerWriter.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCnBerWriter.SaveToStream(Stream: TStream);
var
  I: Integer;
begin
  for I := 0 to FBerTree.Root.Count - 1 do
    TCnBerWriteNode(FBerTree.Root).Items[I].SaveToStream(Stream);
end;

function TCnBerWriter.AddRawNode(RawTag: Integer; RawLV: PByte;
  LVLen: Integer; Parent: TCnBerWriteNode): TCnBerWriteNode;
var
  B: Byte;
begin
  if Parent = nil then
    Parent := FBerTree.Root as TCnBerWriteNode;

  Result := FBerTree.AddChild(Parent) as TCnBerWriteNode;
  Result.BerTag := RawTag;

  B := RawTag;
  Result.FMem.Write(B, 1);
  if (RawLV <> nil) and (LVLen > 0) then
    Result.FMem.Write(RawLV^, LVLen);
end;

function TCnBerWriter.AddBasicNode(ATag: Integer; AStream: TStream;
  Parent: TCnBerWriteNode): TCnBerWriteNode;
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    Mem.LoadFromStream(AStream);
    Result := AddBasicNode(ATag, Mem.Memory, Mem.Size, Parent);
  finally
    Mem.Free;
  end;
end;

function TCnBerWriter.AddAnsiStringNode(ATag: Integer;
  const AStr: AnsiString; Parent: TCnBerWriteNode): TCnBerWriteNode;
begin
  if Parent = nil then
    Parent := FBerTree.Root as TCnBerWriteNode;
  
  Result := FBerTree.AddChild(Parent) as TCnBerWriteNode;
  Result.FIsContainer := False;
  Result.FillBasicNode(ATag, PByte(AStr), Length(AStr));
end;

{ TCnBerWriteNode }

procedure TCnBerWriteNode.FillHeadCalcLen(ATag: Integer; ADataLen: Integer);
var
  LenLen: Cardinal;
  B: Byte;
  W: Word;
  D: Cardinal;
begin
  FHeadLen := 0;
  if FIsContainer and (FBerTag in [CN_BER_TAG_SEQUENCE, CN_BER_TAG_SET]) then
    FHead[0] := ATag or CN_BER_TAG_STRUCT_MASK // ���ӽڵ�����ָ�����ͣ���λ�� 1
  else if FIsContainer and ((FBerTypeMask and CN_BER_TAG_TYPE_MASK) <> 0) then // ������ Mask ʱ�����������͵� Tag �� Container
    FHead[0] := ATag or CN_BER_TAG_STRUCT_MASK or (FBerTypeMask and CN_BER_TAG_TYPE_MASK)
  else
    FHead[0] := ATag;

  Inc(FHeadLen);
  if FBerTag = CN_BER_TAG_BIT_STRING then // BitString ����ȥҪ�Ӹ�ͷ�ֽ�
    Inc(ADataLen);

  if ADataLen <= 127 then // ���ֽڳ���
  begin
    FHead[1] := ADataLen;
    Inc(FHeadLen);
  end
  else
  begin
    // ���ڻ���� 128������ LeafLen ���ֽ���
    if ADataLen < $100 then
    begin
      LenLen := 1;
      B := ADataLen;
      Move(B, FHead[2], LenLen);
    end
    else if ADataLen < $10000 then
    begin
      LenLen := 2;
      W := ADataLen;
      W := UInt16HostToNetwork(W);
      Move(W, FHead[2], LenLen);
    end
    else if ADataLen < $1000000 then
    begin
      LenLen := 3;
      D := ADataLen;
      D := UInt32HostToNetwork(D);
      D := D shr 8;
      Move(D, FHead[2], LenLen);
    end
    else
    begin
      LenLen := 4;
      D := ADataLen;
      D := UInt32HostToNetwork(D);
      Move(D, FHead[2], LenLen);
    end;

    FHead[1] := CN_BER_LENLEN_MASK or LenLen;
    Inc(FHeadLen, 1 + LenLen);
  end;
end;

constructor TCnBerWriteNode.Create(ATree: TCnTree);
begin
  inherited;
  FMem := TMemoryStream.Create;
end;

destructor TCnBerWriteNode.Destroy;
begin
  FMem.Free;
  inherited;
end;

function TCnBerWriteNode.GetIsContainer: Boolean;
begin
  Result := FIsContainer;
end;

function TCnBerWriteNode.GetItems(AIndex: Integer): TCnBerWriteNode;
begin
  Result := TCnBerWriteNode(inherited GetItems(AIndex));
end;

function TCnBerWriteNode.SaveToStream(Stream: TStream): Integer;
var
  B: Byte;
  I: Integer;
  LeafLen: Cardinal;
  AMem: TMemoryStream;
begin
  if FIsContainer then
  begin
    LeafLen := 0;
    Result := 0;
    AMem := TMemoryStream.Create;
    try
      for I := 0 to Count - 1 do
        LeafLen := LeafLen + Cardinal(Items[I].SaveToStream(AMem));

      FillHeadCalcLen(FBerTag, LeafLen);
      // �� Tag��LeafLen �Լ� AMem ��������Ϻ�д��

      Result := Result + Stream.Write(FHead[0], FHeadLen); // дͷ�볤��
      if FBerTag = CN_BER_TAG_BIT_STRING then              // BitString ��һ�� bit ������ʱ��Ϊȫ 0 ����
      begin
        B := 0;
        Result := Result + Stream.Write(B, 1);
      end;

      // д��������
      Result := Result + Stream.Write(AMem.Memory^, AMem.Size);
    finally
      AMem.Free;
    end;
  end
  else
  begin
    Result := Stream.Write(FMem.Memory^, FMem.Size);
  end;
end;

procedure TCnBerWriteNode.SetIsContainer(const Value: Boolean);
begin
  FIsContainer := Value;
end;

procedure TCnBerWriteNode.SetItems(AIndex: Integer;
  const Value: TCnBerWriteNode);
begin
  inherited SetItems(AIndex, Value);
end;

function TCnBerWriteNode.GetNodeLength: Integer;
var
  I, LeafLen: Integer;
begin
  if FIsContainer then
  begin
    LeafLen := 0;
    for I := 0 to Count - 1 do
      LeafLen := LeafLen + Items[I].GetNodeLength;

    FillHeadCalcLen(FBerTag, LeafLen);
    Result := FHeadLen + LeafLen;

    // BitString ��Ҫһ��ǰ���ֽڱ�ʾ����� bit
    if FBerTag = CN_BER_TAG_BIT_STRING then
      Inc(Result);
  end
  else
  begin
    Result := FMem.Size;
  end;
end;

procedure TCnBerWriteNode.FillBasicNode(ATag: Integer; AData: PByte;
  ADataByteLen: Integer);
var
  B: Byte;
begin
  FBerTag := ATag;
  if FIsContainer then
    Exit;

  FData := AData;
  FDataLength := ADataByteLen;
  FillHeadCalcLen(ATag, ADataByteLen);

  FMem.Clear;
  FMem.Write(FHead[0], FHeadLen);
  if ADataByteLen > 0 then
  begin
    // �� BitString ��Ҫ��һ��ǰ���ֽڣ�ͷ�����Ѿ��� FillHeadCalcLen �ڲ�����
    if ATag = CN_BER_TAG_BIT_STRING then
    begin
      B := 0;
      FMem.Write(B, 1);
    end;
    FMem.Write(Data^, ADataByteLen);
  end;
end;

function TCnBerWriteNode.SaveValueToStream(Stream: TStream): Integer;
var
  B: Byte;
  I: Integer;
  AMem: TMemoryStream;
begin
  Result := 0;
  if FIsContainer then
  begin
    AMem := TMemoryStream.Create;
    try
      for I := 0 to Count - 1 do
        Items[I].SaveToStream(AMem);

      if FBerTag = CN_BER_TAG_BIT_STRING then // BitString ��һ�� bit ������ʱ��Ϊȫ 0 ����
      begin
        B := 0;
        Result := Result + Stream.Write(B, 1);
      end;

      // д��������
      Result := Result + Stream.Write(AMem.Memory^, AMem.Size);
    finally
      AMem.Free;
    end;
  end
  else
  begin
    if (FHeadLen > 0) and (FMem.Size > FHeadLen) then
      Result := Stream.Write(Pointer(TCnIntAddress(FMem.Memory) + FHeadLen)^, FMem.Size - FHeadLen);
  end;
end;

end.
